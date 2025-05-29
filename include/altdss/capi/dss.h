/*! \file dss.h */
#ifndef ALTDSS_CAPI_DLL_H
#define ALTDSS_CAPI_DLL_H
#define ALTDSS_CAPI_VERSION "0.15.0-dev"
#ifndef ALTDSS_CAPI_DLL
//#define ALTDSS_CAPI_DLL __declspec(dllimport)
#define ALTDSS_CAPI_DLL
#endif

#include "./common.h"

#ifdef __cplusplus
#ifdef ALTDSS_CAPI_NAMESPACE
namespace altdss { namespace capi {
#endif
extern "C" {
#else
#endif
    /* Functions start here */

    ALTDSS_CAPI_DLL void DSS_ResetStringBuffer(void);

    /*! 
    Dispose temporary buffer data in the global result (GR) pointers
    */
    ALTDSS_CAPI_DLL void DSS_DisposeGRData(void);

    /*! 
    Get references to the global result (GR) pointers, used in
    the *_GR variations of most getter functions

    The returned values in the DataPtrs will contain pointers to the global variables that contains the actual pointers.
    The CountPtrs are not reallocated during the execution, so the returned values contain the actual pointer values.
    */
    ALTDSS_CAPI_DLL void DSS_GetGRPointers(
        double*** DataPtr_PDouble,
        int32_t*** DataPtr_PInteger,
        int8_t*** DataPtr_PByte,
        int32_t** CountPtr_PDouble,
        int32_t** CountPtr_PInteger,
        int32_t** CountPtr_PByte
    );

    /*!  Functions to get the current GR pointers individually, used in MATLAB */
    ALTDSS_CAPI_DLL double* DSS_GR_DataPtr_PDouble(void);
    ALTDSS_CAPI_DLL int32_t* DSS_GR_DataPtr_PInteger(void);
    ALTDSS_CAPI_DLL int8_t* DSS_GR_DataPtr_PByte(void);
    ALTDSS_CAPI_DLL int32_t* DSS_GR_CountPtr_PDouble(void);
    ALTDSS_CAPI_DLL int32_t* DSS_GR_CountPtr_PInteger(void);
    ALTDSS_CAPI_DLL int32_t* DSS_GR_CountPtr_PByte(void);

    ALTDSS_CAPI_DLL void* DSS_GetMem(uint64_t NumBytes);
    ALTDSS_CAPI_DLL void DSS_FreeMem(void* ptr);
    
    ALTDSS_CAPI_DLL void DSS_RegisterPlotCallback(altdss_callback_plot_t cb);
    ALTDSS_CAPI_DLL void DSS_RegisterMessageCallback(altdss_callback_message_t cb);

    /*!
    API Extension: connect callbacks to both legacy (same style of COM DSSEvents) 
    and more general events of the engine operation.

    See the enumeration AltDSSEvent for values to "evt".
    */
    ALTDSS_CAPI_DLL uint16_t DSSEvents_RegisterAlt(int32_t evt, altdss_callback_event_t cb);
    ALTDSS_CAPI_DLL uint16_t DSSEvents_UnregisterAlt(int32_t evt, altdss_callback_event_t cb);

    /*!
    Make a new circuit and activates it.
    */
    ALTDSS_CAPI_DLL void DSS_NewCircuit(const char* Value);

    /*! 
    Array of strings consisting of all element names in the active class.
    */
    ALTDSS_CAPI_DLL void ActiveClass_Get_AllNames(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Sets first element in the active class to be the active DSS object. 
    If the object is a CktElement, ActiveCktELement also points to this element. 

    Returns 0 if none.
    */
    ALTDSS_CAPI_DLL int32_t ActiveClass_Get_First(void);

    /*! 
    Sets next element in active class to be the active DSS object. 
    If the object is a CktElement, ActiveCktElement also points to this element.

    Returns 0 if no more.
    */
    ALTDSS_CAPI_DLL int32_t ActiveClass_Get_Next(void);

    /*! 
    Name of the Active Element of the Active Class
    */
    ALTDSS_CAPI_DLL const char* ActiveClass_Get_Name(void);

    ALTDSS_CAPI_DLL void ActiveClass_Set_Name(const char* Value);

    /*! 
    Number of elements in this class. Same as Count property.
    */
    ALTDSS_CAPI_DLL int32_t ActiveClass_Get_NumElements(void);

    /*! 
    Returns name of active class.
    */
    ALTDSS_CAPI_DLL const char* ActiveClass_Get_ActiveClassName(void);

    /*! 
    Number of elements in Active Class. Same as NumElements Property.
    */
    ALTDSS_CAPI_DLL int32_t ActiveClass_Get_Count(void);

    /*! 
    Use this property (Read only) for getting the name of the parent class' name of the active class
    */
    ALTDSS_CAPI_DLL const char* ActiveClass_Get_ActiveClassParent(void);

    /*! 
    Returns the data (as a list) of all elements from the active class as a JSON-encoded string.

    The `options` parameter contains bit-flags to toggle specific features.
    See `Obj_ToJSON` for more. 
    
    Additionally, the `ExcludeDisabled` flag can be used to excluded disabled elements from the output.

    (API Extension)
    */
    ALTDSS_CAPI_DLL const char* ActiveClass_ToJSON(int32_t options);

    /*! 
    Returns the current object index in ActiveClass (1-based)

    (API Extension)
    */
    ALTDSS_CAPI_DLL int32_t ActiveClass_Get_idx(void);

    /*! 
    Activate an object in the ActiveClass by index (1-based)

    (API Extension)
    */
    ALTDSS_CAPI_DLL void ActiveClass_Set_idx(int32_t Value);

    /*! 
    Name of the active Bus
    */
    ALTDSS_CAPI_DLL const char* Bus_Get_Name(void);

    /*! 
    Number of Nodes this bus.
    */
    ALTDSS_CAPI_DLL int32_t Bus_Get_NumNodes(void);

    /*! 
    Double Array of sequence voltages at this bus. Magnitudes only.
    */
    ALTDSS_CAPI_DLL void Bus_Get_SeqVoltages(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Bus_Get_SeqVoltages but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Bus_Get_SeqVoltages_GR(void);

    /*! 
    Complex array of voltages at this bus.
    */
    ALTDSS_CAPI_DLL void Bus_Get_Voltages(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Bus_Get_Voltages but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Bus_Get_Voltages_GR(void);

    /*! 
    Integer Array of Node Numbers defined at the bus in same order as the voltages.
    */
    ALTDSS_CAPI_DLL void Bus_Get_Nodes(int32_t** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Bus_Get_Nodes but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Bus_Get_Nodes_GR(void);

    /*! 
    Short circuit currents at bus; Complex Array.
    */
    ALTDSS_CAPI_DLL void Bus_Get_Isc(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Bus_Get_Isc but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Bus_Get_Isc_GR(void);

    /*! 
    Open circuit voltage; Complex array.
    */
    ALTDSS_CAPI_DLL void Bus_Get_Voc(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Bus_Get_Voc but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Bus_Get_Voc_GR(void);

    /*! 
    Base voltage at bus in kV
    */
    ALTDSS_CAPI_DLL double Bus_Get_kVBase(void);

    /*! 
    Complex Array of pu voltages at the bus.
    */
    ALTDSS_CAPI_DLL void Bus_Get_puVoltages(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Bus_Get_puVoltages but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Bus_Get_puVoltages_GR(void);

    /*! 
    Complex Zero-Sequence short circuit impedance at bus.
    */
    ALTDSS_CAPI_DLL void Bus_Get_Zsc0(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Bus_Get_Zsc0 but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Bus_Get_Zsc0_GR(void);

    /*! 
    Complex Positive-Sequence short circuit impedance at bus.
    */
    ALTDSS_CAPI_DLL void Bus_Get_Zsc1(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Bus_Get_Zsc1 but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Bus_Get_Zsc1_GR(void);

    /*! 
    Complex array of Zsc matrix at bus. Column by column.
    */
    ALTDSS_CAPI_DLL void Bus_Get_ZscMatrix(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Bus_Get_ZscMatrix but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Bus_Get_ZscMatrix_GR(void);

    ALTDSS_CAPI_DLL uint16_t Bus_ZscRefresh(void);

    /*! 
    Complex array of Ysc matrix at bus. Column by column.
    */
    ALTDSS_CAPI_DLL void Bus_Get_YscMatrix(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Bus_Get_YscMatrix but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Bus_Get_YscMatrix_GR(void);

    /*! 
    Indicates whether a coordinate has been defined for this bus
    */
    ALTDSS_CAPI_DLL uint16_t Bus_Get_Coorddefined(void);

    /*! 
    X Coordinate for bus (double)
    */
    ALTDSS_CAPI_DLL double Bus_Get_x(void);

    /*! 
    X Coordinate for bus (double)
    */
    ALTDSS_CAPI_DLL void Bus_Set_x(double Value);

    /*! 
    Y coordinate for bus(double)
    */
    ALTDSS_CAPI_DLL double Bus_Get_y(void);

    /*! 
    Y coordinate for bus(double)
    */
    ALTDSS_CAPI_DLL void Bus_Set_y(double Value);

    /*! 
    Longitude for bus (double)

    On the AltDSS engine, this is currently a synonym to the X coordinate.
    */
    ALTDSS_CAPI_DLL void Bus_Set_Longitude(double Value);

    /*! 
    Latitude for bus (double)

    On the AltDSS engine, this is currently a synonym to the Y coordinate.
    */
    ALTDSS_CAPI_DLL void Bus_Set_Latitude(double Value);

    /*! 
    Longitude for bus (double)

    On the AltDSS engine, this is currently a synonym to the X coordinate.
    */
    ALTDSS_CAPI_DLL double Bus_Get_Longitude(void);

    /*! 
    Latitude for bus (double)

    On the AltDSS engine, this is currently a synonym to the Y coordinate.
    */
    ALTDSS_CAPI_DLL double Bus_Get_Latitude(void);

    /*! 
    Distance from EnergyMeter (if non-zero)
    */
    ALTDSS_CAPI_DLL double Bus_Get_Distance(void);

    ALTDSS_CAPI_DLL int32_t Bus_GetUniqueNodeNumber(int32_t StartNumber);

    /*! 
    Complex Double array of Sequence Voltages (0, 1, 2) at this Bus.
    */
    ALTDSS_CAPI_DLL void Bus_Get_CplxSeqVoltages(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Bus_Get_CplxSeqVoltages but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Bus_Get_CplxSeqVoltages_GR(void);

    /*! 
    Average interruption duration, hr.
    */
    ALTDSS_CAPI_DLL double Bus_Get_Int_Duration(void);

    /*! 
    Accumulated failure rate downstream from this bus; faults per year
    */
    ALTDSS_CAPI_DLL double Bus_Get_Lambda(void);

    /*! 
    Accumulated customer outage durations
    */
    ALTDSS_CAPI_DLL double Bus_Get_Cust_Duration(void);

    /*! 
    Annual number of customer-interruptions from this bus
    */
    ALTDSS_CAPI_DLL double Bus_Get_Cust_Interrupts(void);

    /*! 
    Total numbers of customers served downline from this bus
    */
    ALTDSS_CAPI_DLL int32_t Bus_Get_N_Customers(void);

    /*! 
    Number of interruptions this bus per year
    */
    ALTDSS_CAPI_DLL double Bus_Get_N_interrupts(void);

    /*! 
    Returns Complex array of pu L-L voltages for 2- and 3-phase buses. Returns -1.0 for 1-phase bus. If more than 3 phases, returns only 3 phases.
    */
    ALTDSS_CAPI_DLL void Bus_Get_puVLL(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Bus_Get_puVLL but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Bus_Get_puVLL_GR(void);

    /*! 
    For 2- and 3-phase buses, returns array of complex numbers representing L-L voltages in volts. Returns -1.0 for 1-phase bus. If more than 3 phases, returns only first 3.
    */
    ALTDSS_CAPI_DLL void Bus_Get_VLL(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Bus_Get_VLL but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Bus_Get_VLL_GR(void);

    /*! 
    Array of doubles containing voltage magnitude, angle (degrees) pairs in per unit
    */
    ALTDSS_CAPI_DLL void Bus_Get_puVmagAngle(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Bus_Get_puVmagAngle but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Bus_Get_puVmagAngle_GR(void);

    /*! 
    Array of doubles containing voltages in Magnitude (VLN), angle (degrees)
    */
    ALTDSS_CAPI_DLL void Bus_Get_VMagAngle(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Bus_Get_VMagAngle but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Bus_Get_VMagAngle_GR(void);

    /*! 
    Total length of line downline from this bus, in miles. For recloser siting algorithm.
    */
    ALTDSS_CAPI_DLL double Bus_Get_TotalMiles(void);

    /*! 
    Integer ID of the feeder section in which this bus is located.
    */
    ALTDSS_CAPI_DLL int32_t Bus_Get_SectionID(void);
    
    /*! 
    Array of strings: Full Names of LINE elements connected to the active bus.
    */
    ALTDSS_CAPI_DLL void Bus_Get_LineList(char*** ResultPtr, int32_t* ResultDims);
    
    /*! 
    Array of strings: Full Names of LOAD elements connected to the active bus.
    */
    ALTDSS_CAPI_DLL void Bus_Get_LoadList(char*** ResultPtr, int32_t* ResultDims);
    
    /*! 
    Array of doubles (complex) containing the complete 012 Zsc matrix. 
    Only available after Zsc is computed, either through the "ZscRefresh" command, or running a "FaultStudy" solution.
    Only available for buses with 3 nodes.
    */
    ALTDSS_CAPI_DLL void Bus_Get_ZSC012Matrix(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Bus_Get_ZSC012Matrix but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Bus_Get_ZSC012Matrix_GR(void);

    /*! 
    Returns an array with the names of all PCE connected to the active bus    
    */
    ALTDSS_CAPI_DLL void Bus_Get_AllPCEatBus(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Returns an array with the names of all PDE connected to the active bus
    */
    ALTDSS_CAPI_DLL void Bus_Get_AllPDEatBus(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Array of strings with all Capacitor names in the circuit.
    */
    ALTDSS_CAPI_DLL void Capacitors_Get_AllNames(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Sets the first Capacitor active. Returns 0 if no more.
    */
    ALTDSS_CAPI_DLL int32_t Capacitors_Get_First(void);

    /*! 
    Delta connection or wye?
    */
    ALTDSS_CAPI_DLL uint16_t Capacitors_Get_IsDelta(void);

    /*! 
    Bank kV rating. Use LL for 2 or 3 phases, or actual can rating for 1 phase.
    */
    ALTDSS_CAPI_DLL double Capacitors_Get_kV(void);

    /*! 
    Total bank KVAR, distributed equally among phases and steps.
    */
    ALTDSS_CAPI_DLL double Capacitors_Get_kvar(void);

    /*! 
    Sets the active Capacitor by Name.
    */
    ALTDSS_CAPI_DLL const char* Capacitors_Get_Name(void);

    /*! 
    Sets the next Capacitor active. Returns 0 if no more.
    */
    ALTDSS_CAPI_DLL int32_t Capacitors_Get_Next(void);

    /*! 
    Number of steps (default 1) for distributing and switching the total bank kVAR.
    */
    ALTDSS_CAPI_DLL int32_t Capacitors_Get_NumSteps(void);

    /*! 
    Delta connection or wye?
    */
    ALTDSS_CAPI_DLL void Capacitors_Set_IsDelta(uint16_t Value);

    /*! 
    Bank kV rating. Use LL for 2 or 3 phases, or actual can rating for 1 phase.
    */
    ALTDSS_CAPI_DLL void Capacitors_Set_kV(double Value);

    /*! 
    Total bank KVAR, distributed equally among phases and steps.
    */
    ALTDSS_CAPI_DLL void Capacitors_Set_kvar(double Value);

    /*! 
    Sets the active Capacitor by Name.
    */
    ALTDSS_CAPI_DLL void Capacitors_Set_Name(const char* Value);

    /*! 
    Number of steps (default 1) for distributing and switching the total bank kVAR.
    */
    ALTDSS_CAPI_DLL void Capacitors_Set_NumSteps(int32_t Value);

    /*! 
    Number of Capacitor objects in active circuit.
    */
    ALTDSS_CAPI_DLL int32_t Capacitors_Get_Count(void);

    ALTDSS_CAPI_DLL uint16_t Capacitors_AddStep(void);

    ALTDSS_CAPI_DLL uint16_t Capacitors_SubtractStep(void);

    /*! 
    Number of Steps available in cap bank to be switched ON.
    */
    ALTDSS_CAPI_DLL int32_t Capacitors_Get_AvailableSteps(void);

    /*! 
    An array of integers [0..NumSteps-1] indicating state of each step. If the read value is -1 an error has occurred.
    */
    ALTDSS_CAPI_DLL void Capacitors_Get_States(int32_t** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Capacitors_Get_States but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Capacitors_Get_States_GR(void);

    /*! 
    Array of integer [0..numSteps-1] indicating the state of each step
    */
    ALTDSS_CAPI_DLL void Capacitors_Set_States(const int32_t* ValuePtr, int32_t ValueCount);

    ALTDSS_CAPI_DLL void Capacitors_Open(void);

    ALTDSS_CAPI_DLL void Capacitors_Close(void);

    /*! 
    Array of strings with all CapControl names.
    */
    ALTDSS_CAPI_DLL void CapControls_Get_AllNames(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Name of the Capacitor that is controlled.
    */
    ALTDSS_CAPI_DLL const char* CapControls_Get_Capacitor(void);

    /*! 
    Transducer ratio from primary current to control current.
    */
    ALTDSS_CAPI_DLL double CapControls_Get_CTratio(void);

    ALTDSS_CAPI_DLL double CapControls_Get_DeadTime(void);

    /*! 
    Time delay [s] to switch on after arming.  Control may reset before actually switching.
    */
    ALTDSS_CAPI_DLL double CapControls_Get_Delay(void);

    /*! 
    Time delay [s] before switching off a step. Control may reset before actually switching.
    */
    ALTDSS_CAPI_DLL double CapControls_Get_DelayOff(void);

    /*! 
    Sets the first CapControl as active. Return 0 if none.
    */
    ALTDSS_CAPI_DLL int32_t CapControls_Get_First(void);

    /*! 
    Type of automatic controller.
    */
    ALTDSS_CAPI_DLL int32_t CapControls_Get_Mode(void);

    /*! 
    Full name of the element that PT and CT are connected to.
    */
    ALTDSS_CAPI_DLL const char* CapControls_Get_MonitoredObj(void);

    /*! 
    Terminal number on the element that PT and CT are connected to.
    */
    ALTDSS_CAPI_DLL int32_t CapControls_Get_MonitoredTerm(void);

    /*! 
    Sets a CapControl active by name.
    */
    ALTDSS_CAPI_DLL const char* CapControls_Get_Name(void);

    /*! 
    Gets the next CapControl in the circuit. Returns 0 if none.
    */
    ALTDSS_CAPI_DLL int32_t CapControls_Get_Next(void);

    /*! 
    Threshold to switch off a step. See Mode for units.
    */
    ALTDSS_CAPI_DLL double CapControls_Get_OFFSetting(void);

    /*! 
    Threshold to arm or switch on a step.  See Mode for units.
    */
    ALTDSS_CAPI_DLL double CapControls_Get_ONSetting(void);

    /*! 
    Transducer ratio from primary feeder to control voltage.
    */
    ALTDSS_CAPI_DLL double CapControls_Get_PTratio(void);

    /*! 
    Enables Vmin and Vmax to override the control Mode
    */
    ALTDSS_CAPI_DLL uint16_t CapControls_Get_UseVoltOverride(void);

    /*! 
    With VoltOverride, switch off whenever PT voltage exceeds this level.
    */
    ALTDSS_CAPI_DLL double CapControls_Get_Vmax(void);

    /*! 
    With VoltOverride, switch ON whenever PT voltage drops below this level.
    */
    ALTDSS_CAPI_DLL double CapControls_Get_Vmin(void);

    /*! 
    Name of the Capacitor that is controlled.
    */
    ALTDSS_CAPI_DLL void CapControls_Set_Capacitor(const char* Value);

    /*! 
    Transducer ratio from primary current to control current.
    */
    ALTDSS_CAPI_DLL void CapControls_Set_CTratio(double Value);

    ALTDSS_CAPI_DLL void CapControls_Set_DeadTime(double Value);

    /*! 
    Time delay [s] to switch on after arming.  Control may reset before actually switching.
    */
    ALTDSS_CAPI_DLL void CapControls_Set_Delay(double Value);

    /*! 
    Time delay [s] before switching off a step. Control may reset before actually switching.
    */
    ALTDSS_CAPI_DLL void CapControls_Set_DelayOff(double Value);

    /*! 
    Type of automatic controller.
    */
    ALTDSS_CAPI_DLL void CapControls_Set_Mode(int32_t Value);

    /*! 
    Full name of the element that PT and CT are connected to.
    */
    ALTDSS_CAPI_DLL void CapControls_Set_MonitoredObj(const char* Value);

    /*! 
    Terminal number on the element that PT and CT are connected to.
    */
    ALTDSS_CAPI_DLL void CapControls_Set_MonitoredTerm(int32_t Value);

    /*! 
    Sets a CapControl active by name.
    */
    ALTDSS_CAPI_DLL void CapControls_Set_Name(const char* Value);

    /*! 
    Threshold to switch off a step. See Mode for units.
    */
    ALTDSS_CAPI_DLL void CapControls_Set_OFFSetting(double Value);

    /*! 
    Threshold to arm or switch on a step.  See Mode for units.
    */
    ALTDSS_CAPI_DLL void CapControls_Set_ONSetting(double Value);

    /*! 
    Transducer ratio from primary feeder to control voltage.
    */
    ALTDSS_CAPI_DLL void CapControls_Set_PTratio(double Value);

    /*! 
    Enables Vmin and Vmax to override the control Mode
    */
    ALTDSS_CAPI_DLL void CapControls_Set_UseVoltOverride(uint16_t Value);

    /*! 
    With VoltOverride, switch off whenever PT voltage exceeds this level.
    */
    ALTDSS_CAPI_DLL void CapControls_Set_Vmax(double Value);

    /*! 
    With VoltOverride, switch ON whenever PT voltage drops below this level.
    */
    ALTDSS_CAPI_DLL void CapControls_Set_Vmin(double Value);

    /*! 
    Number of CapControls in Active Circuit
    */
    ALTDSS_CAPI_DLL int32_t CapControls_Get_Count(void);

    ALTDSS_CAPI_DLL void CapControls_Reset(void);

    /*! 
    Name of the active circuit.
    */
    ALTDSS_CAPI_DLL const char* Circuit_Get_Name(void);

    /*! 
    Total number of Buses in the circuit.
    */
    ALTDSS_CAPI_DLL int32_t Circuit_Get_NumBuses(void);

    /*! 
    Number of CktElements in the circuit.
    */
    ALTDSS_CAPI_DLL int32_t Circuit_Get_NumCktElements(void);

    /*! 
    Total number of nodes in the circuit.
    */
    ALTDSS_CAPI_DLL int32_t Circuit_Get_NumNodes(void);

    /*! 
    Complex total line losses in the circuit
    */
    ALTDSS_CAPI_DLL void Circuit_Get_LineLosses(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Circuit_Get_LineLosses but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Circuit_Get_LineLosses_GR(void);

    /*! 
    Total losses in active circuit, complex number (two-element array of double).
    */
    ALTDSS_CAPI_DLL void Circuit_Get_Losses(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Circuit_Get_Losses but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Circuit_Get_Losses_GR(void);

    /*! 
    Array of magnitudes (doubles) of voltages at all buses
    */
    ALTDSS_CAPI_DLL void Circuit_Get_AllBusVmag(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Circuit_Get_AllBusVmag but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Circuit_Get_AllBusVmag_GR(void);

    /*! 
    Complex array of all bus, node voltages from most recent solution
    */
    ALTDSS_CAPI_DLL void Circuit_Get_AllBusVolts(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Circuit_Get_AllBusVolts but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Circuit_Get_AllBusVolts_GR(void);

    /*! 
    Array of strings containing Full Name of all elements.
    */
    ALTDSS_CAPI_DLL void Circuit_Get_AllElementNames(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Complex losses in all transformers designated to substations.
    */
    ALTDSS_CAPI_DLL void Circuit_Get_SubstationLosses(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Circuit_Get_SubstationLosses but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Circuit_Get_SubstationLosses_GR(void);

    /*! 
    Total power (complex), kVA delivered to the circuit
    */
    ALTDSS_CAPI_DLL void Circuit_Get_TotalPower(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Circuit_Get_TotalPower but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Circuit_Get_TotalPower_GR(void);

    ALTDSS_CAPI_DLL void Circuit_Disable(const char* Name);

    ALTDSS_CAPI_DLL void Circuit_Enable(const char* Name);

    ALTDSS_CAPI_DLL int32_t Circuit_FirstPCElement(void);

    ALTDSS_CAPI_DLL int32_t Circuit_FirstPDElement(void);

    ALTDSS_CAPI_DLL int32_t Circuit_NextPCElement(void);

    ALTDSS_CAPI_DLL int32_t Circuit_NextPDElement(void);

    /*! 
    Array of strings containing names of all buses in circuit (see AllNodeNames).
    */
    ALTDSS_CAPI_DLL void Circuit_Get_AllBusNames(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Array of total losses (complex) in each circuit element
    */
    ALTDSS_CAPI_DLL void Circuit_Get_AllElementLosses(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Circuit_Get_AllElementLosses but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Circuit_Get_AllElementLosses_GR(void);

    ALTDSS_CAPI_DLL void Circuit_Sample(void);

    ALTDSS_CAPI_DLL void Circuit_SaveSample(void);

    ALTDSS_CAPI_DLL int32_t Circuit_SetActiveElement(const char* FullName);

    ALTDSS_CAPI_DLL double Circuit_Capacity(double Start, double Increment);

    /*! 
    Double Array of all bus voltages (each node) magnitudes in Per unit
    */
    ALTDSS_CAPI_DLL void Circuit_Get_AllBusVmagPu(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Circuit_Get_AllBusVmagPu but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Circuit_Get_AllBusVmagPu_GR(void);

    /*!
    Sets Active bus by name. 
      
    Ignores node list. Returns bus index (zero based) compatible with `AllBusNames` and Buses collection.
    */
    ALTDSS_CAPI_DLL int32_t Circuit_SetActiveBus(const char* BusName);

    /*!
    Set ActiveBus by an integer value. 
    
    0-based index compatible with SetActiveBus return value and AllBusNames indexing. 
    Returns 0 if OK.
    */
    ALTDSS_CAPI_DLL int32_t Circuit_SetActiveBusi(int32_t BusIndex);

    /*! 
    Array of strings containing full name of each node in system in same order as returned by AllBusVolts, etc.
    */
    ALTDSS_CAPI_DLL void Circuit_Get_AllNodeNames(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    System Y matrix (after a solution has been performed). 
    This is deprecated as it returns a dense matrix. Only use it for small systems.
    For large-scale systems, prefer YMatrix_GetCompressedYMatrix.
    */
    ALTDSS_CAPI_DLL void Circuit_Get_SystemY(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Circuit_Get_SystemY but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Circuit_Get_SystemY_GR(void);

    /*! 
    Returns distance from each bus to parent EnergyMeter. Corresponds to sequence in AllBusNames.
    */
    ALTDSS_CAPI_DLL void Circuit_Get_AllBusDistances(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Circuit_Get_AllBusDistances but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Circuit_Get_AllBusDistances_GR(void);

    /*! 
    Returns an array of distances from parent EnergyMeter for each Node. Corresponds to AllBusVMag sequence.
    */
    ALTDSS_CAPI_DLL void Circuit_Get_AllNodeDistances(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Circuit_Get_AllNodeDistances but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Circuit_Get_AllNodeDistances_GR(void);

    /*! 
    Returns an array of doubles representing the distances to parent EnergyMeter. Sequence of array corresponds to other node ByPhase properties.
    */
    ALTDSS_CAPI_DLL void Circuit_Get_AllNodeDistancesByPhase(double** ResultPtr, int32_t* ResultDims, int32_t Phase);
    /*! 
    Same as Circuit_Get_AllNodeDistancesByPhase but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Circuit_Get_AllNodeDistancesByPhase_GR(int32_t Phase);

    /*! 
    Returns Array of doubles represent voltage magnitudes for nodes on the specified phase.
    */
    ALTDSS_CAPI_DLL void Circuit_Get_AllNodeVmagByPhase(double** ResultPtr, int32_t* ResultDims, int32_t Phase);
    /*! 
    Same as Circuit_Get_AllNodeVmagByPhase but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Circuit_Get_AllNodeVmagByPhase_GR(int32_t Phase);

    /*! 
    Returns array of per unit voltage magnitudes for each node by phase
    */
    ALTDSS_CAPI_DLL void Circuit_Get_AllNodeVmagPUByPhase(double** ResultPtr, int32_t* ResultDims, int32_t Phase);
    /*! 
    Same as Circuit_Get_AllNodeVmagPUByPhase but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Circuit_Get_AllNodeVmagPUByPhase_GR(int32_t Phase);

    /*! 
    Return array of strings of the node names for the By Phase criteria. Sequence corresponds to other ByPhase properties.
    */
    ALTDSS_CAPI_DLL void Circuit_Get_AllNodeNamesByPhase(char*** ResultPtr, int32_t* ResultDims, int32_t Phase);

    ALTDSS_CAPI_DLL int32_t Circuit_SetActiveClass(const char* ClassName);

    ALTDSS_CAPI_DLL int32_t Circuit_FirstElement(void);

    ALTDSS_CAPI_DLL int32_t Circuit_NextElement(void);

    ALTDSS_CAPI_DLL void Circuit_UpdateStorage(void);

    /*! 
    Sets Parent PD element, if any, to be the active circuit element and returns index>0; Returns 0 if it fails or not applicable.
    */
    ALTDSS_CAPI_DLL int32_t Circuit_Get_ParentPDElement(void);

    ALTDSS_CAPI_DLL void Circuit_EndOfTimeStepUpdate(void);

    /*! 
    Array of strings containing the names of the nodes in the same order as the Y matrix
    */
    ALTDSS_CAPI_DLL void Circuit_Get_YNodeOrder(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Array of doubles containing complex injection currents for the present solution. Is is the "I" vector of I=YV
    */
    ALTDSS_CAPI_DLL void Circuit_Get_YCurrents(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Circuit_Get_YCurrents but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Circuit_Get_YCurrents_GR(void);

    /*! 
    Complex array of actual node voltages in same order as SystemY matrix.
    */
    ALTDSS_CAPI_DLL void Circuit_Get_YNodeVarray(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Circuit_Get_YNodeVarray but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Circuit_Get_YNodeVarray_GR(void);
    ALTDSS_CAPI_DLL void Circuit_SetCktElementName(const char* Value);
    ALTDSS_CAPI_DLL void Circuit_SetCktElementIndex(int32_t Value);


    /*
    EXPERIMENTAL: Returns the general circuit data, including all DSS objects, as a
    JSON-encoded string. The data is encoded using the proposed AltDSS Schema, see
    https://github.com/orgs/dss-extensions/discussions/ for links to docs and to
    provide feedback for future revisions.

    (API Extension)
    */
    ALTDSS_CAPI_DLL const char* Circuit_ToJSON(int32_t options);

    /*
    Equivalent of the "save circuit" DSS command, but allows customization
    through the `saveFlags` argument, which is a set of bit flags. 
    See the "DSSSaveFlags" enumeration for available flags:

    - `CalcVoltageBases`: Include the command CalcVoltageBases.
    - `SetVoltageBases`: Include commands to set the voltage bases individually.
    - `IncludeOptions`: Include most of the options (from the Set/Get DSS commands).
    - `IncludeDisabled`: Include disabled circuit elements (and LoadShapes).
    - `ExcludeDefault`: Exclude default DSS items if they are not modified by the user.
    - `SingleFile`: Use a single file instead of a folder for output.
    - `KeepOrder`: Save the circuit elements in the order they were loaded in the active circuit. Guarantees better reproducibility, especially when the system is ill-conditioned. Requires "SingleFile" flag.
    - `ExcludeMeterZones`: Do not export meter zones (as "feeders") separately. Has no effect when using a single file.
    - `IsOpen`: Export commands to open terminals of elements.
    - `ToString`: to the result string. Requires "SingleFile" flag.

    If `SingleFile` is enabled, the first argument (`dirOrFilePath`) is the file path,
    otherwise it is the folder path. For string output, the argument is not used.

    (API Extension)
    */
    ALTDSS_CAPI_DLL const char* Circuit_Save(const char* dirOrFilePath, uint32_t saveFlags);

    /*
    EXPERIMENTAL: Loads a full circuit from a JSON-encoded string. The data must 
    be encoded using the proposed AltDSS Schema, see
    https://github.com/orgs/dss-extensions/discussions/ for links to docs and to
    provide feedback for future revisions.

    (API Extension)
    */
    ALTDSS_CAPI_DLL void Circuit_FromJSON(const char *circ, int32_t options);

    /*
    Flatten the circuit

    Flatten the circuit structures, removing any object of the following types:

    - XfmrCode
    - LineCode
    - LineSpacing
    - LineGeometry
    - WireData
    - CNData
    - TSData

    The general data from those objects is propagated to the referencing Line and Transformer objects,
    and the properties on the latter are updated to remove any references to the removed objects.

    This is useful for some converting the DSS circuit to another format, without requiring the user to handle all 
    the types listed above. This, of course, results in some limitations since a lot of detail is removed. Numerically,
    a normal snapshot or daily solution should be the same before and after the flatten operation.

    Available only on AltDSS.

    (API Extension)
    */
    ALTDSS_CAPI_DLL void Circuit_Flatten(int32_t options);

    /*! 
    Array of strings. Get  Bus definitions to which each terminal is connected. 0-based array.

    The `removeNodes` argument is an **API Extension**. Use it to get only the bus names, 
    without the connection/node specification, if present.
    */
    ALTDSS_CAPI_DLL void CktElement_Get_BusNames(char*** ResultPtr, int32_t* ResultDims, uint16_t removeNodes);

    /*! 
    Full Name of Active Circuit Element
    */
    ALTDSS_CAPI_DLL const char* CktElement_Get_Name(void);

    /*! 
    Number of Conductors per Terminal
    */
    ALTDSS_CAPI_DLL int32_t CktElement_Get_NumConductors(void);

    /*! 
    Number of Phases
    */
    ALTDSS_CAPI_DLL int32_t CktElement_Get_NumPhases(void);

    /*! 
    Number of Terminals this Circuit Element
    */
    ALTDSS_CAPI_DLL int32_t CktElement_Get_NumTerminals(void);

    /*! 
    Array of strings. Set Bus definitions for each terminal is connected.
    */
    ALTDSS_CAPI_DLL void CktElement_Set_BusNames(const char** ValuePtr, int32_t ValueCount);

    /*! 
    Complex array of currents into each conductor of each terminal
    */
    ALTDSS_CAPI_DLL void CktElement_Get_Currents(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as CktElement_Get_Currents but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void CktElement_Get_Currents_GR(void);

    /*! 
    Complex array of voltages at terminals
    */
    ALTDSS_CAPI_DLL void CktElement_Get_Voltages(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as CktElement_Get_Voltages but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void CktElement_Get_Voltages_GR(void);

    /*! 
    Emergency Ampere Rating for PD elements
    */
    ALTDSS_CAPI_DLL double CktElement_Get_EmergAmps(void);

    /*! 
    Boolean indicating that element is currently in the circuit.
    */
    ALTDSS_CAPI_DLL uint16_t CktElement_Get_Enabled(void);

    /*! 
    Total losses in the element: two-element double array (complex), in VA (watts, vars)
    */
    ALTDSS_CAPI_DLL void CktElement_Get_Losses(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as CktElement_Get_Losses but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void CktElement_Get_Losses_GR(void);

    /*! 
    Returns a complex array of the 3 types of losses (total losses, load losses, no-load losses) for the active circuit element, in VA.

    Added in May 2025. Same as `LossesByType` introduced for Transformers in AltDSS/DSS C-API in May 2019.
    */
    ALTDSS_CAPI_DLL void CktElement_Get_AllLosses(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as CktElement_Get_AllLosses but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void CktElement_Get_AllLosses_GR(void);

    /*! 
    Normal ampere rating for PD Elements
    */
    ALTDSS_CAPI_DLL double CktElement_Get_NormalAmps(void);

    /*! 
    Complex array of losses (kVA) by phase
    */
    ALTDSS_CAPI_DLL void CktElement_Get_PhaseLosses(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as CktElement_Get_PhaseLosses but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void CktElement_Get_PhaseLosses_GR(void);

    /*! 
    Complex array of powers (kVA) into each conductor of each terminal
    */
    ALTDSS_CAPI_DLL void CktElement_Get_Powers(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as CktElement_Get_Powers but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void CktElement_Get_Powers_GR(void);

    /*! 
    Double array of symmetrical component currents (magnitudes only) into each 3-phase terminal
    */
    ALTDSS_CAPI_DLL void CktElement_Get_SeqCurrents(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as CktElement_Get_SeqCurrents but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void CktElement_Get_SeqCurrents_GR(void);

    /*! 
    Complex array of sequence powers (kW, kvar) into each 3-phase terminal
    */
    ALTDSS_CAPI_DLL void CktElement_Get_SeqPowers(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as CktElement_Get_SeqPowers but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void CktElement_Get_SeqPowers_GR(void);

    /*! 
    Double array of symmetrical component voltages (magnitudes only) at each 3-phase terminal
    */
    ALTDSS_CAPI_DLL void CktElement_Get_SeqVoltages(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as CktElement_Get_SeqVoltages but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void CktElement_Get_SeqVoltages_GR(void);

    ALTDSS_CAPI_DLL void CktElement_Close(int32_t Term, int32_t Phs);

    ALTDSS_CAPI_DLL void CktElement_Open(int32_t Term, int32_t Phs);

    /*! 
    Emergency Ampere Rating
    */
    ALTDSS_CAPI_DLL void CktElement_Set_EmergAmps(double Value);

    /*! 
    Boolean indicating that element is currently in the circuit.
    */
    ALTDSS_CAPI_DLL void CktElement_Set_Enabled(uint16_t Value);

    /*! 
    Normal ampere rating
    */
    ALTDSS_CAPI_DLL void CktElement_Set_NormalAmps(double Value);

    /*
    Indicates if the specified terminal and, optionally, a specific phase conductor is open.

    Provide zero in the `Phs` argument to check if any conductor of the terminal `Term` is open.

    Provide a non-zero phase number in `Phs` to check if a specific phase conductor is open.

    Original COM help: https://opendss.epri.com/Open1.html
    */
    ALTDSS_CAPI_DLL uint16_t CktElement_IsOpen(int32_t Term, int32_t Phs);

    /*! 
    Array containing all property names of the active device.
    */
    ALTDSS_CAPI_DLL void CktElement_Get_AllPropertyNames(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Number of Properties this Circuit Element.
    */
    ALTDSS_CAPI_DLL int32_t CktElement_Get_NumProperties(void);

    /*! 
    Residual currents for each terminal: (magnitude, angle in degrees)
    */
    ALTDSS_CAPI_DLL void CktElement_Get_Residuals(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as CktElement_Get_Residuals but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void CktElement_Get_Residuals_GR(void);

    /*! 
    Order (size) of the active circuit element's primite Y matrix (Yprim), typically `NumConductors * NumTerminals`

    (API Extension)
    */
    ALTDSS_CAPI_DLL int32_t CktElement_Get_YprimOrder(void);

    /*! 
    YPrim matrix, column order, complex numbers (paired)
    */
    ALTDSS_CAPI_DLL void CktElement_Get_Yprim(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as CktElement_Get_Yprim but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void CktElement_Get_Yprim_GR(void);

    /*! 
    Display name of the object (not necessarily unique)
    */
    ALTDSS_CAPI_DLL const char* CktElement_Get_DisplayName(void);

    /*! 
    globally unique identifier for this object
    */
    ALTDSS_CAPI_DLL const char* CktElement_Get_GUID(void);

    /*! 
    Pointer to this object
    */
    ALTDSS_CAPI_DLL int32_t CktElement_Get_Handle(void);

    /*! 
    Display name of the object (not necessarily unique)
    */
    ALTDSS_CAPI_DLL void CktElement_Set_DisplayName(const char* Value);

    /*! 
    Full name of the i-th controller attached to this element. Ex: str = Controller(2).  See NumControls to determine valid index range
    */
    ALTDSS_CAPI_DLL const char* CktElement_Get_Controller(int32_t idx);

    /*! 
    Name of the Energy Meter this element is assigned to.
    */
    ALTDSS_CAPI_DLL const char* CktElement_Get_EnergyMeter(void);

    /*! 
    This element has a CapControl or RegControl attached.
    */
    ALTDSS_CAPI_DLL uint16_t CktElement_Get_HasVoltControl(void);

    /*! 
    This element has a SwtControl attached.
    */
    ALTDSS_CAPI_DLL uint16_t CktElement_Get_HasSwitchControl(void);

    /*! 
    Complex double array of Sequence Voltage for all terminals of active circuit element.
    */
    ALTDSS_CAPI_DLL void CktElement_Get_CplxSeqVoltages(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as CktElement_Get_CplxSeqVoltages but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void CktElement_Get_CplxSeqVoltages_GR(void);

    /*! 
    Complex double array of Sequence Currents for all conductors of all terminals of active circuit element.
    */
    ALTDSS_CAPI_DLL void CktElement_Get_CplxSeqCurrents(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as CktElement_Get_CplxSeqCurrents but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void CktElement_Get_CplxSeqCurrents_GR(void);

    /*! 
    Array of strings listing all the published variable names, if a PCElement. Otherwise, null string.
    */
    ALTDSS_CAPI_DLL void CktElement_Get_AllVariableNames(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Array of doubles. Values of state variables of active element if PC element.
    */
    ALTDSS_CAPI_DLL void CktElement_Get_AllVariableValues(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as CktElement_Get_AllVariableValues but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void CktElement_Get_AllVariableValues_GR(void);

    /*! 
    For PCElement, set/get the value of a variable by name. If Code>0 Then no variable by this name or not a PCelement.
    */
    ALTDSS_CAPI_DLL double CktElement_Get_Variable(const char* MyVarName, int32_t *Code);
    ALTDSS_CAPI_DLL void CktElement_Set_Variable(const char* MyVarName, int32_t *Code, double Value);

    /*! 
    For PCElement, set/get the value of a variable by integer index.
    */
    ALTDSS_CAPI_DLL double CktElement_Get_Variablei(int32_t Idx, int32_t *Code);
    ALTDSS_CAPI_DLL void CktElement_Set_Variablei(int32_t Idx, int32_t *Code, double Value);

    /*! 
    Array of integer containing the node numbers (representing phases, for example) for each conductor of each terminal.
    Be sure to run a solution to initialize the values after the circuit is created or modified.
    */
    ALTDSS_CAPI_DLL void CktElement_Get_NodeOrder(int32_t** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as CktElement_Get_NodeOrder but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void CktElement_Get_NodeOrder_GR(void);

    /*! 
    True if a recloser, relay, or fuse controlling this ckt element. OCP = Overcurrent Protection
    */
    ALTDSS_CAPI_DLL uint16_t CktElement_Get_HasOCPDevice(void);

    /*! 
    Number of controls connected to this device. Use to determine valid range for index into Controller array.
    */
    ALTDSS_CAPI_DLL int32_t CktElement_Get_NumControls(void);

    /*! 
    Index into Controller list of OCP Device controlling this CktElement
    */
    ALTDSS_CAPI_DLL int32_t CktElement_Get_OCPDevIndex(void);

    /*! 
    0=None; 1=Fuse; 2=Recloser; 3=Relay;  Type of OCP controller device
    */
    ALTDSS_CAPI_DLL int32_t CktElement_Get_OCPDevType(void);

    /*! 
    Currents in magnitude, angle (degrees) format as an array of doubles.
    */
    ALTDSS_CAPI_DLL void CktElement_Get_CurrentsMagAng(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as CktElement_Get_CurrentsMagAng but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void CktElement_Get_CurrentsMagAng_GR(void);

    /*! 
    Voltages at each conductor in magnitude, angle form as array of doubles.
    */
    ALTDSS_CAPI_DLL void CktElement_Get_VoltagesMagAng(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as CktElement_Get_VoltagesMagAng but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void CktElement_Get_VoltagesMagAng_GR(void);

    /*! 
    Returns true if the current active element is isolated.
    Note that this only fetches the current value. See also the Topology interface.

    (API Extension)
    */
    ALTDSS_CAPI_DLL uint16_t CktElement_Get_IsIsolated(void);

    /*! 
    Returns an array with the total powers (complex, kVA) at ALL terminals of the active circuit element.
    */
    ALTDSS_CAPI_DLL void CktElement_Get_TotalPowers(double** ResultPtr, int32_t* ResultDims);

    /*! 
    Same as CktElement_Get_TotalPowers but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void CktElement_Get_TotalPowers_GR(void);

    /*! 
    Gets the name of the active state variable if any, otherwise, returns and empty string
    */
    ALTDSS_CAPI_DLL const char* CktElement_Get_VariableName(void);

    /*!
    Sets the name of the active state variable for using with the VariableValue property
    */
    ALTDSS_CAPI_DLL void CktElement_Set_VariableName(const char* Value);
    
    /*!
    Gets the  present value for the active state variable. If there no active variable, returns a error message.
    */
    ALTDSS_CAPI_DLL double CktElement_Get_VariableValue(void);
    
    /*!
    Sets the given value to be the present value for the active state variable. If there no active variable, returns a error message.
    */
    ALTDSS_CAPI_DLL void CktElement_Set_VariableValue(double Value);

    /*!
    Gets the index of the active state variable if any, otherwise, returns -1
    */
    ALTDSS_CAPI_DLL int32_t CktElement_Get_VariableIdx(void);

    /*!
    Activates a  state variable by index for using with the VariableValue property
    */
    ALTDSS_CAPI_DLL void CktElement_Set_VariableIdx(int32_t Value);

    /*! 
    Convert real and imaginary doubles to Array of doubles
    */
    ALTDSS_CAPI_DLL void CmathLib_Get_cmplx(double** ResultPtr, int32_t* ResultDims, double RealPart, double ImagPart);
    /*! 
    Same as CmathLib_Get_cmplx but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void CmathLib_Get_cmplx_GR(double RealPart, double ImagPart);

    /*! 
    Return abs value of complex number given in real and imag doubles
    */
    ALTDSS_CAPI_DLL double CmathLib_Get_cabs(double realpart, double imagpart);

    /*! 
    Returns the angle, in degrees, of a complex number specified as two doubles: Realpart and imagpart.
    */
    ALTDSS_CAPI_DLL double CmathLib_Get_cdang(double RealPart, double ImagPart);

    /*! 
    Convert complex number to magnitude and angle, degrees. Returns array of two doubles.
    */
    ALTDSS_CAPI_DLL void CmathLib_Get_ctopolardeg(double** ResultPtr, int32_t* ResultDims, double RealPart, double ImagPart);
    /*! 
    Same as CmathLib_Get_ctopolardeg but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void CmathLib_Get_ctopolardeg_GR(double RealPart, double ImagPart);

    /*! 
    Convert magnitude, angle in degrees to a complex number. Returns Array of two doubles.
    */
    ALTDSS_CAPI_DLL void CmathLib_Get_pdegtocomplex(double** ResultPtr, int32_t* ResultDims, double magnitude, double angle);
    /*! 
    Same as CmathLib_Get_pdegtocomplex but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void CmathLib_Get_pdegtocomplex_GR(double magnitude, double angle);

    /*! 
    Multiply two complex numbers: (a1, b1) * (a2, b2). Returns result as an array of two doubles.
    */
    ALTDSS_CAPI_DLL void CmathLib_Get_cmul(double** ResultPtr, int32_t* ResultDims, double a1, double b1, double a2, double b2);
    /*! 
    Same as CmathLib_Get_cmul but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void CmathLib_Get_cmul_GR(double a1, double b1, double a2, double b2);

    /*! 
    Divide two complex number: (a1, b1)/(a2, b2). Returns array of two doubles representing complex result.
    */
    ALTDSS_CAPI_DLL void CmathLib_Get_cdiv(double** ResultPtr, int32_t* ResultDims, double a1, double b1, double a2, double b2);
    /*! 
    Same as CmathLib_Get_cdiv but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void CmathLib_Get_cdiv_GR(double a1, double b1, double a2, double b2);

    /*!
    Clear the control queue.
    */
    ALTDSS_CAPI_DLL void CtrlQueue_ClearQueue(void);

    /*!
    Delete an Action from the DSS Control Queue by the handle that is returned when the action is added.
    
    (The Push function returns the handle.)
    */
    ALTDSS_CAPI_DLL void CtrlQueue_Delete(int32_t ActionHandle);

    /*! 
    Code for the active action. Integer code to tell the control device what to do.
    
    Use this to determine what the user-defined controls are supposed to do.
    It can be any 32-bit integer of the user's choosing and is the same value that the control pushed onto the control queue earlier.
    */
    ALTDSS_CAPI_DLL int32_t CtrlQueue_Get_ActionCode(void);

    /*! 
    Handle (user defined) to device that must act on the pending action.
    
    The user-written code driving the interface may support more than one 
    control element as necessary to perform the simulation. This handle is
    an index returned to the user program that lets the program know which
    control is to perform the active action.        
    */
    ALTDSS_CAPI_DLL int32_t CtrlQueue_Get_DeviceHandle(void);

    /*! 
    Number of Actions on the current action list (that have been popped off the control queue by CheckControlActions)
    */
    ALTDSS_CAPI_DLL int32_t CtrlQueue_Get_NumActions(void);

    /*! 
    Push a control action onto the DSS control queue by time, action code, and device handle (user defined). Returns Control Queue handle.
    */
    ALTDSS_CAPI_DLL int32_t CtrlQueue_Push(int32_t Hour, double Seconds, int32_t ActionCode, int32_t DeviceHandle);

    /*!
    Export the queue to a CSV table and show it.    
    */
    ALTDSS_CAPI_DLL void CtrlQueue_Show(void);

    /*!
    Clear all actions from the Control Proxy's Action List (they are popped off the list). 
    */
    ALTDSS_CAPI_DLL void CtrlQueue_ClearActions(void);

    /*! 
    Pops next action off the action list and makes it the active action. Returns zero if none.
    */
    ALTDSS_CAPI_DLL int32_t CtrlQueue_Get_PopAction(void);

    /*! 
    Set the active action by index
    */
    ALTDSS_CAPI_DLL void CtrlQueue_Set_Action(int32_t Param1);

    /*! 
    Number of items on the OpenDSS control Queue
    */
    ALTDSS_CAPI_DLL int32_t CtrlQueue_Get_QueueSize(void);

    /*!
    Execute all actions currently on the Control Queue. 

    Side effect: clears the queue.    
    */
    ALTDSS_CAPI_DLL void CtrlQueue_DoAllQueue(void);

    /*! 
    Array of strings containing the entire queue in CSV format
    */
    ALTDSS_CAPI_DLL void CtrlQueue_Get_Queue(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Number of Circuits currently defined
    */
    ALTDSS_CAPI_DLL int32_t DSS_Get_NumCircuits(void);

    /*!

    Clear the current circuit and any related actor.

    On the AltDSS engine, also clears `SkipFileRegEx` and `SkipCommands`.

    */
    ALTDSS_CAPI_DLL void DSS_ClearAll(void);

    /*! 
    Get version string for the DSS.
    */
    ALTDSS_CAPI_DLL const char* DSS_Get_Version(void);

    /*! 
    Runs potential initialization of internal structures of the DSS engine.
    
    For DSS-Extensions, users are required to call this function at least
    once per process, especially in multi-threaded applications.

    Returns 0/false on failure. A failure might indicate an incompatibility between
    the Pascal threading system and the host process.
    */
    ALTDSS_CAPI_DLL uint16_t DSS_Start(int32_t code);

    /*! 
    List of DSS intrinsic classes (names of the classes)
    */
    ALTDSS_CAPI_DLL void DSS_Get_Classes(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    List of user-defined classes
    */
    ALTDSS_CAPI_DLL void DSS_Get_UserClasses(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Number of DSS intrinsic classes
    */
    ALTDSS_CAPI_DLL int32_t DSS_Get_NumClasses(void);

    /*! 
    Number of user-defined classes
    */
    ALTDSS_CAPI_DLL int32_t DSS_Get_NumUserClasses(void);

    /*! 
    DSS Data File Path.  Default path for reports, etc. from DSS
    */
    ALTDSS_CAPI_DLL const char* DSS_Get_DataPath(void);

    /*! 
    DSS Data File Path.  Default path for reports, etc. from DSS
    */
    ALTDSS_CAPI_DLL void DSS_Set_DataPath(const char* Value);

    ALTDSS_CAPI_DLL void DSS_Reset(void);

    /*! 
    Returns the path name for the default text editor.
    */
    ALTDSS_CAPI_DLL const char* DSS_Get_DefaultEditor(void);

    ALTDSS_CAPI_DLL int32_t DSS_SetActiveClass(const char* ClassName);
    
    /*!
    Indicates whether text output is allowed or forms are used.  Disable to silence most output.

    Currently, forms/windows are only used for EPRI's OpenDSS distribution on Windows.
    */
    ALTDSS_CAPI_DLL uint16_t DSS_Get_AllowForms(void);
    ALTDSS_CAPI_DLL void DSS_Set_AllowForms(uint16_t Value);

    /*! 
    Sets/gets the AllowEditor mode.

    AllowEditor controls whether the external editor is used in commands like "Show".
    If you set to 0 (false), the editor is not executed. Note that other side effects,
    such as the creation of files, are not affected.
    */
    ALTDSS_CAPI_DLL uint16_t DSS_Get_AllowEditor(void);
    ALTDSS_CAPI_DLL void DSS_Set_AllowEditor(uint16_t Value);

    /*! 
    LegacyModels was a flag used to toggle legacy (pre-2019) models for PVSystem, InvControl, Storage and
    StorageControl.
    In the official OpenDSS version 9.0, the old models were removed. They were temporarily present here
    but were also removed in DSS C-API v0.13.0.
        
    NOTE: this function pair will be removed for v1.0. It is left to avoid breaking the current API too soon.
    
    (API Extension)
    */
    ALTDSS_CAPI_DLL uint16_t DSS_Get_LegacyModels(void);
    ALTDSS_CAPI_DLL void DSS_Set_LegacyModels(uint16_t Value);

    /*! 
    If enabled, the DOScmd command is allowed. Otherwise, an error is reported if the user tries to use it.
    
    Defaults to False/0 (disabled state). Users should consider DOScmd deprecated on DSS-Extensions.
    
    This can also be set through the environment variable DSS_CAPI_ALLOW_DOSCMD. Setting it to 1 enables
    the command.
    
    (API Extension)
    */
    ALTDSS_CAPI_DLL uint16_t DSS_Get_AllowDOScmd(void);
    ALTDSS_CAPI_DLL void DSS_Set_AllowDOScmd(uint16_t Value);

    /*! 
    If disabled, the engine will not change the active working directory during execution. E.g. a "compile"
    command will not "chdir" to the file path.
    
    If you have issues with long paths, enabling this might help in some scenarios.
    
    Defaults to True (allow changes, backwards compatible) in the 0.12.x versions of DSS C-API. 
    This might change to false in future versions.
    
    This can also be set through the environment variable DSS_CAPI_ALLOW_CHANGE_DIR. Setting it to 0 to
    disallow changing the active working directory.
    
    (API Extension)
    */
    ALTDSS_CAPI_DLL uint16_t DSS_Get_AllowChangeDir(void);
    ALTDSS_CAPI_DLL void DSS_Set_AllowChangeDir(uint16_t Value);
    
    /*! 
    If enabled, the engine will fill the array dimensions as the third and forth elements of 
    the "count" pointer (first elements is the current size, second is the capacity). For user-managed
    memory, the user must provide a valid "count" pointer with the correct capacity.

    Most matrices from the DSS engine are column-major (a.k.a. "Fortran order").

    If the array is not a matrix, the elements are left as zeroes, i.e. the current size can be used as 
    the dimension of the vector.
    For complex matrices, the sizes are referred to the number of complex elements, not the primary the float64 elements.

    Defaults to true/1 starting in the 0.15.x versions of DSS C-API.

    **Deprecated: this will be removed in a future version; be sure that `ResultDims` values can always accomodate 4 int32 values**

    (API Extension)
    */
    ALTDSS_CAPI_DLL uint16_t DSS_Get_EnableArrayDimensions(void);
    ALTDSS_CAPI_DLL void DSS_Set_EnableArrayDimensions(uint16_t Value);

    /*! 
    Controls some compatibility flags introduced to toggle some behavior from the official OpenDSS.
    The current bit flags are listed in the enum description.

    The flags may change for each version of DSS C-API, but the same value will not be reused. That is,
    when we remove a compatibility flag, it will have no effect but will also not affect anything else
    besides raising an error if the user tries to toggle a flag that was available in a previous version.

    We expect to keep a very limited number of flags. Since the flags are more transient than the other
    options/flags, it was preferred to add this generic function instead of a separate function per
    flag.

    **These flags are global**, affecting any DSS context in the process.

    Related enumeration: DSSCompatFlags

    (API Extension)
    */
    ALTDSS_CAPI_DLL uint32_t DSS_Get_CompatFlags(void);
    ALTDSS_CAPI_DLL void DSS_Set_CompatFlags(uint32_t Value);

    /*! 
    If enabled, in case of errors or empty arrays, the API returns arrays with values compatible with the 
    official OpenDSS COM interface. 
    
    For example, consider the function Loads_Get_ZIPV. If there is no active circuit or active load element:
    - In the disabled state (COMErrorResults=False), the function will return "[]", an array with 0 elements.
    - In the enabled state (COMErrorResults=True), the function will return "[0.0]" instead. This should
      be compatible with the return value of the official COM interface.
    
    Defaults to false (disabled state) in AltDSS since the v0.15.x series.

    This does not affect the results when using EPRI's OpenDSS distribution through Oddie.

    This can also be set through the environment variable `DSS_CAPI_COM_DEFAULTS`. Setting it to 1 enables
    the legacy/COM behavior. The value can be toggled through the API at any time.
    
    (API Extension)
    */
    ALTDSS_CAPI_DLL uint16_t DSS_Get_COMErrorResults(void);
    ALTDSS_CAPI_DLL void DSS_Set_COMErrorResults(uint16_t Value);

    /*! 
    Array of strings containing the names of all properties for the active DSS object.
    */
    ALTDSS_CAPI_DLL void DSSElement_Get_AllPropertyNames(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Full Name of Active DSS Object (general element or circuit element).
    */
    ALTDSS_CAPI_DLL const char* DSSElement_Get_Name(void);

    /*! 
    Number of Properties for the active DSS object.
    */
    ALTDSS_CAPI_DLL int32_t DSSElement_Get_NumProperties(void);

    /*! 
    Returns the properties of the active DSS object as a JSON-encoded string.

    The `options` parameter contains bit-flags to toggle specific features.
    See `Obj_ToJSON` for more.

    (API Extension)
    */
    ALTDSS_CAPI_DLL const char* DSSElement_ToJSON(int32_t options);

    ALTDSS_CAPI_DLL void DSSimComs_BusVoltagepu(double** ResultPtr, int32_t* ResultDims, size_t Index);
    /*! 
    Same as DSSimComs_BusVoltagepu but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void DSSimComs_BusVoltagepu_GR(size_t Index);

    ALTDSS_CAPI_DLL void DSSimComs_BusVoltage(double** ResultPtr, int32_t* ResultDims, size_t Index);
    /*! 
    Same as DSSimComs_BusVoltage but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void DSSimComs_BusVoltage_GR(size_t Index);

    ALTDSS_CAPI_DLL void DSSProgress_Close(void);

    /*! 
    Caption to appear on the bottom of the DSS Progress form.
    */
    ALTDSS_CAPI_DLL void DSSProgress_Set_Caption(const char* Value);

    /*! 
    Set the percent progress to indicate [0..100] on the progress form.

    Typically used with EPRI's OpenDSS, on Windows. Otherwise, it could be a no-op.

    Original COM help: https://opendss.epri.com/PctProgress.html
    */
    ALTDSS_CAPI_DLL void DSSProgress_Set_PctProgress(int32_t Value);

    ALTDSS_CAPI_DLL void DSSProgress_Show(void);

    /*! 
    Description of the property.
    */
    ALTDSS_CAPI_DLL const char* DSSProperty_Get_Description(void);

    /*! 
    Name of Property
    */
    ALTDSS_CAPI_DLL const char* DSSProperty_Get_Name(void);

    /*!
    Get/set the value of the active property. The value must be specified as a string.
    */
    ALTDSS_CAPI_DLL const char* DSSProperty_Get_Val(void);

    ALTDSS_CAPI_DLL void DSSProperty_Set_Val(const char* Value);
    ALTDSS_CAPI_DLL void DSSProperty_Set_Name(const char* Value);
    ALTDSS_CAPI_DLL void DSSProperty_Set_Index(int32_t Value);

    /*! 
    Get i-th command
    */
    ALTDSS_CAPI_DLL const char* DSS_Executive_Get_Command(int32_t i);

    /*! 
    Number of DSS Executive Commands
    */
    ALTDSS_CAPI_DLL int32_t DSS_Executive_Get_NumCommands(void);

    /*! 
    Number of DSS Executive Options
    */
    ALTDSS_CAPI_DLL int32_t DSS_Executive_Get_NumOptions(void);

    /*! 
    Get i-th option
    */
    ALTDSS_CAPI_DLL const char* DSS_Executive_Get_Option(int32_t i);

    /*! 
    Get help string for i-th command
    */
    ALTDSS_CAPI_DLL const char* DSS_Executive_Get_CommandHelp(int32_t i);

    /*! 
    Get help string for i-th option
    */
    ALTDSS_CAPI_DLL const char* DSS_Executive_Get_OptionHelp(int32_t i);

    /*! 
    Get present value of i-th option
    */
    ALTDSS_CAPI_DLL const char* DSS_Executive_Get_OptionValue(int32_t i);

    /*! 
    Description of error for last operation
    */
    ALTDSS_CAPI_DLL const char* Error_Get_Description(void);

    /*! 
    Set the description of error for last operation; for advanced usage only, e.g. callbacks

    (API Extension)
    */
    ALTDSS_CAPI_DLL void Error_Set_Description(const char* Value);

    /*! 
    Error Number (returns current value and then resets to zero)
    */
    ALTDSS_CAPI_DLL int32_t Error_Get_Number(void);

    /*! 
    Integer pointer to the Error Number. Remember to reset its value to zero after the error treatment.
    */
    ALTDSS_CAPI_DLL int32_t* Error_Get_NumberPtr(void);

    /*! 
    Array of strings containing names of all Fuses in the circuit
    */
    ALTDSS_CAPI_DLL void Fuses_Get_AllNames(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Number of Fuse elements in the circuit
    */
    ALTDSS_CAPI_DLL int32_t Fuses_Get_Count(void);

    /*! 
    Set the first Fuse to be the active fuse. Returns 0 if none.
    */
    ALTDSS_CAPI_DLL int32_t Fuses_Get_First(void);

    /*! 
    Get the name of the active Fuse element
    */
    ALTDSS_CAPI_DLL const char* Fuses_Get_Name(void);

    /*! 
    Advance the active Fuse element pointer to the next fuse. Returns 0 if no more fuses.
    */
    ALTDSS_CAPI_DLL int32_t Fuses_Get_Next(void);

    /*! 
    Set the active Fuse element by name.
    */
    ALTDSS_CAPI_DLL void Fuses_Set_Name(const char* Value);

    /*! 
    Full name of the circuit element to which the fuse is connected.
    */
    ALTDSS_CAPI_DLL const char* Fuses_Get_MonitoredObj(void);

    /*! 
    Terminal number to which the fuse is connected.
    */
    ALTDSS_CAPI_DLL int32_t Fuses_Get_MonitoredTerm(void);

    /*! 
    Full name of the circuit element switch that the fuse controls. Defaults to the MonitoredObj.
    */
    ALTDSS_CAPI_DLL const char* Fuses_Get_SwitchedObj(void);

    /*! 
    Full name of the circuit element to which the fuse is connected.
    */
    ALTDSS_CAPI_DLL void Fuses_Set_MonitoredObj(const char* Value);

    /*! 
    Number of the terminal to which the fuse is connected
    */
    ALTDSS_CAPI_DLL void Fuses_Set_MonitoredTerm(int32_t Value);

    /*! 
    Full name of the circuit element switch that the fuse controls. Defaults to MonitoredObj.
    */
    ALTDSS_CAPI_DLL void Fuses_Set_SwitchedObj(const char* Value);

    /*! 
    Number of the terminal containing the switch controlled by the fuse.
    */
    ALTDSS_CAPI_DLL int32_t Fuses_Get_SwitchedTerm(void);

    /*! 
    Number of the terminal of the controlled element containing the switch controlled by the fuse.
    */
    ALTDSS_CAPI_DLL void Fuses_Set_SwitchedTerm(int32_t Value);

    /*! 
    Name of the TCCcurve object that determines fuse blowing.
    */
    ALTDSS_CAPI_DLL const char* Fuses_Get_TCCcurve(void);

    /*! 
    Name of the TCCcurve object that determines fuse blowing.
    */
    ALTDSS_CAPI_DLL void Fuses_Set_TCCcurve(const char* Value);

    /*! 
    Multiplier or actual amps for the TCCcurve object. Defaults to 1.0.  Multiply current values of TCC curve by this to get actual amps.
    */
    ALTDSS_CAPI_DLL double Fuses_Get_RatedCurrent(void);

    /*! 
    Multiplier or actual fuse amps for the TCC curve. Defaults to 1.0. Has to correspond to the Current axis of TCCcurve object.
    */
    ALTDSS_CAPI_DLL void Fuses_Set_RatedCurrent(double Value);

    /*! 
    A fixed delay time in seconds added to the fuse blowing time determined by the TCC curve. Default is 0.
    */
    ALTDSS_CAPI_DLL double Fuses_Get_Delay(void);

    /*! 
    Manual opening of all phases of the fuse.
    */
    ALTDSS_CAPI_DLL void Fuses_Open(void);

    /*! 
    Close all phases of the fuse.
    */
    ALTDSS_CAPI_DLL void Fuses_Close(void);

    /*! 
    Fixed delay time in seconds added to the fuse blowing time to represent fuse clear or other delay.
    */
    ALTDSS_CAPI_DLL void Fuses_Set_Delay(double Value);

    /*! 
    Current state of the fuses. TRUE if any fuse on any phase is blown. Else FALSE.
    */
    ALTDSS_CAPI_DLL uint16_t Fuses_IsBlown(void);

    /*! 
    Get/set active fuse by index into the list of fuses. 1 based: 1..count
    */
    ALTDSS_CAPI_DLL int32_t Fuses_Get_idx(void);

    /*! 
    Set Fuse active by index into the list of fuses. 1..count
    */
    ALTDSS_CAPI_DLL void Fuses_Set_idx(int32_t Value);

    /*! 
    Reset fuse to normal state.
    */
    ALTDSS_CAPI_DLL void Fuses_Reset(void);
    
    /*! 
    Array of strings ('open' or 'closed') indicating the state of each phase of the fuse.
    */
    ALTDSS_CAPI_DLL void Fuses_Get_State(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Array of strings ('open' or 'closed') indicating the state of each phase of the fuse.
    */
    ALTDSS_CAPI_DLL void Fuses_Set_State(const char** ValuePtr, int32_t ValueCount);

    /*! 
    Array of strings ('open' or 'closed') indicating the normal state of each phase of the fuse.
    */
    ALTDSS_CAPI_DLL void Fuses_Get_NormalState(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Array of strings ('open' or 'closed') indicating the normal state of each phase of the fuse.
    */
    ALTDSS_CAPI_DLL void Fuses_Set_NormalState(const char** ValuePtr, int32_t ValueCount);

    /*! 
    Number of phases, this fuse.
    */
    ALTDSS_CAPI_DLL int32_t Fuses_Get_NumPhases(void);

    /*! 
    Array of names of all Generator objects.
    */
    ALTDSS_CAPI_DLL void Generators_Get_AllNames(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Sets first Generator to be active.  Returns 0 if none.
    */
    ALTDSS_CAPI_DLL int32_t Generators_Get_First(void);

    /*! 
    Sets a generator active by name.
    */
    ALTDSS_CAPI_DLL const char* Generators_Get_Name(void);

    /*! 
    Sets next Generator to be active.  Returns 0 if no more.
    */
    ALTDSS_CAPI_DLL int32_t Generators_Get_Next(void);

    /*! 
    Array of Names of all generator energy meter registers
    
    See also the enum `GeneratorRegisters`.
    */
    ALTDSS_CAPI_DLL void Generators_Get_RegisterNames(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Array of values in generator energy meter registers.
    */
    ALTDSS_CAPI_DLL void Generators_Get_RegisterValues(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Generators_Get_RegisterValues but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Generators_Get_RegisterValues_GR(void);

    /*! 
    Indicates whether the generator is forced ON regardless of other dispatch criteria.
    */
    ALTDSS_CAPI_DLL uint16_t Generators_Get_ForcedON(void);

    /*! 
    Indicates whether the generator is forced ON regardless of other dispatch criteria.
    */
    ALTDSS_CAPI_DLL void Generators_Set_ForcedON(uint16_t Value);

    /*! 
    Sets a generator active by name.
    */
    ALTDSS_CAPI_DLL void Generators_Set_Name(const char* Value);

    /*! 
    Voltage base for the active generator, kV
    */
    ALTDSS_CAPI_DLL double Generators_Get_kV(void);

    /*! 
    kvar output for the active generator. Updates power factor based on present kW value.
    */
    ALTDSS_CAPI_DLL double Generators_Get_kvar(void);

    /*! 
    kW output for the active generator. kvar is updated for current power factor.
    */
    ALTDSS_CAPI_DLL double Generators_Get_kW(void);

    /*! 
    Power factor (pos. = producing vars). Updates kvar based on present kW value.
    */
    ALTDSS_CAPI_DLL double Generators_Get_PF(void);

    /*! 
    Number of phases
    */
    ALTDSS_CAPI_DLL int32_t Generators_Get_Phases(void);

    /*! 
    Voltage base for the active generator, kV
    */
    ALTDSS_CAPI_DLL void Generators_Set_kV(double Value);

    /*! 
    kvar output for the active generator. Updates power factor based on present kW.
    */
    ALTDSS_CAPI_DLL void Generators_Set_kvar(double Value);

    /*! 
    kW output for the active generator. kvar is updated for current power factor
    */
    ALTDSS_CAPI_DLL void Generators_Set_kW(double Value);

    /*! 
    Power factor (pos. = producing vars). Updates kvar based on present kW value.
    */
    ALTDSS_CAPI_DLL void Generators_Set_PF(double Value);

    /*! 
    Number of phases
    */
    ALTDSS_CAPI_DLL void Generators_Set_Phases(int32_t Value);

    /*! 
    Number of Generator Objects in Active Circuit
    */
    ALTDSS_CAPI_DLL int32_t Generators_Get_Count(void);

    /*! 
    Get/Set active Generator by index into generators list.  1..Count
    */
    ALTDSS_CAPI_DLL int32_t Generators_Get_idx(void);

    /*! 
    Get/Set active Generator by index into generators list. 1..Count
    */
    ALTDSS_CAPI_DLL void Generators_Set_idx(int32_t Value);

    /*! 
    Generator Model
    */
    ALTDSS_CAPI_DLL int32_t Generators_Get_Model(void);

    /*! 
    Generator Model
    */
    ALTDSS_CAPI_DLL void Generators_Set_Model(int32_t Value);

    /*! 
    kVA rating of the generator
    */
    ALTDSS_CAPI_DLL double Generators_Get_kVArated(void);

    /*! 
    KVA Rating of the generator
    */
    ALTDSS_CAPI_DLL void Generators_Set_kVArated(double Value);

    /*! 
    vmaxpu for Generator model
    */
    ALTDSS_CAPI_DLL double Generators_Get_Vmaxpu(void);

    /*! 
    Vminpu for Generator model
    */
    ALTDSS_CAPI_DLL double Generators_Get_Vminpu(void);

    /*! 
    Vmaxpu for generator model
    */
    ALTDSS_CAPI_DLL void Generators_Set_Vmaxpu(double Value);

    /*! 
    Vminpu for Generator model
    */
    ALTDSS_CAPI_DLL void Generators_Set_Vminpu(double Value);

    /*! 
    Name of the loadshape for a daily generation profile.

    (API Extension)
    */
    ALTDSS_CAPI_DLL const char* Generators_Get_daily(void);
    ALTDSS_CAPI_DLL void Generators_Set_daily(const char* Value);

    /*! 
    Name of the loadshape for a duty cycle simulation.

    (API Extension)
    */
    ALTDSS_CAPI_DLL const char* Generators_Get_duty(void);
    ALTDSS_CAPI_DLL void Generators_Set_duty(const char* Value);

    /*! 
    Name of yearly loadshape

    (API Extension)
    */
    ALTDSS_CAPI_DLL const char* Generators_Get_Yearly(void);
    ALTDSS_CAPI_DLL void Generators_Set_Yearly(const char* Value);

    /*! 
    Response to dispatch multipliers: Fixed=1 (dispatch multipliers do not apply), Variable=0 (follows curves).

    Related enumeration: GeneratorStatus

    (API Extension)
    */
    ALTDSS_CAPI_DLL int32_t Generators_Get_Status(void);
    ALTDSS_CAPI_DLL void Generators_Set_Status(int32_t Value);

    /*! 
    Generator connection. True/1 if delta connection, False/0 if wye.

    (API Extension)
    */
    ALTDSS_CAPI_DLL uint16_t Generators_Get_IsDelta(void);
    ALTDSS_CAPI_DLL void Generators_Set_IsDelta(uint16_t Value);

    /*! 
    kVA rating of electrical machine. Applied to machine or inverter definition for Dynamics mode solutions.

    (Same as kVArated, added for consistency)

    (API Extension)
    */
    ALTDSS_CAPI_DLL double Generators_Get_kva(void);
    ALTDSS_CAPI_DLL void Generators_Set_kva(double Value);

    /*! 
    An arbitrary integer number representing the class of Generator so that Generator values may be segregated by class.

    (API Extension)
    */
    ALTDSS_CAPI_DLL int32_t Generators_Get_Class_(void);
    ALTDSS_CAPI_DLL void Generators_Set_Class_(int32_t Value);

    /*! 
    Bus to which the Generator is connected. May include specific node specification.
    
    (API Extension)
    */
    ALTDSS_CAPI_DLL const char* Generators_Get_Bus1(void);
    ALTDSS_CAPI_DLL void Generators_Set_Bus1(const char* Value);

    /*! 
    Names of all GICSource Objects
    */
    ALTDSS_CAPI_DLL void GICSources_Get_AllNames(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Number of GICSource objects in Active Circuit.
    */
    ALTDSS_CAPI_DLL int32_t GICSources_Get_Count(void);

    /*! 
    Invoking this property sets the first element active.  Returns 0 if no GICSources.  Otherwise, index of the GICSource element.
    */
    ALTDSS_CAPI_DLL int32_t GICSources_Get_First(void);

    /*! 
    Invoking this property advances to the next GICSource element active.  Returns 0 if no more GICSources.  Otherwise, index of the GICSource element.
    */
    ALTDSS_CAPI_DLL int32_t GICSources_Get_Next(void);

    /*! 
    Specify the name of the GICSource element to set it active.
    */
    ALTDSS_CAPI_DLL const char* GICSources_Get_Name(void);

    /*! 
    Specify the name of the GICSource element to set it active.
    */
    ALTDSS_CAPI_DLL void GICSources_Set_Name(const char* Value);

    /*! 
    Number of Phases, this GICSource element.
    */
    ALTDSS_CAPI_DLL int32_t GICSources_Get_Phases(void);

    /*! 
    Number of Phases, this GICSource element.
    */
    ALTDSS_CAPI_DLL void GICSources_Set_Phases(int32_t Value);

    /*! 
    First bus name of GICSource (Created name)
    */
    ALTDSS_CAPI_DLL const char* GICSources_Get_Bus1(void);

    /*! 
    Second bus name
    */
    ALTDSS_CAPI_DLL const char* GICSources_Get_Bus2(void);

    /*! 
    Northward E Field V/km
    */
    ALTDSS_CAPI_DLL double GICSources_Get_EN(void);

    /*! 
    Northward E Field V/km
    */
    ALTDSS_CAPI_DLL void GICSources_Set_EN(double Value);

    /*! 
    Eastward E Field, V/km
    */
    ALTDSS_CAPI_DLL double GICSources_Get_EE(void);

    /*! 
    Eastward E Field, V/km
    */
    ALTDSS_CAPI_DLL void GICSources_Set_EE(double Value);

    /*! 
    Latitude of Bus1 (degrees)
    */
    ALTDSS_CAPI_DLL double GICSources_Get_Lat1(void);

    /*! 
    Latitude of Bus1 (degrees)
    */
    ALTDSS_CAPI_DLL void GICSources_Set_Lat1(double Value);

    /*! 
    Latitude of Bus2 (degrees)
    */
    ALTDSS_CAPI_DLL double GICSources_Get_Lat2(void);

    /*! 
    Latitude of Bus2 (degrees)
    */
    ALTDSS_CAPI_DLL void GICSources_Set_Lat2(double Value);

    /*! 
    Longitude of Bus1 (Degrees)
    */
    ALTDSS_CAPI_DLL double GICSources_Get_Lon1(void);

    /*! 
    Longitude of Bus1 (Degrees)
    */
    ALTDSS_CAPI_DLL void GICSources_Set_Lon1(double Value);

    /*! 
    Longitude of Bus2 (Degrees)
    */
    ALTDSS_CAPI_DLL double GICSources_Get_Lon2(void);

    /*! 
    Longitude of Bus2 (Degrees)
    */
    ALTDSS_CAPI_DLL void GICSources_Set_Lon2(double Value);

    /*! 
    Specify dc voltage directly
    */
    ALTDSS_CAPI_DLL double GICSources_Get_Volts(void);

    /*! 
    Specify dc voltage directly
    */
    ALTDSS_CAPI_DLL void GICSources_Set_Volts(double Value);

    /*! 
    Array of strings containing names of all ISOURCE elements.
    */
    ALTDSS_CAPI_DLL void ISources_Get_AllNames(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Count: Number of ISOURCE elements.
    */
    ALTDSS_CAPI_DLL int32_t ISources_Get_Count(void);

    /*! 
    Set the First ISOURCE to be active; returns Zero if none.
    */
    ALTDSS_CAPI_DLL int32_t ISources_Get_First(void);

    /*! 
    Sets the next ISOURCE element to be the active one. Returns Zero if no more.
    */
    ALTDSS_CAPI_DLL int32_t ISources_Get_Next(void);

    /*! 
    Get name of active ISOURCE
    */
    ALTDSS_CAPI_DLL const char* ISources_Get_Name(void);

    /*! 
    Set Active ISOURCE by name
    */
    ALTDSS_CAPI_DLL void ISources_Set_Name(const char* Value);

    /*! 
    Get the magnitude of the ISOURCE in amps
    */
    ALTDSS_CAPI_DLL double ISources_Get_Amps(void);

    /*! 
    Set the magnitude of the ISOURCE, amps
    */
    ALTDSS_CAPI_DLL void ISources_Set_Amps(double Value);

    /*! 
    Phase angle for ISOURCE, degrees
    */
    ALTDSS_CAPI_DLL double ISources_Get_AngleDeg(void);

    /*! 
    The present frequency of the ISOURCE, Hz
    */
    ALTDSS_CAPI_DLL double ISources_Get_Frequency(void);

    /*! 
    Phase angle for ISOURCE, degrees
    */
    ALTDSS_CAPI_DLL void ISources_Set_AngleDeg(double Value);

    /*! 
    Set the present frequency for the ISOURCE
    */
    ALTDSS_CAPI_DLL void ISources_Set_Frequency(double Value);

    /*! 
    Number of LineCodes
    */
    ALTDSS_CAPI_DLL int32_t LineCodes_Get_Count(void);

    ALTDSS_CAPI_DLL int32_t LineCodes_Get_First(void);

    ALTDSS_CAPI_DLL int32_t LineCodes_Get_Next(void);

    /*! 
    Name of active LineCode
    */
    ALTDSS_CAPI_DLL const char* LineCodes_Get_Name(void);

    ALTDSS_CAPI_DLL void LineCodes_Set_Name(const char* Value);

    /*! 
    Flag denoting whether impedance data were entered in symmetrical components
    */
    ALTDSS_CAPI_DLL uint16_t LineCodes_Get_IsZ1Z0(void);

    ALTDSS_CAPI_DLL int32_t LineCodes_Get_Units(void);

    ALTDSS_CAPI_DLL void LineCodes_Set_Units(int32_t Value);

    /*! 
    Number of Phases
    */
    ALTDSS_CAPI_DLL int32_t LineCodes_Get_Phases(void);

    /*! 
    Number of Phases
    */
    ALTDSS_CAPI_DLL void LineCodes_Set_Phases(int32_t Value);

    /*! 
    Positive-sequence resistance ohms per unit length
    */
    ALTDSS_CAPI_DLL double LineCodes_Get_R1(void);

    ALTDSS_CAPI_DLL void LineCodes_Set_R1(double Value);

    /*! 
    Positive-sequence reactance, ohms per unit length
    */
    ALTDSS_CAPI_DLL double LineCodes_Get_X1(void);

    ALTDSS_CAPI_DLL void LineCodes_Set_X1(double Value);

    /*! 
    Zero-sequence Resistance, ohms per unit length
    */
    ALTDSS_CAPI_DLL double LineCodes_Get_R0(void);

    /*! 
    Zero-sequence Reactance, Ohms per unit length
    */
    ALTDSS_CAPI_DLL double LineCodes_Get_X0(void);

    ALTDSS_CAPI_DLL void LineCodes_Set_R0(double Value);

    ALTDSS_CAPI_DLL void LineCodes_Set_X0(double Value);

    /*! 
    Zero-sequence capacitance, nF per unit length
    */
    ALTDSS_CAPI_DLL double LineCodes_Get_C0(void);

    /*! 
    Positive-sequence capacitance, nF per unit length
    */
    ALTDSS_CAPI_DLL double LineCodes_Get_C1(void);

    ALTDSS_CAPI_DLL void LineCodes_Set_C0(double Value);

    ALTDSS_CAPI_DLL void LineCodes_Set_C1(double Value);

    /*! 
    Capacitance matrix, nF per unit length
    */
    ALTDSS_CAPI_DLL void LineCodes_Get_Cmatrix(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as LineCodes_Get_Cmatrix but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void LineCodes_Get_Cmatrix_GR(void);

    /*! 
    Resistance matrix, ohms per unit length
    */
    ALTDSS_CAPI_DLL void LineCodes_Get_Rmatrix(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as LineCodes_Get_Rmatrix but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void LineCodes_Get_Rmatrix_GR(void);

    /*! 
    Reactance matrix, ohms per unit length
    */
    ALTDSS_CAPI_DLL void LineCodes_Get_Xmatrix(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as LineCodes_Get_Xmatrix but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void LineCodes_Get_Xmatrix_GR(void);

    ALTDSS_CAPI_DLL void LineCodes_Set_Cmatrix(const double* ValuePtr, int32_t ValueCount);

    ALTDSS_CAPI_DLL void LineCodes_Set_Rmatrix(const double* ValuePtr, int32_t ValueCount);

    ALTDSS_CAPI_DLL void LineCodes_Set_Xmatrix(const double* ValuePtr, int32_t ValueCount);

    /*! 
    Normal Ampere rating
    */
    ALTDSS_CAPI_DLL double LineCodes_Get_NormAmps(void);

    ALTDSS_CAPI_DLL void LineCodes_Set_NormAmps(double Value);

    /*! 
    Emergency ampere rating
    */
    ALTDSS_CAPI_DLL double LineCodes_Get_EmergAmps(void);

    ALTDSS_CAPI_DLL void LineCodes_Set_EmergAmps(double Value);

    /*! 
    Array of strings with names of all devices
    */
    ALTDSS_CAPI_DLL void LineCodes_Get_AllNames(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Names of all Line Objects
    */
    ALTDSS_CAPI_DLL void Lines_Get_AllNames(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Name of bus for terminal 1.
    */
    ALTDSS_CAPI_DLL const char* Lines_Get_Bus1(void);

    /*! 
    Name of bus for terminal 2.
    */
    ALTDSS_CAPI_DLL const char* Lines_Get_Bus2(void);

    /*! 
    Invoking this property sets the first element active.  Returns 0 if no lines.  Otherwise, index of the line element.
    */
    ALTDSS_CAPI_DLL int32_t Lines_Get_First(void);

    /*! 
    Length of line section in units compatible with the LineCode definition.
    */
    ALTDSS_CAPI_DLL double Lines_Get_Length(void);

    /*! 
    Name of LineCode object that defines the impedances.
    */
    ALTDSS_CAPI_DLL const char* Lines_Get_LineCode(void);

    /*! 
    Specify the name of the Line element to set it active.
    */
    ALTDSS_CAPI_DLL const char* Lines_Get_Name(void);

    /*! 
    Invoking this property advances to the next Line element active.  Returns 0 if no more lines.  Otherwise, index of the line element.
    */
    ALTDSS_CAPI_DLL int32_t Lines_Get_Next(void);

    /*! 
    Number of Phases, this Line element.
    */
    ALTDSS_CAPI_DLL int32_t Lines_Get_Phases(void);

    /*! 
    Positive-sequence resistance, ohms per unit length.
    */
    ALTDSS_CAPI_DLL double Lines_Get_R1(void);

    /*! 
    Positive-sequence reactance, ohms per unit length.
    */
    ALTDSS_CAPI_DLL double Lines_Get_X1(void);

    ALTDSS_CAPI_DLL int32_t Lines_New(const char* Name);

    /*! 
    Name of bus for terminal 1.
    */
    ALTDSS_CAPI_DLL void Lines_Set_Bus1(const char* Value);

    /*! 
    Name of bus for terminal 2.
    */
    ALTDSS_CAPI_DLL void Lines_Set_Bus2(const char* Value);

    /*! 
    Length of line section in units compatible with the LineCode definition.
    */
    ALTDSS_CAPI_DLL void Lines_Set_Length(double Value);

    /*! 
    Name of LineCode object that defines the impedances.
    */
    ALTDSS_CAPI_DLL void Lines_Set_LineCode(const char* Value);

    /*! 
    Specify the name of the Line element to set it active.
    */
    ALTDSS_CAPI_DLL void Lines_Set_Name(const char* Value);

    /*! 
    Number of Phases, this Line element.
    */
    ALTDSS_CAPI_DLL void Lines_Set_Phases(int32_t Value);

    /*! 
    Positive-sequence resistance, ohms per unit length.
    */
    ALTDSS_CAPI_DLL void Lines_Set_R1(double Value);

    /*! 
    Positive-sequence reactance, ohms per unit length.
    */
    ALTDSS_CAPI_DLL void Lines_Set_X1(double Value);

    /*! 
    Zero-sequence capacitance, nanofarads per unit length.
    */
    ALTDSS_CAPI_DLL double Lines_Get_C0(void);

    /*! 
    Positive-sequence capacitance, nanofarads per unit length.
    */
    ALTDSS_CAPI_DLL double Lines_Get_C1(void);

    ALTDSS_CAPI_DLL void Lines_Get_Cmatrix(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Lines_Get_Cmatrix but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Lines_Get_Cmatrix_GR(void);

    /*! 
    Zero-sequence resistance, ohms per unit length.
    */
    ALTDSS_CAPI_DLL double Lines_Get_R0(void);

    /*! 
    Resistance matrix (full), ohms per unit length. Array of doubles.
    */
    ALTDSS_CAPI_DLL void Lines_Get_Rmatrix(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Lines_Get_Rmatrix but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Lines_Get_Rmatrix_GR(void);

    /*! 
    Zero-sequence reactance ohms per unit length.
    */
    ALTDSS_CAPI_DLL double Lines_Get_X0(void);

    /*!
    Reactance matrix (full), ohms per unit length. Array of doubles.
    */
    ALTDSS_CAPI_DLL void Lines_Get_Xmatrix(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Lines_Get_Xmatrix but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Lines_Get_Xmatrix_GR(void);

    /*! 
    Zero-sequence capacitance, nanofarads per unit length.
    */
    ALTDSS_CAPI_DLL void Lines_Set_C0(double Value);

    /*! 
    Positive-sequence capacitance, nanofarads per unit length.
    */
    ALTDSS_CAPI_DLL void Lines_Set_C1(double Value);

    ALTDSS_CAPI_DLL void Lines_Set_Cmatrix(const double* ValuePtr, int32_t ValueCount);

    /*! 
    Zero-sequence resistance, ohms per unit length.
    */
    ALTDSS_CAPI_DLL void Lines_Set_R0(double Value);

    /*! 
    Resistance matrix (full), ohms per unit length. Array of doubles.
    */
    ALTDSS_CAPI_DLL void Lines_Set_Rmatrix(const double* ValuePtr, int32_t ValueCount);

    /*! 
    Zero-sequence reactance ohms per unit length.
    */
    ALTDSS_CAPI_DLL void Lines_Set_X0(double Value);

    ALTDSS_CAPI_DLL void Lines_Set_Xmatrix(const double* ValuePtr, int32_t ValueCount);

    /*! 
    Emergency (maximum) ampere rating of Line.
    */
    ALTDSS_CAPI_DLL double Lines_Get_EmergAmps(void);

    /*! 
    Normal ampere rating of Line.
    */
    ALTDSS_CAPI_DLL double Lines_Get_NormAmps(void);

    /*! 
    Emergency (maximum) ampere rating of Line.
    */
    ALTDSS_CAPI_DLL void Lines_Set_EmergAmps(double Value);

    /*! 
    Normal ampere rating of Line.
    */
    ALTDSS_CAPI_DLL void Lines_Set_NormAmps(double Value);

    /*! 
    Line geometry code
    */
    ALTDSS_CAPI_DLL const char* Lines_Get_Geometry(void);

    /*! 
    Line geometry code
    */
    ALTDSS_CAPI_DLL void Lines_Set_Geometry(const char* Value);

    /*! 
    Earth return resistance value used to compute line impedances at power frequency
    */
    ALTDSS_CAPI_DLL double Lines_Get_Rg(void);

    /*! 
    Earth Resistivity, m-ohms
    */
    ALTDSS_CAPI_DLL double Lines_Get_Rho(void);

    /*! 
    Earth return reactance value used to compute line impedances at power frequency
    */
    ALTDSS_CAPI_DLL double Lines_Get_Xg(void);

    /*! 
    Earth return resistance value used to compute line impedances at power frequency
    */
    ALTDSS_CAPI_DLL void Lines_Set_Rg(double Value);

    /*! 
    Earth Resistivity, m-ohms
    */
    ALTDSS_CAPI_DLL void Lines_Set_Rho(double Value);

    /*! 
    Earth return reactance value used to compute line impedances at power frequency
    */
    ALTDSS_CAPI_DLL void Lines_Set_Xg(double Value);

    /*! 
    Yprimitive for the active line object (complex array).
    */
    ALTDSS_CAPI_DLL void Lines_Get_Yprim(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Lines_Get_Yprim but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Lines_Get_Yprim_GR(void);

    /*! 
    Yprimitive: Does Nothing at present on Put; Dangerous
    */
    ALTDSS_CAPI_DLL void Lines_Set_Yprim(const double* ValuePtr, int32_t ValueCount);

    /*! 
    Number of customers on this line section.
    */
    ALTDSS_CAPI_DLL int32_t Lines_Get_NumCust(void);

    /*! 
    Total Number of customers served from this line section.
    */
    ALTDSS_CAPI_DLL int32_t Lines_Get_TotalCust(void);

    /*! 
    Sets Parent of the active Line to be the active line. Returns 0 if no parent or action fails.
    */
    ALTDSS_CAPI_DLL int32_t Lines_Get_Parent(void);

    /*! 
    Number of Line objects in Active Circuit.
    */
    ALTDSS_CAPI_DLL int32_t Lines_Get_Count(void);

    /*! 
    Line spacing code
    */
    ALTDSS_CAPI_DLL const char* Lines_Get_Spacing(void);

    /*! 
    Line spacing code
    */
    ALTDSS_CAPI_DLL void Lines_Set_Spacing(const char* Value);

    ALTDSS_CAPI_DLL int32_t Lines_Get_Units(void);

    ALTDSS_CAPI_DLL void Lines_Set_Units(int32_t Value);

    /*! 
    Delivers the rating for the current season (in Amps)  if the "SeasonalRatings" option is active
    */
    ALTDSS_CAPI_DLL double Lines_Get_SeasonRating(void);

    /*! 
    Line element switch status. Setting it has side-effects to the line parameters.
    */
    ALTDSS_CAPI_DLL uint16_t Lines_Get_IsSwitch(void);
    ALTDSS_CAPI_DLL void Lines_Set_IsSwitch(uint16_t Value);

    /*! 
    Array of strings containing all Load names
    */
    ALTDSS_CAPI_DLL void Loads_Get_AllNames(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Set first Load element to be active; returns 0 if none.
    */
    ALTDSS_CAPI_DLL int32_t Loads_Get_First(void);

    ALTDSS_CAPI_DLL int32_t Loads_Get_idx(void);

    /*! 
    Set active load by name.
    */
    ALTDSS_CAPI_DLL const char* Loads_Get_Name(void);

    /*! 
    Sets next Load element to be active; returns 0 of none else index of active load.
    */
    ALTDSS_CAPI_DLL int32_t Loads_Get_Next(void);

    ALTDSS_CAPI_DLL void Loads_Set_idx(int32_t Value);

    /*! 
    Set active load by name.
    */
    ALTDSS_CAPI_DLL void Loads_Set_Name(const char* Value);

    /*! 
    Set kV rating for active Load. For 2 or more phases set Line-Line kV. Else actual kV across terminals.
    */
    ALTDSS_CAPI_DLL double Loads_Get_kV(void);

    /*! 
    Set kvar for active Load. Updates PF based in present kW.
    */
    ALTDSS_CAPI_DLL double Loads_Get_kvar(void);

    /*! 
    Set kW for active Load. Updates kvar based on present PF.
    */
    ALTDSS_CAPI_DLL double Loads_Get_kW(void);

    /*! 
    Set Power Factor for Active Load. Specify leading PF as negative. Updates kvar based on kW value
    */
    ALTDSS_CAPI_DLL double Loads_Get_PF(void);

    /*! 
    Set kV rating for active Load. For 2 or more phases set Line-Line kV. Else actual kV across terminals.
    */
    ALTDSS_CAPI_DLL void Loads_Set_kV(double Value);

    /*! 
    Set kvar for active Load. Updates PF based on present kW.
    */
    ALTDSS_CAPI_DLL void Loads_Set_kvar(double Value);

    /*! 
    Set kW for active Load. Updates kvar based on present PF.
    */
    ALTDSS_CAPI_DLL void Loads_Set_kW(double Value);

    /*! 
    Set Power Factor for Active Load. Specify leading PF as negative. Updates kvar based on present value of kW.
    */
    ALTDSS_CAPI_DLL void Loads_Set_PF(double Value);

    /*! 
    Number of Load objects in active circuit.
    */
    ALTDSS_CAPI_DLL int32_t Loads_Get_Count(void);

    /*! 
    Factor for allocating loads by connected xfkva
    */
    ALTDSS_CAPI_DLL double Loads_Get_AllocationFactor(void);

    /*! 
    CFactor relates average to peak kw.  Used for allocation with kwh and kwhdays/
    */
    ALTDSS_CAPI_DLL double Loads_Get_Cfactor(void);

    ALTDSS_CAPI_DLL int32_t Loads_Get_Class_(void);

    /*! 
    Name of a loadshape with both Mult and Qmult, for CVR factors as a function of time.
    */
    ALTDSS_CAPI_DLL const char* Loads_Get_CVRcurve(void);

    /*! 
    Percent reduction in Q for percent reduction in V. Must be used with LoadModelCVR.
    */
    ALTDSS_CAPI_DLL double Loads_Get_CVRvars(void);

    /*! 
    Percent reduction in P for percent reduction in V. Must be used with LoadModelCVR.
    */
    ALTDSS_CAPI_DLL double Loads_Get_CVRwatts(void);

    /*! 
    Name of the loadshape for a daily load profile.
    */
    ALTDSS_CAPI_DLL const char* Loads_Get_daily(void);

    /*! 
    Name of the loadshape for a duty cycle simulation.
    */
    ALTDSS_CAPI_DLL const char* Loads_Get_duty(void);

    /*! 
    Name of the growthshape curve for yearly load growth factors.
    */
    ALTDSS_CAPI_DLL const char* Loads_Get_Growth(void);

    /*! 
    Delta loads are connected line-to-line.
    */
    ALTDSS_CAPI_DLL uint16_t Loads_Get_IsDelta(void);

    /*! 
    Base load kva. Also defined kw and kvar or pf input, or load allocation by kwh or xfkva.
    */
    ALTDSS_CAPI_DLL double Loads_Get_kva(void);

    /*! 
    kwh billed for this period. Can be used with Cfactor for load allocation.
    */
    ALTDSS_CAPI_DLL double Loads_Get_kwh(void);

    /*! 
    Length of kwh billing period for average demand calculation. Default 30.
    */
    ALTDSS_CAPI_DLL double Loads_Get_kwhdays(void);

    /*! 
    The Load Model defines variation of P and Q with voltage.
    */
    ALTDSS_CAPI_DLL int32_t Loads_Get_Model(void);

    /*! 
    Number of customers in this load, defaults to one.
    */
    ALTDSS_CAPI_DLL int32_t Loads_Get_NumCust(void);

    /*! 
    Average percent of nominal load in Monte Carlo studies; only if no loadshape defined for this load.
    */
    ALTDSS_CAPI_DLL double Loads_Get_PctMean(void);

    /*! 
    Percent standard deviation for Monte Carlo load studies; if there is no loadshape assigned to this load.
    */
    ALTDSS_CAPI_DLL double Loads_Get_PctStdDev(void);

    /*! 
    Neutral resistance for wye-connected loads.
    */
    ALTDSS_CAPI_DLL double Loads_Get_Rneut(void);

    /*! 
    Name of harmonic current spectrum shape.
    */
    ALTDSS_CAPI_DLL const char* Loads_Get_Spectrum(void);

    /*! 
    Response to load multipliers: Fixed (growth only), Exempt (no LD curve), Variable (all).
    */
    ALTDSS_CAPI_DLL int32_t Loads_Get_Status(void);

    /*! 
    Maximum per-unit voltage to use the load model. Above this, constant Z applies.
    */
    ALTDSS_CAPI_DLL double Loads_Get_Vmaxpu(void);

    /*! 
    Minimum voltage for unserved energy (UE) evaluation.
    */
    ALTDSS_CAPI_DLL double Loads_Get_Vminemerg(void);

    /*! 
    Minimum voltage for energy exceeding normal (EEN) evaluations.
    */
    ALTDSS_CAPI_DLL double Loads_Get_Vminnorm(void);

    /*! 
    Minimum voltage to apply the load model. Below this, constant Z is used.
    */
    ALTDSS_CAPI_DLL double Loads_Get_Vminpu(void);

    /*! 
    Rated service transformer kVA for load allocation, using AllocationFactor. Affects kW, kvar, and pf.
    */
    ALTDSS_CAPI_DLL double Loads_Get_xfkVA(void);

    /*! 
    Neutral reactance for wye-connected loads.
    */
    ALTDSS_CAPI_DLL double Loads_Get_Xneut(void);

    /*! 
    Name of yearly duration loadshape
    */
    ALTDSS_CAPI_DLL const char* Loads_Get_Yearly(void);

    ALTDSS_CAPI_DLL void Loads_Set_AllocationFactor(double Value);

    ALTDSS_CAPI_DLL void Loads_Set_Cfactor(double Value);

    ALTDSS_CAPI_DLL void Loads_Set_Class_(int32_t Value);

    ALTDSS_CAPI_DLL void Loads_Set_CVRcurve(const char* Value);

    ALTDSS_CAPI_DLL void Loads_Set_CVRvars(double Value);

    ALTDSS_CAPI_DLL void Loads_Set_CVRwatts(double Value);

    ALTDSS_CAPI_DLL void Loads_Set_daily(const char* Value);

    ALTDSS_CAPI_DLL void Loads_Set_duty(const char* Value);

    ALTDSS_CAPI_DLL void Loads_Set_Growth(const char* Value);

    ALTDSS_CAPI_DLL void Loads_Set_IsDelta(uint16_t Value);

    ALTDSS_CAPI_DLL void Loads_Set_kva(double Value);

    ALTDSS_CAPI_DLL void Loads_Set_kwh(double Value);

    ALTDSS_CAPI_DLL void Loads_Set_kwhdays(double Value);

    ALTDSS_CAPI_DLL void Loads_Set_Model(int32_t Value);

    ALTDSS_CAPI_DLL void Loads_Set_NumCust(int32_t Value);

    ALTDSS_CAPI_DLL void Loads_Set_PctMean(double Value);

    ALTDSS_CAPI_DLL void Loads_Set_PctStdDev(double Value);

    ALTDSS_CAPI_DLL void Loads_Set_Rneut(double Value);

    ALTDSS_CAPI_DLL void Loads_Set_Spectrum(const char* Value);

    ALTDSS_CAPI_DLL void Loads_Set_Status(int32_t Value);

    ALTDSS_CAPI_DLL void Loads_Set_Vmaxpu(double Value);

    ALTDSS_CAPI_DLL void Loads_Set_Vminemerg(double Value);

    ALTDSS_CAPI_DLL void Loads_Set_Vminnorm(double Value);

    ALTDSS_CAPI_DLL void Loads_Set_Vminpu(double Value);

    ALTDSS_CAPI_DLL void Loads_Set_xfkVA(double Value);

    ALTDSS_CAPI_DLL void Loads_Set_Xneut(double Value);

    ALTDSS_CAPI_DLL void Loads_Set_Yearly(const char* Value);

    /*! 
    Array of 7 doubles with values for ZIPV property of the LOAD object
    */
    ALTDSS_CAPI_DLL void Loads_Get_ZIPV(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Loads_Get_ZIPV but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Loads_Get_ZIPV_GR(void);

    ALTDSS_CAPI_DLL void Loads_Set_ZIPV(const double* ValuePtr, int32_t ValueCount);

    /*! 
    Percent of Load that is modeled as series R-L for harmonics studies
    */
    ALTDSS_CAPI_DLL double Loads_Get_pctSeriesRL(void);

    /*! 
    Percent of Load that is modeled as series R-L for harmonics studies
    */
    ALTDSS_CAPI_DLL void Loads_Set_pctSeriesRL(double Value);

    /*! 
    Relative Weighting factor for the active load.

    This value is used in reliability methods.
    */
    ALTDSS_CAPI_DLL double Loads_Get_RelWeight(void);

    /*! 
    Relative Weighting factor for the active load.

    This value is used in reliability methods.
    */
    ALTDSS_CAPI_DLL void Loads_Set_RelWeight(double Value);

    /*! 
    Name of the sensor monitoring this load.
    */
    ALTDSS_CAPI_DLL const char* Loads_Get_Sensor(void);

    /*! 
    Get the Name of the active LoadShape
    */
    ALTDSS_CAPI_DLL const char* LoadShapes_Get_Name(void);

    /*! 
    Set the active LoadShape by name
    */
    ALTDSS_CAPI_DLL void LoadShapes_Set_Name(const char* Value);

    /*! 
    Number of LoadShape objects currently defined in the LoadShape collection
    */
    ALTDSS_CAPI_DLL int32_t LoadShapes_Get_Count(void);

    /*! 
    Set the first loadshape active and return integer index of the loadshape. Returns 0 if none.
    */
    ALTDSS_CAPI_DLL int32_t LoadShapes_Get_First(void);

    /*! 
    Advance active LoadShape to the next on in the collection. Returns 0 if no more loadshapes.
    */
    ALTDSS_CAPI_DLL int32_t LoadShapes_Get_Next(void);

    /*! 
    Array of strings containing names of all LoadShape objects currently defined.
    */
    ALTDSS_CAPI_DLL void LoadShapes_Get_AllNames(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Get Number of points in active LoadShape.
    */
    ALTDSS_CAPI_DLL int32_t LoadShapes_Get_Npts(void);

    /*! 
    Array of Doubles for the P multiplier in the LoadShape.
    */
    ALTDSS_CAPI_DLL void LoadShapes_Get_Pmult(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as LoadShapes_Get_Pmult but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void LoadShapes_Get_Pmult_GR(void);

    /*! 
    Array of doubles containing the Q multipliers.
    */
    ALTDSS_CAPI_DLL void LoadShapes_Get_Qmult(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as LoadShapes_Get_Qmult but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void LoadShapes_Get_Qmult_GR(void);

    /*! 
    Set number of points to allocate for active LoadShape.
    */
    ALTDSS_CAPI_DLL void LoadShapes_Set_Npts(int32_t Value);

    /*! 
    Array of doubles containing the P array for the LoadShape.
    */
    ALTDSS_CAPI_DLL void LoadShapes_Set_Pmult(const double* ValuePtr, int32_t ValueCount);

    /*! 
    Array of doubles containing the Q multipliers.
    */
    ALTDSS_CAPI_DLL void LoadShapes_Set_Qmult(const double* ValuePtr, int32_t ValueCount);

    /*!
    Normalize the LoadShape data inplace
    */
    ALTDSS_CAPI_DLL void LoadShapes_Normalize(void);

    /*! 
    Time array in hours corresponding to P and Q multipliers when the Interval=0.
    */
    ALTDSS_CAPI_DLL void LoadShapes_Get_TimeArray(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as LoadShapes_Get_TimeArray but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void LoadShapes_Get_TimeArray_GR(void);

    /*! 
    Time array in hours corresponding to P and Q multipliers when the Interval=0.
    */
    ALTDSS_CAPI_DLL void LoadShapes_Set_TimeArray(const double* ValuePtr, int32_t ValueCount);

    /*! 
    Fixed interval time value, in hours
    */
    ALTDSS_CAPI_DLL double LoadShapes_Get_HrInterval(void);

    /*! 
    Fixed Interval time value, in minutes
    */
    ALTDSS_CAPI_DLL double LoadShapes_Get_MinInterval(void);

    /*! 
    Fixed interval data time interval, in seconds
    */
    ALTDSS_CAPI_DLL double LoadShapes_Get_SInterval(void);

    /*! 
    Fixed interval data time interval, in seconds
    */
    ALTDSS_CAPI_DLL void LoadShapes_Set_SInterval(double Value);

    /*! 
    Fixed interval time value, in hours.
    */
    ALTDSS_CAPI_DLL void LoadShapes_Set_HrInterval(double Value);

    /*! 
    Fixed Interval time value, in minutes
    */
    ALTDSS_CAPI_DLL void LoadShapes_Set_MinInterval(double Value);


    ALTDSS_CAPI_DLL int32_t LoadShapes_New(const char* Name);

    /*!
    Base P value for normalization. Default is zero, meaning the peak will be used.    
    */
    ALTDSS_CAPI_DLL double LoadShapes_Get_PBase(void);

    /*! 
    Base for normalizing Q curve. If left at zero, the peak value is used.
    */
    ALTDSS_CAPI_DLL double LoadShapes_Get_Qbase(void);

    /*!
    Base P value for normalization. Default is zero, meaning the peak will be used.    
    */
    ALTDSS_CAPI_DLL void LoadShapes_Set_PBase(double Value);

    /*! 
    Base for normalizing Q curve. If left at zero, the peak value is used.
    */
    ALTDSS_CAPI_DLL void LoadShapes_Set_Qbase(double Value);

    /*! 
    T/F flag to let Loads know to use the actual value in the curve rather than use the value as a multiplier.
    */
    ALTDSS_CAPI_DLL uint16_t LoadShapes_Get_UseActual(void);

    /*! 
    T/F flag to let Loads know to use the actual value in the curve rather than use the value as a multiplier.
    */
    ALTDSS_CAPI_DLL void LoadShapes_Set_UseActual(uint16_t Value);

    /*! 
    Array of all energy Meter names
    */
    ALTDSS_CAPI_DLL void Meters_Get_AllNames(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Set the first energy Meter active. Returns 0 if none.
    */
    ALTDSS_CAPI_DLL int32_t Meters_Get_First(void);

    /*! 
    Get/Set the active meter  name.
    */
    ALTDSS_CAPI_DLL const char* Meters_Get_Name(void);

    /*! 
    Sets the next energy Meter active.  Returns 0 if no more.
    */
    ALTDSS_CAPI_DLL int32_t Meters_Get_Next(void);

    /*! 
    Array of strings containing the names of the registers.
    */
    ALTDSS_CAPI_DLL void Meters_Get_RegisterNames(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Array of all the values contained in the Meter registers for the active Meter.
    */
    ALTDSS_CAPI_DLL void Meters_Get_RegisterValues(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Meters_Get_RegisterValues but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Meters_Get_RegisterValues_GR(void);

    ALTDSS_CAPI_DLL void Meters_Reset(void);

    ALTDSS_CAPI_DLL void Meters_ResetAll(void);

    ALTDSS_CAPI_DLL void Meters_Sample(void);

    ALTDSS_CAPI_DLL void Meters_Save(void);

    /*! 
    Set a meter to be active by name.
    */
    ALTDSS_CAPI_DLL void Meters_Set_Name(const char* Value);

    /*! 
    Totals of all registers of all meters
    */
    ALTDSS_CAPI_DLL void Meters_Get_Totals(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Meters_Get_Totals but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Meters_Get_Totals_GR(void);

    /*! 
    Array of doubles to set values of Peak Current property
    */
    ALTDSS_CAPI_DLL void Meters_Get_Peakcurrent(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Meters_Get_Peakcurrent but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Meters_Get_Peakcurrent_GR(void);

    /*! 
    Array of doubles to set values of Peak Current property
    */
    ALTDSS_CAPI_DLL void Meters_Set_Peakcurrent(const double* ValuePtr, int32_t ValueCount);

    /*! 
    Set the magnitude of the real part of the Calculated Current (normally determined by solution) for the Meter to force some behavior on Load Allocation
    */
    ALTDSS_CAPI_DLL void Meters_Get_CalcCurrent(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Meters_Get_CalcCurrent but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Meters_Get_CalcCurrent_GR(void);

    /*! 
    Set the magnitude of the real part of the Calculated Current (normally determined by solution) for the Meter to force some behavior on Load Allocation
    */
    ALTDSS_CAPI_DLL void Meters_Set_CalcCurrent(const double* ValuePtr, int32_t ValueCount);

    /*! 
    Array of doubles: set the phase allocation factors for the active meter.
    */
    ALTDSS_CAPI_DLL void Meters_Get_AllocFactors(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Meters_Get_AllocFactors but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Meters_Get_AllocFactors_GR(void);

    /*! 
    Array of doubles: set the phase allocation factors for the active meter.
    */
    ALTDSS_CAPI_DLL void Meters_Set_AllocFactors(const double* ValuePtr, int32_t ValueCount);

    /*! 
    Set Name of metered element
    */
    ALTDSS_CAPI_DLL const char* Meters_Get_MeteredElement(void);

    /*! 
    set Number of Metered Terminal
    */
    ALTDSS_CAPI_DLL int32_t Meters_Get_MeteredTerminal(void);

    /*! 
    Set Name of metered element
    */
    ALTDSS_CAPI_DLL void Meters_Set_MeteredElement(const char* Value);

    /*! 
    set Number of Metered Terminal
    */
    ALTDSS_CAPI_DLL void Meters_Set_MeteredTerminal(int32_t Value);

    /*! 
    Global Flag in the DSS to indicate if Demand Interval (DI) files have been properly opened.
    */
    ALTDSS_CAPI_DLL uint16_t Meters_Get_DIFilesAreOpen(void);

    ALTDSS_CAPI_DLL void Meters_CloseAllDIFiles(void);

    ALTDSS_CAPI_DLL void Meters_OpenAllDIFiles(void);

    ALTDSS_CAPI_DLL void Meters_SampleAll(void);

    ALTDSS_CAPI_DLL void Meters_SaveAll(void);

    /*! 
    Array of names of all zone end elements.
    */
    ALTDSS_CAPI_DLL void Meters_Get_AllEndElements(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Number of zone end elements in the active meter zone.
    */
    ALTDSS_CAPI_DLL int32_t Meters_Get_CountEndElements(void);

    /*! 
    Number of Energy Meters in the Active Circuit
    */
    ALTDSS_CAPI_DLL int32_t Meters_Get_Count(void);

    /*! 
    Wide string list of all branches in zone of the active energymeter object.
    */
    ALTDSS_CAPI_DLL void Meters_Get_AllBranchesInZone(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Number of branches in Active Energy Meter zone. (Same as sequence list size)
    */
    ALTDSS_CAPI_DLL int32_t Meters_Get_CountBranches(void);

    /*! 
    Returns SAIFI for this meter's Zone. Execute Reliability Calc method first.
    */
    ALTDSS_CAPI_DLL double Meters_Get_SAIFI(void);

    /*! 
    Get/set Index into Meter's SequenceList that contains branch pointers in lexical order. Earlier index guaranteed to be upline from later index. Sets PDelement active.
    */
    ALTDSS_CAPI_DLL int32_t Meters_Get_SequenceIndex(void);

    /*! 
    Get/set Index into Meter's SequenceList that contains branch pointers in lexical order. Earlier index guaranteed to be upline from later index. Sets PDelement active.
    */
    ALTDSS_CAPI_DLL void Meters_Set_SequenceIndex(int32_t Value);

    /*! 
    SAIFI based on kW rather than number of customers. Get after reliability calcs.
    */
    ALTDSS_CAPI_DLL double Meters_Get_SAIFIKW(void);

    ALTDSS_CAPI_DLL void Meters_DoReliabilityCalc(uint16_t AssumeRestoration);

    /*! 
    Size of Sequence List
    */
    ALTDSS_CAPI_DLL int32_t Meters_Get_SeqListSize(void);

    /*! 
    Total Number of customers in this zone (downline from the EnergyMeter)
    */
    ALTDSS_CAPI_DLL int32_t Meters_Get_TotalCustomers(void);

    /*! 
    SAIDI for this meter's zone. Execute DoReliabilityCalc first.
    */
    ALTDSS_CAPI_DLL double Meters_Get_SAIDI(void);

    /*! 
    Total customer interruptions for this Meter zone based on reliability calcs.
    */
    ALTDSS_CAPI_DLL double Meters_Get_CustInterrupts(void);

    /*! 
    Number of feeder sections in this meter's zone
    */
    ALTDSS_CAPI_DLL int32_t Meters_Get_NumSections(void);

    ALTDSS_CAPI_DLL void Meters_SetActiveSection(int32_t SectIdx);

    /*! 
    Average Repair time in this section of the meter zone
    */
    ALTDSS_CAPI_DLL double Meters_Get_AvgRepairTime(void);

    /*! 
    Sum of Fault Rate time Repair Hrs in this section of the meter zone
    */
    ALTDSS_CAPI_DLL double Meters_Get_FaultRateXRepairHrs(void);

    /*! 
    Number of branches (lines) in this section
    */
    ALTDSS_CAPI_DLL int32_t Meters_Get_NumSectionBranches(void);

    /*! 
    Number of Customers in the active section.
    */
    ALTDSS_CAPI_DLL int32_t Meters_Get_NumSectionCustomers(void);

    /*! 
    Type of OCP device. 1=Fuse; 2=Recloser; 3=Relay
    */
    ALTDSS_CAPI_DLL int32_t Meters_Get_OCPDeviceType(void);

    /*! 
    Sum of the branch fault rates in this section of the meter's zone
    */
    ALTDSS_CAPI_DLL double Meters_Get_SumBranchFltRates(void);

    /*! 
    SequenceIndex of the branch at the head of this section
    */
    ALTDSS_CAPI_DLL int32_t Meters_Get_SectSeqIdx(void);

    /*! 
    Total Customers downline from this section
    */
    ALTDSS_CAPI_DLL int32_t Meters_Get_SectTotalCust(void);
    
    /*! 
    Returns the list of all PCE within the area covered by the energy meter
    */
    ALTDSS_CAPI_DLL void Meters_Get_ZonePCE(char*** ResultPtr, int32_t* ResultDims);
    

    /*! 
    Array of all Monitor Names
    */
    ALTDSS_CAPI_DLL void Monitors_Get_AllNames(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Name of CSV file associated with active Monitor.
    */
    ALTDSS_CAPI_DLL const char* Monitors_Get_FileName(void);

    /*! 
    Sets the first Monitor active.  Returns 0 if no monitors.
    */
    ALTDSS_CAPI_DLL int32_t Monitors_Get_First(void);

    /*! 
    Monitor mode (bitmask integer - see DSS Help)
    */
    ALTDSS_CAPI_DLL int32_t Monitors_Get_Mode(void);

    /*! 
    Sets the active Monitor object by name
    */
    ALTDSS_CAPI_DLL const char* Monitors_Get_Name(void);

    /*! 
    Sets next monitor active.  Returns 0 if no more.
    */
    ALTDSS_CAPI_DLL int32_t Monitors_Get_Next(void);

    /*!
    Reset active Monitor object.
    */
    ALTDSS_CAPI_DLL void Monitors_Reset(void);

    /*!
    Reset all Monitor objects.
    */
    ALTDSS_CAPI_DLL void Monitors_ResetAll(void);

    /*!
    Instruct the active Monitor to take a sample of the present state.
    */
    ALTDSS_CAPI_DLL void Monitors_Sample(void);

    /*!
    Instructs the active monitor to save its current sample buffer to its monitor stream.

    After the data is on the stream, you can access the ByteStream or channel data.

    **Most standard solution modes do this automatically.**
    */
    ALTDSS_CAPI_DLL void Monitors_Save(void);

    /*! 
    Set Monitor mode (bitmask integer - see DSS Help)
    */
    ALTDSS_CAPI_DLL void Monitors_Set_Mode(int32_t Value);

    /*!
    Convert the monitor data to text and displays it with the text editor.
    */
    ALTDSS_CAPI_DLL void Monitors_Show(void);

    /*! 
    Sets the active Monitor object by name
    */
    ALTDSS_CAPI_DLL void Monitors_Set_Name(const char* Value);

    /*! 
    Byte Array containing monitor stream values. Make sure a "save" is done first (standard solution modes do this automatically)
    */
    ALTDSS_CAPI_DLL void Monitors_Get_ByteStream(int8_t** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Monitors_Get_ByteStream but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Monitors_Get_ByteStream_GR(void);

    /*! 
    Number of Samples in Monitor at Present
    */
    ALTDSS_CAPI_DLL int32_t Monitors_Get_SampleCount(void);

    /*!
    Instruct all Monitor objects to take a sample of the present state.    
    */
    ALTDSS_CAPI_DLL void Monitors_SampleAll(void);

    /*!
    Instructs the all monitor objects to save their current sample buffers to the respective monitor streams.
    
    **Most standard solution modes do this automatically.**
    */
    ALTDSS_CAPI_DLL void Monitors_SaveAll(void);

    /*! 
    Number of Monitors
    */
    ALTDSS_CAPI_DLL int32_t Monitors_Get_Count(void);

    /*!
    Post-process monitor samples taken so far, e.g., Pst for mode=4.
    */
    ALTDSS_CAPI_DLL void Monitors_Process(void);

    /*!
    Post-process all monitor samples taken so far, e.g., Pst for mode=4.
    */
    ALTDSS_CAPI_DLL void Monitors_ProcessAll(void);

    /*! 
    Array of doubles for the specified channel  
    
    usage: `MyArray = DSSMonitor.Channel(i)`
    
    A `Save` or `SaveAll` should be executed first, which is done automatically by most standard solution modes.

    Note: it might be beneficial to use process the ByteStream directly in your programming language. For example,
    see the DSS-Python's implementation for `Channel` and `AsMatrix`.
    */
    ALTDSS_CAPI_DLL void Monitors_Get_Channel(double** ResultPtr, int32_t* ResultDims, int32_t Index);
    /*! 
    Same as Monitors_Get_Channel but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Monitors_Get_Channel_GR(int32_t Index);

    /*! 
    Array of doubles containing frequency values for harmonics mode solutions; Empty for time mode solutions (use dblHour)
    */
    ALTDSS_CAPI_DLL void Monitors_Get_dblFreq(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Monitors_Get_dblFreq but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Monitors_Get_dblFreq_GR(void);

    /*! 
    Array of doubles containing time value in hours for time-sampled monitor values; Empty if frequency-sampled values for harmonics solution  (see dblFreq)
    */
    ALTDSS_CAPI_DLL void Monitors_Get_dblHour(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Monitors_Get_dblHour but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Monitors_Get_dblHour_GR(void);

    /*! 
    Monitor File Version (integer)
    */
    ALTDSS_CAPI_DLL int32_t Monitors_Get_FileVersion(void);

    /*! 
    Header string;  Array of strings containing Channel names
    */
    ALTDSS_CAPI_DLL void Monitors_Get_Header(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Number of Channels in the active Monitor
    */
    ALTDSS_CAPI_DLL int32_t Monitors_Get_NumChannels(void);

    /*! 
    Size of each record in ByteStream (Integer). Same as NumChannels.
    */
    ALTDSS_CAPI_DLL int32_t Monitors_Get_RecordSize(void);

    /*! 
    Full object name of element being monitored.
    */
    ALTDSS_CAPI_DLL const char* Monitors_Get_Element(void);

    /*! 
    Full object name of element being monitored.
    */
    ALTDSS_CAPI_DLL void Monitors_Set_Element(const char* Value);

    /*! 
    Terminal number of element being monitored
    */
    ALTDSS_CAPI_DLL int32_t Monitors_Get_Terminal(void);

    /*! 
    Terminal number of element being monitored.
    */
    ALTDSS_CAPI_DLL void Monitors_Set_Terminal(int32_t Value);

    /*! 
    Delivers the number of CPUs on the current PC
    */
    ALTDSS_CAPI_DLL int32_t Parallel_Get_NumCPUs(void);

    /*! 
    Delivers the number of Cores of the local PC
    */
    ALTDSS_CAPI_DLL int32_t Parallel_Get_NumCores(void);

    /*! 
    Gets the ID of the Active Actor
    */
    ALTDSS_CAPI_DLL int32_t Parallel_Get_ActiveActor(void);

    /*! 
    Sets the Active Actor
    */
    ALTDSS_CAPI_DLL void Parallel_Set_ActiveActor(int32_t Value);

    /*!
    Create a new actor, if there are still cores available.    
    */
    ALTDSS_CAPI_DLL void Parallel_CreateActor(void);

    /*! 
    Gets the CPU of the Active Actor
    */
    ALTDSS_CAPI_DLL int32_t Parallel_Get_ActorCPU(void);

    /*! 
    Sets the CPU for the Active Actor
    */
    ALTDSS_CAPI_DLL void Parallel_Set_ActorCPU(int32_t Value);

    /*! 
    Gets the number of Actors created
    */
    ALTDSS_CAPI_DLL int32_t Parallel_Get_NumOfActors(void);

    /*!
    Suspends the host's thread until all the OpenDSS running jobs finish.
    */
    ALTDSS_CAPI_DLL void Parallel_Wait(void);

    /*! 
    Gets the progress of all existing actors in pct
    */
    ALTDSS_CAPI_DLL void Parallel_Get_ActorProgress(int32_t** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Parallel_Get_ActorProgress but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Parallel_Get_ActorProgress_GR(void);

    /*! 
    Gets the status of each actor
    */
    ALTDSS_CAPI_DLL void Parallel_Get_ActorStatus(int32_t** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Parallel_Get_ActorStatus but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Parallel_Get_ActorStatus_GR(void);

    /*! 
    Sets ON/OFF (1/0) Parallel features of the Engine
    */
    ALTDSS_CAPI_DLL int32_t Parallel_Get_ActiveParallel(void);

    /*! 
    Delivers if the Parallel features of the Engine are Active
    */
    ALTDSS_CAPI_DLL void Parallel_Set_ActiveParallel(int32_t Value);

    /*! 
    Reads the values of the ConcatenateReports option (1=enabled, 0=disabled)
    */
    ALTDSS_CAPI_DLL int32_t Parallel_Get_ConcatenateReports(void);

    /*! 
    Enable/Disable (1/0) the ConcatenateReports option for extracting monitors data
    */
    ALTDSS_CAPI_DLL void Parallel_Set_ConcatenateReports(int32_t Value);

    /*! 
    String to be parsed. Loading this string resets the Parser to the beginning of the line. Then parse off the tokens in sequence.
    */
    ALTDSS_CAPI_DLL const char* Parser_Get_CmdString(void);

    /*! 
    String to be parsed. Loading this string resets the Parser to the beginning of the line. Then parse off the tokens in sequence.
    */
    ALTDSS_CAPI_DLL void Parser_Set_CmdString(const char* Value);

    /*! 
    Get next token and return tag name (before = sign) if any. See AutoIncrement.
    */
    ALTDSS_CAPI_DLL const char* Parser_Get_NextParam(void);

    /*! 
    Default is FALSE. If TRUE parser automatically advances to next token after DblValue, IntValue, or StrValue. Simpler when you don't need to check for parameter names.
    */
    ALTDSS_CAPI_DLL uint16_t Parser_Get_AutoIncrement(void);

    /*! 
    Default is FALSE. If TRUE parser automatically advances to next token after DblValue, IntValue, or StrValue. Simpler when you don't need to check for parameter names.
    */
    ALTDSS_CAPI_DLL void Parser_Set_AutoIncrement(uint16_t Value);

    /*! 
    Return next parameter as a double.
    */
    ALTDSS_CAPI_DLL double Parser_Get_DblValue(void);

    /*! 
    Return next parameter as a long integer.
    */
    ALTDSS_CAPI_DLL int32_t Parser_Get_IntValue(void);

    /*! 
    Return next parameter as a string
    */
    ALTDSS_CAPI_DLL const char* Parser_Get_StrValue(void);

    /*! 
    Get the characters used for White space in the command string.  Default is blank and Tab.
    */
    ALTDSS_CAPI_DLL const char* Parser_Get_WhiteSpace(void);

    /*! 
    Set the characters used for White space in the command string.  Default is blank and Tab.
    */
    ALTDSS_CAPI_DLL void Parser_Set_WhiteSpace(const char* Value);

    /*! 
    Get String containing the the characters for Quoting in OpenDSS scripts. Matching pairs defined in EndQuote. Default is "'([{.
    */
    ALTDSS_CAPI_DLL const char* Parser_Get_BeginQuote(void);

    /*! 
    String containing characters, in order, that match the beginning quote characters in BeginQuote. Default is "')]}
    */
    ALTDSS_CAPI_DLL const char* Parser_Get_EndQuote(void);

    /*! 
    Set String containing the the characters for Quoting in OpenDSS scripts. Matching pairs defined in EndQuote. Default is "'([{.
    */
    ALTDSS_CAPI_DLL void Parser_Set_BeginQuote(const char* Value);

    /*! 
    String containing characters, in order, that match the beginning quote characters in BeginQuote. Default is "')]}
    */
    ALTDSS_CAPI_DLL void Parser_Set_EndQuote(const char* Value);

    /*! 
    String defining hard delimiters used to separate token on the command string. Default is , and =. The = separates token name from token value. These override whitespace to separate tokens.
    */
    ALTDSS_CAPI_DLL const char* Parser_Get_Delimiters(void);

    /*! 
    String defining hard delimiters used to separate token on the command string. Default is , and =. The = separates token name from token value. These override whitespace to separate tokens.
    */
    ALTDSS_CAPI_DLL void Parser_Set_Delimiters(const char* Value);

    ALTDSS_CAPI_DLL void Parser_ResetDelimiters(void);

    /*! 
    Returns token as array of doubles. For parsing quoted array syntax.
    */
    ALTDSS_CAPI_DLL void Parser_Get_Vector(double** ResultPtr, int32_t* ResultDims, int32_t ExpectedSize);
    /*! 
    Same as Parser_Get_Vector but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Parser_Get_Vector_GR(int32_t ExpectedSize);

    /*! 
    Use this property to parse a Matrix token in OpenDSS format.  Returns square matrix of order specified. Order same as default Fortran order: column by column.
    */
    ALTDSS_CAPI_DLL void Parser_Get_Matrix(double** ResultPtr, int32_t* ResultDims, int32_t ExpectedOrder);
    /*! 
    Same as Parser_Get_Matrix but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Parser_Get_Matrix_GR(int32_t ExpectedOrder);

    /*! 
    Use this property to parse a matrix token specified in lower triangle form. Symmetry is forced.
    */
    ALTDSS_CAPI_DLL void Parser_Get_SymMatrix(double** ResultPtr, int32_t* ResultDims, int32_t ExpectedOrder);
    /*! 
    Same as Parser_Get_SymMatrix but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Parser_Get_SymMatrix_GR(int32_t ExpectedOrder);

    /*! 
    Number of PD elements (including disabled elements)
    */
    ALTDSS_CAPI_DLL int32_t PDElements_Get_Count(void);

    /*! 
    Get/Set Number of failures per year. For LINE elements: Number of failures per unit length per year.
    */
    ALTDSS_CAPI_DLL double PDElements_Get_FaultRate(void);

    /*! 
    Set the first enabled PD element to be the active element.  Returns 0 if none found.
    */
    ALTDSS_CAPI_DLL int32_t PDElements_Get_First(void);

    /*! 
    Boolean indicating of PD element should be treated as a shunt element rather than a series element. Applies to Capacitor and Reactor elements in particular.
    */
    ALTDSS_CAPI_DLL uint16_t PDElements_Get_IsShunt(void);

    /*! 
    Advance to the next PD element in the circuit. Enabled elements only. Returns 0 when no more elements.
    */
    ALTDSS_CAPI_DLL int32_t PDElements_Get_Next(void);

    /*! 
    Get/Set percent of faults that are permanent (require repair). Otherwise, fault is assumed to be transient/temporary.
    */
    ALTDSS_CAPI_DLL double PDElements_Get_pctPermanent(void);

    ALTDSS_CAPI_DLL void PDElements_Set_FaultRate(double Value);

    ALTDSS_CAPI_DLL void PDElements_Set_pctPermanent(double Value);

    /*! 
    Get/Set name of active PD Element. Returns null string if active element is not PDElement type.
    */
    ALTDSS_CAPI_DLL const char* PDElements_Get_Name(void);

    ALTDSS_CAPI_DLL void PDElements_Set_Name(const char* Value);

    /*! 
    Accumulated failure rate for this branch on downline
    */
    ALTDSS_CAPI_DLL double PDElements_Get_AccumulatedL(void);

    /*! 
    Failure rate for this branch. Faults per year including length of line.
    */
    ALTDSS_CAPI_DLL double PDElements_Get_Lambda(void);

    /*! 
    Number of customers, this branch
    */
    ALTDSS_CAPI_DLL int32_t PDElements_Get_Numcustomers(void);

    /*! 
    Sets the parent PD element to be the active circuit element.  Returns 0 if no more elements upline.
    */
    ALTDSS_CAPI_DLL int32_t PDElements_Get_ParentPDElement(void);

    /*! 
    Average repair time for this element in hours
    */
    ALTDSS_CAPI_DLL double PDElements_Get_RepairTime(void);

    /*! 
    Total number of customers from this branch to the end of the zone
    */
    ALTDSS_CAPI_DLL int32_t PDElements_Get_Totalcustomers(void);

    /*! 
    Number of the terminal of active PD element that is on the "from" side. This is set after the meter zone is determined.
    */
    ALTDSS_CAPI_DLL int32_t PDElements_Get_FromTerminal(void);

    /*! 
    Total miles of line from this element to the end of the zone. For recloser siting algorithm.
    */
    ALTDSS_CAPI_DLL double PDElements_Get_TotalMiles(void);

    /*! 
    Integer ID of the feeder section that this PDElement branch is part of
    */
    ALTDSS_CAPI_DLL int32_t PDElements_Get_SectionID(void);

    /*! 
    Average repair time for this element in hours
    */
    ALTDSS_CAPI_DLL void PDElements_Set_RepairTime(double Value);

    /*! 
    Returns the current object index in the PDElements list (1-based)

    (API Extension)
    */
    ALTDSS_CAPI_DLL int32_t PDElements_Get_idx(void);

    /*! 
    Activate an object in the PDElements list by index (1-based)

    (API Extension)
    */
    ALTDSS_CAPI_DLL void PDElements_Set_idx(int32_t Value);

    /*! 
    Array of strings consisting of all PD element names.
    
    (API Extension)
    */
    ALTDSS_CAPI_DLL void PDElements_Get_AllNames(char*** ResultPtr, int32_t* ResultDims);


    /*! 
    Array of doubles with the maximum current across the conductors, for each PD 
    element.
    
    By default, only the *first terminal* is used for the maximum current, matching
    the behavior of the "export capacity" command. Pass `AllNodes=True` to 
    force the analysis to all terminals.
    
    See also: 
    https://sourceforge.net/p/electricdss/discussion/beginners/thread/da5b93ca/
    
    (API Extension)
    */
    ALTDSS_CAPI_DLL void PDElements_Get_AllMaxCurrents(double** ResultPtr, int32_t* ResultDims, uint16_t AllNodes);

    /*! 
    Same as PDElements_Get_AllMaxCurrents but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void PDElements_Get_AllMaxCurrents_GR(uint16_t AllNodes);


    /*! 
    Array of doubles with the maximum current across the conductors as a percentage 
    of the Normal Ampere Rating, for each PD element.

    By default, only the *first terminal* is used for the maximum current, matching
    the behavior of the "export capacity" command. Pass `AllNodes=True` to 
    force the analysis to all terminals.
    
    See also: 
    https://sourceforge.net/p/electricdss/discussion/beginners/thread/da5b93ca/
    
    (API Extension)
    */
    ALTDSS_CAPI_DLL void PDElements_Get_AllPctNorm(double** ResultPtr, int32_t* ResultDims, uint16_t AllNodes);
    /*! 
    Same as PDElements_Get_AllPctNorm but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void PDElements_Get_AllPctNorm_GR(uint16_t AllNodes);


    /*! 
    Array of doubles with the maximum current across the conductors as a percentage
    of the Emergency Ampere Rating, for each PD element.

    By default, only the *first terminal* is used for the maximum current, matching
    the behavior of the "export capacity" command. Pass `AllNodes=True` to 
    force the analysis to all terminals.
    
    See also: 
    https://sourceforge.net/p/electricdss/discussion/beginners/thread/da5b93ca/
    
    (API Extension)
    */
    ALTDSS_CAPI_DLL void PDElements_Get_AllPctEmerg(double** ResultPtr, int32_t* ResultDims, uint16_t AllNodes);

    /*! 
    Same as PDElements_Get_AllPctEmerg but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void PDElements_Get_AllPctEmerg_GR(uint16_t AllNodes);


    /*! 
    Complex array of currents for all conductors, all terminals, for each PD element.
    
    (API Extension)
    */
    ALTDSS_CAPI_DLL void PDElements_Get_AllCurrents(double** ResultPtr, int32_t* ResultDims);

    /*! 
    Same as PDElements_Get_AllCurrents but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void PDElements_Get_AllCurrents_GR(void);


    /*! 
    Complex array (magnitude and angle format) of currents for all conductors, all terminals, for each PD element.
    
    (API Extension)
    */
    ALTDSS_CAPI_DLL void PDElements_Get_AllCurrentsMagAng(double** ResultPtr, int32_t* ResultDims);

    /*! 
    Same as PDElements_Get_AllCurrentsMagAng but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void PDElements_Get_AllCurrentsMagAng_GR(void);


    /*! 
    Complex double array of Sequence Currents for all conductors of all terminals, for each PD elements.

    (API Extension)
    */
    ALTDSS_CAPI_DLL void PDElements_Get_AllCplxSeqCurrents(double** ResultPtr, int32_t* ResultDims);

    /*! 
    Same as PDElements_Get_AllCplxSeqCurrents but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void PDElements_Get_AllCplxSeqCurrents_GR(void);


    /*! 
    Double array of the symmetrical component currents (magnitudes only) into each 3-phase terminal, for each PD element.
    
    (API Extension)
    */
    ALTDSS_CAPI_DLL void PDElements_Get_AllSeqCurrents(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as PDElements_Get_AllSeqCurrents but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void PDElements_Get_AllSeqCurrents_GR(void);


    /*! 
    Complex array of powers into each conductor of each terminal, for each PD element.
    
    (API Extension)
    */
    ALTDSS_CAPI_DLL void PDElements_Get_AllPowers(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as PDElements_Get_AllPowers but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void PDElements_Get_AllPowers_GR(void);


    /*! 
    Complex array of sequence powers into each 3-phase terminal, for each PD element
    
    (API Extension)
    */
    ALTDSS_CAPI_DLL void PDElements_Get_AllSeqPowers(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as PDElements_Get_AllSeqPowers but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void PDElements_Get_AllSeqPowers_GR(void);


    /*! 
    Integer array listing the number of phases of all PD elements
    
    (API Extension)
    */
    ALTDSS_CAPI_DLL void PDElements_Get_AllNumPhases(int32_t** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as PDElements_Get_AllNumPhases but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void PDElements_Get_AllNumPhases_GR(void);


    /*! 
    Integer array listing the number of conductors of all PD elements
    
    (API Extension)
    */
    ALTDSS_CAPI_DLL void PDElements_Get_AllNumConductors(int32_t** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as PDElements_Get_AllNumConductors but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void PDElements_Get_AllNumConductors_GR(void);


    /*! 
    Integer array listing the number of terminals of all PD elements
    
    (API Extension)
    */
    ALTDSS_CAPI_DLL void PDElements_Get_AllNumTerminals(int32_t** ResultPtr, int32_t* ResultDims);

    /*! 
    Same as PDElements_Get_AllNumTerminals but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void PDElements_Get_AllNumTerminals_GR(void);

    /*! 
    Array of strings with all PVSystem names
    */
    ALTDSS_CAPI_DLL void PVSystems_Get_AllNames(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Array of PVSystem energy meter register names
    
    See also the enum `GeneratorRegisters`.
    */
    ALTDSS_CAPI_DLL void PVSystems_Get_RegisterNames(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Array of doubles containing values in PVSystem registers.
    */
    ALTDSS_CAPI_DLL void PVSystems_Get_RegisterValues(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as PVSystems_Get_RegisterValues but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void PVSystems_Get_RegisterValues_GR(void);

    /*! 
    Set first PVSystem active; returns 0 if none.
    */
    ALTDSS_CAPI_DLL int32_t PVSystems_Get_First(void);

    /*! 
    Sets next PVSystem active; returns 0 if no more.
    */
    ALTDSS_CAPI_DLL int32_t PVSystems_Get_Next(void);

    /*! 
    Number of PVSystems
    */
    ALTDSS_CAPI_DLL int32_t PVSystems_Get_Count(void);

    /*! 
    Get/set active PVSystem by index;  1..Count
    */
    ALTDSS_CAPI_DLL int32_t PVSystems_Get_idx(void);

    /*! 
    Get/Set Active PVSystem by index:  1.. Count
    */
    ALTDSS_CAPI_DLL void PVSystems_Set_idx(int32_t Value);

    /*! 
    Get the name of the active PVSystem
    */
    ALTDSS_CAPI_DLL const char* PVSystems_Get_Name(void);

    /*! 
    Set the name of the active PVSystem
    */
    ALTDSS_CAPI_DLL void PVSystems_Set_Name(const char* Value);

    /*! 
    Get the present value of the Irradiance property in kW/sq-m
    */
    ALTDSS_CAPI_DLL double PVSystems_Get_Irradiance(void);

    /*! 
    Set the present Irradiance value in kW/sq-m
    */
    ALTDSS_CAPI_DLL void PVSystems_Set_Irradiance(double Value);

    /*! 
    Get kvar value
    */
    ALTDSS_CAPI_DLL double PVSystems_Get_kvar(void);

    /*! 
    Get Rated kVA of the PVSystem
    */
    ALTDSS_CAPI_DLL double PVSystems_Get_kVArated(void);

    /*! 
    get kW output
    */
    ALTDSS_CAPI_DLL double PVSystems_Get_kW(void);

    /*! 
    Get Power factor
    */
    ALTDSS_CAPI_DLL double PVSystems_Get_PF(void);

    /*! 
    Set kva rated
    */
    ALTDSS_CAPI_DLL void PVSystems_Set_kVArated(double Value);

    /*! 
    Set PF
    */
    ALTDSS_CAPI_DLL void PVSystems_Set_PF(double Value);

    /*! 
    Set kvar output value
    */
    ALTDSS_CAPI_DLL void PVSystems_Set_kvar(double Value);

    /*! 
    Name of the dispatch shape to use for daily simulations. Must be previously
    defined as a Loadshape object of 24 hrs, typically. In the default dispatch
    mode, the PVSystem element uses this loadshape to trigger State changes.
    */
    ALTDSS_CAPI_DLL const char* PVSystems_Get_daily(void);
    ALTDSS_CAPI_DLL void PVSystems_Set_daily(const char* Value);

    /*! 
    Name of the load shape to use for duty cycle dispatch simulations such as
    for solar ramp rate studies. Must be previously defined as a Loadshape
    object. Typically would have time intervals of 1-5 seconds.
    */
    ALTDSS_CAPI_DLL const char* PVSystems_Get_duty(void);
    ALTDSS_CAPI_DLL void PVSystems_Set_duty(const char* Value);

    /*! 
    Dispatch shape to use for yearly simulations. Must be previously defined
    as a Loadshape object. If this is not specified, the Daily dispatch shape,
    if any, is repeated during Yearly solution modes. In the default dispatch
    mode, the PVSystem element uses this loadshape to trigger State changes.
    */
    ALTDSS_CAPI_DLL const char* PVSystems_Get_yearly(void);
    ALTDSS_CAPI_DLL void PVSystems_Set_yearly(const char* Value);

    /*! 
    Temperature shape to use for daily simulations. Must be previously defined
    as a TShape object of 24 hrs, typically. The PVSystem element uses this
    TShape to determine the Pmpp from the Pmpp vs T curve. Units must agree
    with the Pmpp vs T curve.
    */
    ALTDSS_CAPI_DLL const char* PVSystems_Get_Tdaily(void);
    ALTDSS_CAPI_DLL void PVSystems_Set_Tdaily(const char* Value);

    /*! 
    Temperature shape to use for duty cycle dispatch simulations such as for
    solar ramp rate studies. Must be previously defined as a TShape object.
    Typically would have time intervals of 1-5 seconds. Designate the number
    of points to solve using the Set Number=xxxx command. If there are fewer
    points in the actual shape, the shape is assumed to repeat. The PVSystem
    model uses this TShape to determine the Pmpp from the Pmpp vs T curve.
    Units must agree with the Pmpp vs T curve.
    */
    ALTDSS_CAPI_DLL const char* PVSystems_Get_Tduty(void);
    ALTDSS_CAPI_DLL void PVSystems_Set_Tduty(const char* Value);

    /*! 
    Temperature shape to use for yearly simulations. Must be previously defined
    as a TShape object. If this is not specified, the Daily dispatch shape, if
    any, is repeated during Yearly solution modes. The PVSystem element uses
    this TShape to determine the Pmpp from the Pmpp vs T curve. Units must
    agree with the Pmpp vs T curve.
    */
    ALTDSS_CAPI_DLL const char* PVSystems_Get_Tyearly(void);
    ALTDSS_CAPI_DLL void PVSystems_Set_Tyearly(const char* Value);
    
    /*! 
    Gets/sets the rated max power of the PV array for 1.0 kW/sq-m irradiance and a user-selected array temperature of the active PVSystem.
    */
    ALTDSS_CAPI_DLL double PVSystems_Get_Pmpp(void);
    ALTDSS_CAPI_DLL void PVSystems_Set_Pmpp(double Value);

    /*! 
    Returns the current irradiance value for the active PVSystem. Use it to 
    know what's the current irradiance value for the PV during a simulation.
    */
    ALTDSS_CAPI_DLL double PVSystems_Get_IrradianceNow(void);

    /*! 
    Name of the sensor monitoring this PVSystem element.
    */
    ALTDSS_CAPI_DLL const char* PVSystems_Get_Sensor(void);


    /*! 
    Array of strings with names of all Reclosers in Active Circuit
    */
    ALTDSS_CAPI_DLL void Reclosers_Get_AllNames(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Number of Reclosers in active circuit.
    */
    ALTDSS_CAPI_DLL int32_t Reclosers_Get_Count(void);

    /*! 
    Set First Recloser to be Active Ckt Element. Returns 0 if none.
    */
    ALTDSS_CAPI_DLL int32_t Reclosers_Get_First(void);

    /*! 
    Get Name of active Recloser or set the active Recloser by name.
    */
    ALTDSS_CAPI_DLL const char* Reclosers_Get_Name(void);

    /*! 
    Iterate to the next recloser in the circuit. Returns zero if no more.
    */
    ALTDSS_CAPI_DLL int32_t Reclosers_Get_Next(void);

    ALTDSS_CAPI_DLL void Reclosers_Set_Name(const char* Value);

    /*! 
    Terminal number of Monitored object for the Recloser
    */
    ALTDSS_CAPI_DLL int32_t Reclosers_Get_MonitoredTerm(void);

    ALTDSS_CAPI_DLL void Reclosers_Set_MonitoredTerm(int32_t Value);

    /*! 
    Full name of the circuit element that is being switched by the Recloser.
    */
    ALTDSS_CAPI_DLL const char* Reclosers_Get_SwitchedObj(void);

    ALTDSS_CAPI_DLL void Reclosers_Set_SwitchedObj(const char* Value);

    /*! 
    Full name of object this Recloser is monitoring.
    */
    ALTDSS_CAPI_DLL const char* Reclosers_Get_MonitoredObj(void);

    /*! 
    Terminal number of the controlled device being switched by the Recloser
    */
    ALTDSS_CAPI_DLL int32_t Reclosers_Get_SwitchedTerm(void);

    /*! 
    Set monitored object by full name.
    */
    ALTDSS_CAPI_DLL void Reclosers_Set_MonitoredObj(const char* Value);

    ALTDSS_CAPI_DLL void Reclosers_Set_SwitchedTerm(int32_t Value);

    /*! 
    Number of fast shots
    */
    ALTDSS_CAPI_DLL int32_t Reclosers_Get_NumFast(void);

    /*! 
    Array of Doubles: reclose intervals, s, between shots.
    */
    ALTDSS_CAPI_DLL void Reclosers_Get_RecloseIntervals(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Reclosers_Get_RecloseIntervals but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Reclosers_Get_RecloseIntervals_GR(void);

    /*! 
    Number of shots to lockout (fast + delayed)
    */
    ALTDSS_CAPI_DLL int32_t Reclosers_Get_Shots(void);

    ALTDSS_CAPI_DLL void Reclosers_Set_NumFast(int32_t Value);

    ALTDSS_CAPI_DLL void Reclosers_Set_Shots(int32_t Value);

    /*! 
    Phase trip curve multiplier or actual amps
    */
    ALTDSS_CAPI_DLL double Reclosers_Get_PhaseTrip(void);

    /*! 
    Phase Trip multiplier or actual amps
    */
    ALTDSS_CAPI_DLL void Reclosers_Set_PhaseTrip(double Value);

    /*! 
    Ground (3I0) instantaneous trip setting - curve multiplier or actual amps.
    */
    ALTDSS_CAPI_DLL double Reclosers_Get_GroundInst(void);

    /*! 
    Ground (3I0) trip multiplier or actual amps
    */
    ALTDSS_CAPI_DLL double Reclosers_Get_GroundTrip(void);

    /*! 
    Phase instantaneous curve multiplier or actual amps
    */
    ALTDSS_CAPI_DLL double Reclosers_Get_PhaseInst(void);

    /*! 
    Ground (3I0) trip instantaneous multiplier or actual amps
    */
    ALTDSS_CAPI_DLL void Reclosers_Set_GroundInst(double Value);

    ALTDSS_CAPI_DLL void Reclosers_Set_GroundTrip(double Value);

    ALTDSS_CAPI_DLL void Reclosers_Set_PhaseInst(double Value);

    /*! 
    Close the switched object controlled by the recloser. Resets recloser to first operation.
    */
    ALTDSS_CAPI_DLL void Reclosers_Close(void);

    /*! 
    Open recloser's controlled element and lock out the recloser.
    */
    ALTDSS_CAPI_DLL void Reclosers_Open(void);

    /*! 
    Reset recloser to normal state. If open, lock out the recloser. If closed, resets recloser to first operation.
    */
    ALTDSS_CAPI_DLL void Reclosers_Reset(void);

    /*! 
    Get/Set the active Recloser by index into the recloser list.  1..Count
    */
    ALTDSS_CAPI_DLL int32_t Reclosers_Get_idx(void);

    /*! 
    Get/Set the Active Recloser by index into the recloser list. 1..Count
    */
    ALTDSS_CAPI_DLL void Reclosers_Set_idx(int32_t Value);

    /*! 
    Get/Set present state of recloser. 
    If set to open (ActionCodes.Open=1), open recloser's controlled element and lock out the recloser. 
    If set to close (ActionCodes.Close=2), close recloser's controlled element and resets recloser to first operation.
    */
    ALTDSS_CAPI_DLL int32_t Reclosers_Get_State(void);

    /*! 
    Get/Set present state of recloser. 
    If set to open (ActionCodes.Open=1), open recloser's controlled element and lock out the recloser. 
    If set to close (ActionCodes.Close=2), close recloser's controlled element and resets recloser to first operation.
    */
    ALTDSS_CAPI_DLL void Reclosers_Set_State(int32_t Value);

    /*! 
    Get/set normal state (ActionCodes.Open=1, ActionCodes.Close=2) of the recloser.
    */
    ALTDSS_CAPI_DLL int32_t Reclosers_Get_NormalState(void);

    /*! 
    Get/set normal state (ActionCodes.Open=1, ActionCodes.Close=2) of the recloser.
    */
    ALTDSS_CAPI_DLL void Reclosers_Set_NormalState(int32_t Value);

    /*! 
    Array of strings containing all RegControl names
    */
    ALTDSS_CAPI_DLL void RegControls_Get_AllNames(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    CT primary ampere rating (secondary is 0.2 amperes)
    */
    ALTDSS_CAPI_DLL double RegControls_Get_CTPrimary(void);

    /*! 
    Time delay [s] after arming before the first tap change. Control may reset before actually changing taps.
    */
    ALTDSS_CAPI_DLL double RegControls_Get_Delay(void);

    /*! 
    Sets the first RegControl active. Returns 0 if none.
    */
    ALTDSS_CAPI_DLL int32_t RegControls_Get_First(void);

    /*! 
    Regulation bandwidth in forward direction, centered on Vreg
    */
    ALTDSS_CAPI_DLL double RegControls_Get_ForwardBand(void);

    /*! 
    LDC R setting in Volts
    */
    ALTDSS_CAPI_DLL double RegControls_Get_ForwardR(void);

    /*! 
    Target voltage in the forward direction, on PT secondary base.
    */
    ALTDSS_CAPI_DLL double RegControls_Get_ForwardVreg(void);

    /*! 
    LDC X setting in Volts
    */
    ALTDSS_CAPI_DLL double RegControls_Get_ForwardX(void);

    /*! 
    Time delay is inversely adjusted, proportional to the amount of voltage outside the regulating band.
    */
    ALTDSS_CAPI_DLL uint16_t RegControls_Get_IsInverseTime(void);

    /*! 
    Regulator can use different settings in the reverse direction.  Usually not applicable to substation transformers.
    */
    ALTDSS_CAPI_DLL uint16_t RegControls_Get_IsReversible(void);

    /*! 
    Maximum tap change per iteration in STATIC solution mode. 1 is more realistic, 16 is the default for a faster solution.
    */
    ALTDSS_CAPI_DLL int32_t RegControls_Get_MaxTapChange(void);

    /*! 
    Name of a remote regulated bus, in lieu of LDC settings
    */
    ALTDSS_CAPI_DLL const char* RegControls_Get_MonitoredBus(void);

    /*! 
    Get/set Active RegControl  name
    */
    ALTDSS_CAPI_DLL const char* RegControls_Get_Name(void);

    /*! 
    Sets the next RegControl active. Returns 0 if none.
    */
    ALTDSS_CAPI_DLL int32_t RegControls_Get_Next(void);

    /*! 
    PT ratio for voltage control settings
    */
    ALTDSS_CAPI_DLL double RegControls_Get_PTratio(void);

    /*! 
    Bandwidth in reverse direction, centered on reverse Vreg.
    */
    ALTDSS_CAPI_DLL double RegControls_Get_ReverseBand(void);

    /*! 
    Reverse LDC R setting in Volts.
    */
    ALTDSS_CAPI_DLL double RegControls_Get_ReverseR(void);

    /*! 
    Target voltage in the revese direction, on PT secondary base.
    */
    ALTDSS_CAPI_DLL double RegControls_Get_ReverseVreg(void);

    /*! 
    Reverse LDC X setting in volts.
    */
    ALTDSS_CAPI_DLL double RegControls_Get_ReverseX(void);

    /*! 
    Time delay [s] for subsequent tap changes in a set. Control may reset before actually changing taps.
    */
    ALTDSS_CAPI_DLL double RegControls_Get_TapDelay(void);

    /*! 
    Tapped winding number
    */
    ALTDSS_CAPI_DLL int32_t RegControls_Get_TapWinding(void);

    /*! 
    Name of the transformer this regulator controls
    */
    ALTDSS_CAPI_DLL const char* RegControls_Get_Transformer(void);

    /*! 
    First house voltage limit on PT secondary base.  Setting to 0 disables this function.
    */
    ALTDSS_CAPI_DLL double RegControls_Get_VoltageLimit(void);

    /*! 
    Winding number for PT and CT connections
    */
    ALTDSS_CAPI_DLL int32_t RegControls_Get_Winding(void);

    ALTDSS_CAPI_DLL int32_t RegControls_Get_TapNumber(void);

    /*! 
    CT primary ampere rating (secondary is 0.2 amperes)
    */
    ALTDSS_CAPI_DLL void RegControls_Set_CTPrimary(double Value);

    /*! 
    Time delay [s] after arming before the first tap change. Control may reset before actually changing taps.
    */
    ALTDSS_CAPI_DLL void RegControls_Set_Delay(double Value);

    /*! 
    Regulation bandwidth in forward direction, centered on Vreg
    */
    ALTDSS_CAPI_DLL void RegControls_Set_ForwardBand(double Value);

    /*! 
    LDC R setting in Volts
    */
    ALTDSS_CAPI_DLL void RegControls_Set_ForwardR(double Value);

    /*! 
    Target voltage in the forward direction, on PT secondary base.
    */
    ALTDSS_CAPI_DLL void RegControls_Set_ForwardVreg(double Value);

    /*! 
    LDC X setting in Volts
    */
    ALTDSS_CAPI_DLL void RegControls_Set_ForwardX(double Value);

    /*! 
    Time delay is inversely adjusted, proportional to the amount of voltage outside the regulating band.
    */
    ALTDSS_CAPI_DLL void RegControls_Set_IsInverseTime(uint16_t Value);

    /*! 
    Regulator can use different settings in the reverse direction.  Usually not applicable to substation transformers.
    */
    ALTDSS_CAPI_DLL void RegControls_Set_IsReversible(uint16_t Value);

    /*! 
    Maximum tap change per iteration in STATIC solution mode. 1 is more realistic, 16 is the default for a faster solution.
    */
    ALTDSS_CAPI_DLL void RegControls_Set_MaxTapChange(int32_t Value);

    /*! 
    Name of a remote regulated bus, in lieu of LDC settings
    */
    ALTDSS_CAPI_DLL void RegControls_Set_MonitoredBus(const char* Value);

    /*! 
    Sets a RegControl active by name
    */
    ALTDSS_CAPI_DLL void RegControls_Set_Name(const char* Value);

    /*! 
    PT ratio for voltage control settings
    */
    ALTDSS_CAPI_DLL void RegControls_Set_PTratio(double Value);

    /*! 
    Bandwidth in reverse direction, centered on reverse Vreg.
    */
    ALTDSS_CAPI_DLL void RegControls_Set_ReverseBand(double Value);

    /*! 
    Reverse LDC R setting in Volts.
    */
    ALTDSS_CAPI_DLL void RegControls_Set_ReverseR(double Value);

    /*! 
    Target voltage in the revese direction, on PT secondary base.
    */
    ALTDSS_CAPI_DLL void RegControls_Set_ReverseVreg(double Value);

    /*! 
    Reverse LDC X setting in volts.
    */
    ALTDSS_CAPI_DLL void RegControls_Set_ReverseX(double Value);

    /*! 
    Time delay [s] for subsequent tap changes in a set. Control may reset before actually changing taps.
    */
    ALTDSS_CAPI_DLL void RegControls_Set_TapDelay(double Value);

    /*! 
    Tapped winding number
    */
    ALTDSS_CAPI_DLL void RegControls_Set_TapWinding(int32_t Value);

    /*! 
    Name of the transformer this regulator controls
    */
    ALTDSS_CAPI_DLL void RegControls_Set_Transformer(const char* Value);

    /*! 
    First house voltage limit on PT secondary base.  Setting to 0 disables this function.
    */
    ALTDSS_CAPI_DLL void RegControls_Set_VoltageLimit(double Value);

    /*! 
    Winding number for PT and CT connections
    */
    ALTDSS_CAPI_DLL void RegControls_Set_Winding(int32_t Value);

    /*! 
    Integer number of the tap that the controlled transformer winding is currently on.
    */
    ALTDSS_CAPI_DLL void RegControls_Set_TapNumber(int32_t Value);

    /*! 
    Number of RegControl objects in Active Circuit
    */
    ALTDSS_CAPI_DLL int32_t RegControls_Get_Count(void);

    ALTDSS_CAPI_DLL void RegControls_Reset(void);

    /*! 
    Array of strings containing names of all Relay elements
    */
    ALTDSS_CAPI_DLL void Relays_Get_AllNames(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Number of Relays in circuit
    */
    ALTDSS_CAPI_DLL int32_t Relays_Get_Count(void);

    /*! 
    Set First Relay active. If none, returns 0.
    */
    ALTDSS_CAPI_DLL int32_t Relays_Get_First(void);

    /*! 
    Get name of active relay.
    */
    ALTDSS_CAPI_DLL const char* Relays_Get_Name(void);

    /*! 
    Advance to next Relay object. Returns 0 when no more relays.
    */
    ALTDSS_CAPI_DLL int32_t Relays_Get_Next(void);

    /*! 
    Set Relay active by name
    */
    ALTDSS_CAPI_DLL void Relays_Set_Name(const char* Value);

    /*! 
    Full name of object this Relay is monitoring.
    */
    ALTDSS_CAPI_DLL const char* Relays_Get_MonitoredObj(void);

    ALTDSS_CAPI_DLL void Relays_Set_MonitoredObj(const char* Value);

    /*! 
    Number of terminal of monitored element that this Relay is monitoring.
    */
    ALTDSS_CAPI_DLL int32_t Relays_Get_MonitoredTerm(void);

    /*! 
    Full name of element that will be switched when relay trips.
    */
    ALTDSS_CAPI_DLL const char* Relays_Get_SwitchedObj(void);

    ALTDSS_CAPI_DLL void Relays_Set_MonitoredTerm(int32_t Value);

    ALTDSS_CAPI_DLL void Relays_Set_SwitchedObj(const char* Value);

    ALTDSS_CAPI_DLL int32_t Relays_Get_SwitchedTerm(void);

    /*! 
    Terminal number of the switched object that will be opened when the relay trips.
    */
    ALTDSS_CAPI_DLL void Relays_Set_SwitchedTerm(int32_t Value);

    /*! 
    Get/Set active Relay by index into the Relay list. 1..Count
    */
    ALTDSS_CAPI_DLL int32_t Relays_Get_idx(void);

    /*! 
    Get/Set Relay active by index into relay list. 1..Count
    */
    ALTDSS_CAPI_DLL void Relays_Set_idx(int32_t Value);

    /*! 
    Open relay's controlled element and lock out the relay.
    */
    ALTDSS_CAPI_DLL void Relays_Open(void);
    
    /*! 
    Close the switched object controlled by the relay. Resets relay to first operation.
    */
    ALTDSS_CAPI_DLL void Relays_Close(void);

    /*! 
    Reset relay to normal state. If open, lock out the relay. If closed, resets relay to first operation.
    */
    ALTDSS_CAPI_DLL void Relays_Reset(void);

    /*! 
    Get/Set present state of relay. 
    If set to open (ActionCodes.Open = 1), open relay's controlled element and lock out the relay. 
    If set to close (ActionCodes.Close = 2), close relay's controlled element and resets relay to first operation.
    */
    ALTDSS_CAPI_DLL int32_t Relays_Get_State(void);
    
    /*! 
    Get/Set present state of relay. 
    If set to open (ActionCodes.Open = 1), open relay's controlled element and lock out the relay. 
    If set to close (ActionCodes.Close = 2), close relay's controlled element and resets relay to first operation.
    */
    ALTDSS_CAPI_DLL void Relays_Set_State(int32_t Value);

    /*! 
    Get/set normal state of relay.
    */
    ALTDSS_CAPI_DLL int32_t Relays_Get_NormalState(void);

    /*! 
    Get/set normal state of relay.
    */
    ALTDSS_CAPI_DLL void Relays_Set_NormalState(int32_t Value);

    /*! 
    Array of Sensor names.
    */
    ALTDSS_CAPI_DLL void Sensors_Get_AllNames(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Number of Sensors in Active Circuit.
    */
    ALTDSS_CAPI_DLL int32_t Sensors_Get_Count(void);

    /*! 
    Array of doubles for the line current measurements; don't use with kWS and kVARS.
    */
    ALTDSS_CAPI_DLL void Sensors_Get_Currents(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Sensors_Get_Currents but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Sensors_Get_Currents_GR(void);

    /*! 
    Sets the first sensor active. Returns 0 if none.
    */
    ALTDSS_CAPI_DLL int32_t Sensors_Get_First(void);

    /*! 
    True if measured voltages are line-line. Currents are always line currents.
    */
    ALTDSS_CAPI_DLL uint16_t Sensors_Get_IsDelta(void);

    /*! 
    Array of doubles for Q measurements. Overwrites Currents with a new estimate using kWS.
    */
    ALTDSS_CAPI_DLL void Sensors_Get_kVARS(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Sensors_Get_kVARS but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Sensors_Get_kVARS_GR(void);

    /*! 
    Array of doubles for the LL or LN (depending on Delta connection) voltage measurements.
    */
    ALTDSS_CAPI_DLL void Sensors_Get_kVS(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Sensors_Get_kVS but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Sensors_Get_kVS_GR(void);

    /*! 
    Array of doubles for P measurements. Overwrites Currents with a new estimate using kVARS.
    */
    ALTDSS_CAPI_DLL void Sensors_Get_kWS(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Sensors_Get_kWS but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Sensors_Get_kWS_GR(void);

    /*! 
    Full Name of the measured element
    */
    ALTDSS_CAPI_DLL const char* Sensors_Get_MeteredElement(void);

    /*! 
    Number of the measured terminal in the measured element.
    */
    ALTDSS_CAPI_DLL int32_t Sensors_Get_MeteredTerminal(void);

    /*! 
    Name of the active sensor.
    */
    ALTDSS_CAPI_DLL const char* Sensors_Get_Name(void);

    /*! 
    Sets the next Sensor active. Returns 0 if no more.
    */
    ALTDSS_CAPI_DLL int32_t Sensors_Get_Next(void);

    /*! 
    Assumed percent error in the Sensor measurement. Default is 1.
    */
    ALTDSS_CAPI_DLL double Sensors_Get_PctError(void);

    /*! 
    True if voltage measurements are 1-3, 3-2, 2-1.
    */
    ALTDSS_CAPI_DLL uint16_t Sensors_Get_ReverseDelta(void);

    /*! 
    Weighting factor for this Sensor measurement with respect to other Sensors. Default is 1.
    */
    ALTDSS_CAPI_DLL double Sensors_Get_Weight(void);

    ALTDSS_CAPI_DLL void Sensors_Reset(void);

    ALTDSS_CAPI_DLL void Sensors_ResetAll(void);

    ALTDSS_CAPI_DLL void Sensors_Set_Currents(const double* ValuePtr, int32_t ValueCount);

    ALTDSS_CAPI_DLL void Sensors_Set_IsDelta(uint16_t Value);

    ALTDSS_CAPI_DLL void Sensors_Set_kVARS(const double* ValuePtr, int32_t ValueCount);

    ALTDSS_CAPI_DLL void Sensors_Set_kVS(const double* ValuePtr, int32_t ValueCount);

    ALTDSS_CAPI_DLL void Sensors_Set_kWS(const double* ValuePtr, int32_t ValueCount);

    ALTDSS_CAPI_DLL void Sensors_Set_MeteredElement(const char* Value);

    ALTDSS_CAPI_DLL void Sensors_Set_MeteredTerminal(int32_t Value);

    /*! 
    Set the active Sensor by name.
    */
    ALTDSS_CAPI_DLL void Sensors_Set_Name(const char* Value);

    ALTDSS_CAPI_DLL void Sensors_Set_PctError(double Value);

    ALTDSS_CAPI_DLL void Sensors_Set_ReverseDelta(uint16_t Value);

    ALTDSS_CAPI_DLL void Sensors_Set_Weight(double Value);

    /*! 
    Voltage base for the sensor measurements. LL for 2 and 3-phase sensors, LN for 1-phase sensors.
    */
    ALTDSS_CAPI_DLL double Sensors_Get_kVbase(void);

    ALTDSS_CAPI_DLL void Sensors_Set_kVbase(double Value);

    /*! 
    Array of doubles for the allocation factors for each phase.
    */
    ALTDSS_CAPI_DLL void Sensors_Get_AllocationFactor(double** ResultPtr, int32_t* ResultDims);

    /*! 
    Same as Sensors_Get_AllocationFactor but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Sensors_Get_AllocationFactor_GR(void);
    

    /*! 
    {True | False*} Designates whether to allow duplicate names of objects
    */
    ALTDSS_CAPI_DLL uint16_t Settings_Get_AllowDuplicates(void);

    /*! 
    List of Buses or (File=xxxx) syntax for the AutoAdd solution mode.
    */
    ALTDSS_CAPI_DLL const char* Settings_Get_AutoBusList(void);

    /*! 
    {Multiphase * | PositiveSeq} Indicate if the circuit model is positive sequence.
    */
    ALTDSS_CAPI_DLL int32_t Settings_Get_CktModel(void);

    ALTDSS_CAPI_DLL void Settings_Set_CktModel(int32_t Value);

    /*! 
    Per Unit maximum voltage for Emergency conditions.
    */
    ALTDSS_CAPI_DLL double Settings_Get_EmergVmaxpu(void);

    /*! 
    Per Unit minimum voltage for Emergency conditions.
    */
    ALTDSS_CAPI_DLL double Settings_Get_EmergVminpu(void);

    /*! 
    Per Unit maximum voltage for Normal conditions.
    */
    ALTDSS_CAPI_DLL double Settings_Get_NormVmaxpu(void);

    /*! 
    Per Unit minimum voltage for Normal conditions.
    */
    ALTDSS_CAPI_DLL double Settings_Get_NormVminpu(void);

    /*! 
    {True | False*}  Locks Zones on energy meters to prevent rebuilding if a circuit change occurs.
    */
    ALTDSS_CAPI_DLL uint16_t Settings_Get_ZoneLock(void);

    /*! 
    Sets all load allocation factors for all loads defined by XFKVA property to this value.
    */
    ALTDSS_CAPI_DLL void Settings_Set_AllocationFactors(double Value);

    /*! 
    {True | False*} Designates whether to allow duplicate names of objects
    */
    ALTDSS_CAPI_DLL void Settings_Set_AllowDuplicates(uint16_t Value);

    /*! 
    List of Buses or (File=xxxx) syntax for the AutoAdd solution mode.
    */
    ALTDSS_CAPI_DLL void Settings_Set_AutoBusList(const char* Value);

    /*! 
    Per Unit maximum voltage for Emergency conditions.
    */
    ALTDSS_CAPI_DLL void Settings_Set_EmergVmaxpu(double Value);

    /*! 
    Per Unit minimum voltage for Emergency conditions.
    */
    ALTDSS_CAPI_DLL void Settings_Set_EmergVminpu(double Value);

    /*! 
    Per Unit maximum voltage for Normal conditions.
    */
    ALTDSS_CAPI_DLL void Settings_Set_NormVmaxpu(double Value);

    /*! 
    Per Unit minimum voltage for Normal conditions.
    */
    ALTDSS_CAPI_DLL void Settings_Set_NormVminpu(double Value);

    /*! 
    {True | False*}  Locks Zones on energy meters to prevent rebuilding if a circuit change occurs.
    */
    ALTDSS_CAPI_DLL void Settings_Set_ZoneLock(uint16_t Value);

    /*! 
    Integer array defining which energy meter registers to use for computing losses
    */
    ALTDSS_CAPI_DLL void Settings_Get_LossRegs(int32_t** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Settings_Get_LossRegs but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Settings_Get_LossRegs_GR(void);

    /*! 
    Weighting factor applied to Loss register values.
    */
    ALTDSS_CAPI_DLL double Settings_Get_LossWeight(void);

    /*! 
    {True | False *} Gets value of trapezoidal integration flag in energy meters.
    */
    ALTDSS_CAPI_DLL uint16_t Settings_Get_Trapezoidal(void);

    /*! 
    Array of Integers defining energy meter registers to use for computing UE
    */
    ALTDSS_CAPI_DLL void Settings_Get_UEregs(int32_t** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Settings_Get_UEregs but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Settings_Get_UEregs_GR(void);

    /*! 
    Weighting factor applied to UE register values.
    */
    ALTDSS_CAPI_DLL double Settings_Get_UEweight(void);

    /*! 
    Integer array defining which energy meter registers to use for computing losses
    */
    ALTDSS_CAPI_DLL void Settings_Set_LossRegs(const int32_t* ValuePtr, int32_t ValueCount);

    /*! 
    Weighting factor applied to Loss register values.
    */
    ALTDSS_CAPI_DLL void Settings_Set_LossWeight(double Value);

    /*! 
    {True | False *} Gets value of trapezoidal integration flag in energy meters.
    */
    ALTDSS_CAPI_DLL void Settings_Set_Trapezoidal(uint16_t Value);

    /*! 
    Array of Integers defining energy meter registers to use for computing UE
    */
    ALTDSS_CAPI_DLL void Settings_Set_UEregs(const int32_t* ValuePtr, int32_t ValueCount);

    /*! 
    Weighting factor applied to UE register values.
    */
    ALTDSS_CAPI_DLL void Settings_Set_UEweight(double Value);

    /*! 
    {True | False*} Denotes whether to trace the control actions to a file.
    */
    ALTDSS_CAPI_DLL uint16_t Settings_Get_ControlTrace(void);

    /*! 
    Array of doubles defining the legal voltage bases in kV L-L
    */
    ALTDSS_CAPI_DLL void Settings_Get_VoltageBases(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Settings_Get_VoltageBases but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Settings_Get_VoltageBases_GR(void);

    /*! 
    {True | False*} Denotes whether to trace the control actions to a file.
    */
    ALTDSS_CAPI_DLL void Settings_Set_ControlTrace(uint16_t Value);

    /*! 
    Array of doubles defining the legal voltage bases in kV L-L
    */
    ALTDSS_CAPI_DLL void Settings_Set_VoltageBases(const double* ValuePtr, int32_t ValueCount);

    /*! 
    Name of LoadShape object that serves as the source of price signal data for yearly simulations, etc.
    */
    ALTDSS_CAPI_DLL const char* Settings_Get_PriceCurve(void);

    /*! 
    Price Signal for the Circuit
    */
    ALTDSS_CAPI_DLL double Settings_Get_PriceSignal(void);

    /*! 
    Name of LoadShape object that serves as the source of price signal data for yearly simulations, etc.
    */
    ALTDSS_CAPI_DLL void Settings_Set_PriceCurve(const char* Value);

    /*! 
    Price Signal for the Circuit
    */
    ALTDSS_CAPI_DLL void Settings_Set_PriceSignal(double Value);

    /*! 
    Controls whether the terminals are checked when updating the currents in Load component. Defaults to True.
    If the loads are guaranteed to have their terminals closed throughout the simulation, this can be set to False to save some time.
    
    (API Extension)
    */
    ALTDSS_CAPI_DLL uint16_t Settings_Get_LoadsTerminalCheck(void);
    ALTDSS_CAPI_DLL void Settings_Set_LoadsTerminalCheck(uint16_t Value);

    /*! 
    Controls whether `First`/`Next` iteration includes or skips disabled circuit elements.
    The default behavior from OpenDSS is to skip those. The user can still activate the element by name or index.
    
    The default value for IterateDisabled is 0, keeping the original behavior.
    Set it to 1 (or `True`) to include disabled elements.
    Other numeric values are reserved for other potential behaviors.
    
    (API Extension)
    */
    ALTDSS_CAPI_DLL int32_t Settings_Get_IterateDisabled(void);
    ALTDSS_CAPI_DLL void Settings_Set_IterateDisabled(int32_t Value);


    /*!
    This function controls the naming convention of the DSS properties.

    Accepted values:
    
        - DSSPropertyNameStyle_Modern (0): By default, the modern names are used. The names were reviewed to 
          try to reach a convention across all components.
        - DSSPropertyNameStyle_Lowercase (1): Use all lowercase strings.
        - DSSPropertyNameStyle_Legacy (2): Use the previous capitalization of the property names.

    The legacy alternative is kept for easier backwards compatibility. If you software relies on
    comparing strings directly, using this is a good alternative.

    The DSS engine is case insensitive, the capitalization of the names do not affect handling of 
    DSS scripts or most of the related API functions. This setting does affect the property names 
    returned by the Property API and the output of the "save circuit" command.

    In a future version, there could be other options that allow different and incompatible names.
    Replacing the percent sign by "pct" and other names not compatible with identifier in common
    programming languages is under consideration.

    **Affects the current DSS instance immediately, and all DSS instances after a "clear" command.**

    Related enumeration: DSSPropertyNameStyle

    (API Extension)
    */
    ALTDSS_CAPI_DLL void Settings_SetPropertyNameStyle(int32_t style);


    /*!
    Regular expression pattern to skip files

    If a file name as provided in the input for the `Redirect` and `Compile` commands
    matches the regular expression pattern, it is skipped (the file is not read nor
    commands contained in the file are executed).

    Set to an empty string to reset/disable the filter. A `clear` command, or the DSS_ClearAll` 
    function also resets the value of this setting.

    Case-insensitive.
    See https://regex.sorokin.engineer/en/latest/regular_expressions.html for information on 
    the expression syntax and options.

    

    (API Extension)
    */
    ALTDSS_CAPI_DLL void Settings_Set_SkipFileRegExp(const char* Value);
    ALTDSS_CAPI_DLL const char* Settings_Get_SkipFileRegExp(void);

    /*!
    List of command codes to skip

    List of integer command codes representing the command to skip when processing DSS text commands or files.
    
    A `clear` command, or the DSS_ClearAll` function resets the value to include all commands. The user can
    also pass an empty list of commands to reset it manually, enabling the execution of all commands, besides
    DOSCmd and others that have dedicated toggles.

    **Do not hardcode** the integers since they change frequently.

    `DSS_Executive_Get_Command` and `DSS_Executive_Get_NumCommands` can be used to list the available commands.
    Use the integer provided to `DSS_Executive_Get_Command` matching the command name returned.

    (API Extension)
    */
    ALTDSS_CAPI_DLL void Settings_Set_SkipCommands(const int32_t* ValuePtr, int32_t ValueCount);
    ALTDSS_CAPI_DLL void Settings_Get_SkipCommands(int32_t** ResultPtr, int32_t* ResultDims);
    ALTDSS_CAPI_DLL void Settings_Get_SkipCommands_GR(void);

    /*!
    Get the value of an option flag in the settings

    This function was introduce to simplify adding new options to AltDSS.

    See the `DSSSettings` enumeration for the possible flags and their behavior.

    (API Extension)
    */
    ALTDSS_CAPI_DLL uint16_t Settings_Get_Flag(int32_t flag);

    /*!
    Set the value of an option flag in the settings

    This function was introduce to simplify adding new options to AltDSS.

    See the `DSSSettings` enumeration for the possible flags and their behavior.

    Setting a value can have side-effects. Some side-effects require running a `Clear` command.

    (API Extension)
    */
    ALTDSS_CAPI_DLL void Settings_Set_Flag(int32_t flag, uint16_t Value);

    /*! 
    Set the Frequency for next solution
    */
    ALTDSS_CAPI_DLL double Solution_Get_Frequency(void);

    /*! 
    Set Hour for time series solutions.
    */
    ALTDSS_CAPI_DLL int32_t Solution_Get_Hour(void);

    /*! 
    Number of iterations taken for last solution. (Same as TotalIterations)
    */
    ALTDSS_CAPI_DLL int32_t Solution_Get_Iterations(void);

    /*! 
    Default load multiplier applied to all non-fixed loads
    */
    ALTDSS_CAPI_DLL double Solution_Get_LoadMult(void);

    /*! 
    Max allowable iterations.
    */
    ALTDSS_CAPI_DLL int32_t Solution_Get_MaxIterations(void);

    /*! 
    Get present solution mode
    */
    ALTDSS_CAPI_DLL int32_t Solution_Get_Mode(void);

    /*! 
    Number of solutions to perform for Monte Carlo and time series simulations
    */
    ALTDSS_CAPI_DLL int32_t Solution_Get_Number(void);


    /*! 
    Randomization mode for random variables "Gaussian", "Uniform" or "LogNormal"
    */
    ALTDSS_CAPI_DLL void Solution_Set_Random(int32_t Random);

    /*! 
    Randomization mode for random variables "Gaussian", "Uniform" or "LogNormal"
    */
    ALTDSS_CAPI_DLL int32_t Solution_Get_Random(void);

    /*! 
    Seconds from top of the hour.
    */
    ALTDSS_CAPI_DLL double Solution_Get_Seconds(void);

    /*! 
    Time step size in sec
    */
    ALTDSS_CAPI_DLL double Solution_Get_StepSize(void);

    /*! 
    Solution convergence tolerance.
    */
    ALTDSS_CAPI_DLL double Solution_Get_Tolerance(void);

    /*! 
    Set year for planning studies
    */
    ALTDSS_CAPI_DLL int32_t Solution_Get_Year(void);

    /*! 
    Set the Frequency for next solution
    */
    ALTDSS_CAPI_DLL void Solution_Set_Frequency(double Value);

    /*! 
    Set Hour for time series solutions.
    */
    ALTDSS_CAPI_DLL void Solution_Set_Hour(int32_t Value);

    /*! 
    Default load multiplier applied to all non-fixed loads
    */
    ALTDSS_CAPI_DLL void Solution_Set_LoadMult(double Value);

    /*! 
    Max allowable iterations.
    */
    ALTDSS_CAPI_DLL void Solution_Set_MaxIterations(int32_t Value);

    /*! 
    Set present solution mode
    */
    ALTDSS_CAPI_DLL void Solution_Set_Mode(int32_t Mode);

    /*! 
    Number of solutions to perform for Monte Carlo and time series simulations
    */
    ALTDSS_CAPI_DLL void Solution_Set_Number(int32_t Value);

    /*! 
    Seconds from top of the hour.
    */
    ALTDSS_CAPI_DLL void Solution_Set_Seconds(double Value);

    /*! 
    Time step size in sec
    */
    ALTDSS_CAPI_DLL void Solution_Set_StepSize(double Value);

    /*! 
    Solution convergence tolerance.
    */
    ALTDSS_CAPI_DLL void Solution_Set_Tolerance(double Value);

    /*! 
    Set year for planning studies
    */
    ALTDSS_CAPI_DLL void Solution_Set_Year(int32_t Value);

    ALTDSS_CAPI_DLL void Solution_Solve(void);

    /*! 
    ID (text) of the present solution mode
    */
    ALTDSS_CAPI_DLL const char* Solution_Get_ModeID(void);

    /*! 
    Load Model: {PowerFlow (default) | Admittance}
    */
    ALTDSS_CAPI_DLL int32_t Solution_Get_LoadModel(void);

    /*! 
    Load Model: {PowerFlow (default) | Admittance}
    */
    ALTDSS_CAPI_DLL void Solution_Set_LoadModel(int32_t Value);

    /*! 
    Load-Duration Curve name for LD modes
    */
    ALTDSS_CAPI_DLL const char* Solution_Get_LDCurve(void);

    /*! 
    Load-Duration Curve name for LD modes
    */
    ALTDSS_CAPI_DLL void Solution_Set_LDCurve(const char* Value);

    /*! 
    Percent default  annual load growth rate
    */
    ALTDSS_CAPI_DLL double Solution_Get_pctGrowth(void);

    /*! 
    Percent default  annual load growth rate
    */
    ALTDSS_CAPI_DLL void Solution_Set_pctGrowth(double Value);

    /*! 
    Type of device to add in AutoAdd Mode: {AddGen (Default) | AddCap}
    */
    ALTDSS_CAPI_DLL int32_t Solution_Get_AddType(void);

    ALTDSS_CAPI_DLL void Solution_Set_AddType(int32_t Value);

    /*! 
    Generator kW for AutoAdd mode
    */
    ALTDSS_CAPI_DLL double Solution_Get_GenkW(void);

    ALTDSS_CAPI_DLL void Solution_Set_GenkW(double Value);

    /*! 
    PF for generators in AutoAdd mode
    */
    ALTDSS_CAPI_DLL double Solution_Get_GenPF(void);

    /*! 
    PF for generators in AutoAdd mode
    */
    ALTDSS_CAPI_DLL void Solution_Set_GenPF(double Value);

    /*! 
    Capacitor kvar for adding capacitors in AutoAdd mode
    */
    ALTDSS_CAPI_DLL double Solution_Get_Capkvar(void);

    /*! 
    Capacitor kvar for adding capacitors in AutoAdd mode
    */
    ALTDSS_CAPI_DLL void Solution_Set_Capkvar(double Value);

    /*! 
    Base Solution algorithm: {NormalSolve | NewtonSolve}
    */
    ALTDSS_CAPI_DLL int32_t Solution_Get_Algorithm(void);

    /*! 
    Base Solution algorithm: {NormalSolve | NewtonSolve}
    */
    ALTDSS_CAPI_DLL void Solution_Set_Algorithm(int32_t Value);

    /*! 
    {Static* | Event | Time | Off} Modes for control devices (see ControlModes)
    */
    ALTDSS_CAPI_DLL int32_t Solution_Get_ControlMode(void);

    ALTDSS_CAPI_DLL void Solution_Set_ControlMode(int32_t Value);

    /*! 
    Default Multiplier applied to generators (like LoadMult)
    */
    ALTDSS_CAPI_DLL double Solution_Get_GenMult(void);

    /*! 
    Default Multiplier applied to generators (like LoadMult)
    */
    ALTDSS_CAPI_DLL void Solution_Set_GenMult(double Value);

    /*! 
    Default daily load shape (defaults to "Default")
    */
    ALTDSS_CAPI_DLL const char* Solution_Get_DefaultDaily(void);

    /*! 
    Default Yearly load shape (defaults to "Default")
    */
    ALTDSS_CAPI_DLL const char* Solution_Get_DefaultYearly(void);

    /*! 
    Default daily load shape (defaults to "Default")
    */
    ALTDSS_CAPI_DLL void Solution_Set_DefaultDaily(const char* Value);

    /*! 
    Default Yearly load shape (defaults to "Default")
    */
    ALTDSS_CAPI_DLL void Solution_Set_DefaultYearly(const char* Value);

    /*! 
    Array of strings containing the Event Log
    */
    ALTDSS_CAPI_DLL void Solution_Get_EventLog(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Hour as a double, including fractional part
    */
    ALTDSS_CAPI_DLL double Solution_Get_dblHour(void);

    /*! 
    Hour as a double, including fractional part
    */
    ALTDSS_CAPI_DLL void Solution_Set_dblHour(double Value);

    /*! 
    Set Stepsize in Hr
    */
    ALTDSS_CAPI_DLL void Solution_Set_StepsizeHr(double Value);

    /*! 
    Set Stepsize in minutes
    */
    ALTDSS_CAPI_DLL void Solution_Set_StepsizeMin(double Value);

    /*! 
    Value of the control iteration counter
    */
    ALTDSS_CAPI_DLL int32_t Solution_Get_ControlIterations(void);

    /*! 
    Maximum allowable control iterations
    */
    ALTDSS_CAPI_DLL int32_t Solution_Get_MaxControlIterations(void);

    ALTDSS_CAPI_DLL void Solution_Sample_DoControlActions(void);

    /*! 
    Value of the control iteration counter
    */
    ALTDSS_CAPI_DLL void Solution_Set_ControlIterations(int32_t Value);

    /*! 
    Maximum allowable control iterations
    */
    ALTDSS_CAPI_DLL void Solution_Set_MaxControlIterations(int32_t Value);

    ALTDSS_CAPI_DLL void Solution_CheckFaultStatus(void);

    ALTDSS_CAPI_DLL void Solution_SolveDirect(void);

    ALTDSS_CAPI_DLL void Solution_SolveNoControl(void);

    ALTDSS_CAPI_DLL void Solution_SolvePflow(void);

    ALTDSS_CAPI_DLL void Solution_SolvePlusControl(void);

    ALTDSS_CAPI_DLL void Solution_SolveSnap(void);

    ALTDSS_CAPI_DLL void Solution_CheckControls(void);

    ALTDSS_CAPI_DLL void Solution_InitSnap(void);

    /*! 
    Flag that indicates if elements of the System Y have been changed by recent activity.
    */
    ALTDSS_CAPI_DLL uint16_t Solution_Get_SystemYChanged(void);

    ALTDSS_CAPI_DLL void Solution_BuildYMatrix(int32_t BuildOption, int32_t AllocateVI);

    ALTDSS_CAPI_DLL void Solution_DoControlActions(void);

    ALTDSS_CAPI_DLL void Solution_SampleControlDevices(void);

    /*! 
    Flag to indicate whether the circuit solution converged
    */
    ALTDSS_CAPI_DLL uint16_t Solution_Get_Converged(void);

    /*! 
    Flag to indicate whether the circuit solution converged
    */
    ALTDSS_CAPI_DLL void Solution_Set_Converged(uint16_t Value);

    /*! 
    Total iterations including control iterations for most recent solution.
    */
    ALTDSS_CAPI_DLL int32_t Solution_Get_Totaliterations(void);

    /*! 
    Max number of iterations required to converge at any control iteration of the most recent solution.
    */
    ALTDSS_CAPI_DLL int32_t Solution_Get_MostIterationsDone(void);

    /*! 
    Flag indicating the control actions are done.
    */
    ALTDSS_CAPI_DLL uint16_t Solution_Get_ControlActionsDone(void);

    ALTDSS_CAPI_DLL void Solution_Set_ControlActionsDone(uint16_t Value);

    ALTDSS_CAPI_DLL void Solution_Cleanup(void);

    ALTDSS_CAPI_DLL void Solution_FinishTimeStep(void);

    /*! 
    Gets the time required to perform the latest solution (Read only)
    */
    ALTDSS_CAPI_DLL double Solution_Get_Process_Time(void);

    /*! 
    Gets the accumulated time of the simulation
    */
    ALTDSS_CAPI_DLL double Solution_Get_Total_Time(void);

    /*! 
    Sets the Accumulated time of the simulation
    */
    ALTDSS_CAPI_DLL void Solution_Set_Total_Time(double Value);

    /*! 
    Get the solution process time + sample time for time step
    */
    ALTDSS_CAPI_DLL double Solution_Get_Time_of_Step(void);

    /*! 
    Get/Set the Solution.IntervalHrs variable used for devices that integrate
    */
    ALTDSS_CAPI_DLL double Solution_Get_IntervalHrs(void);

    /*! 
    Get/Set the Solution.IntervalHrs variable for custom solution algorithms
    */
    ALTDSS_CAPI_DLL void Solution_Set_IntervalHrs(double Value);

    /*! 
    Minimum number of iterations required for a power flow solution.
    */
    ALTDSS_CAPI_DLL int32_t Solution_Get_MinIterations(void);

    /*! 
    Mininum number of iterations required for a power flow solution.
    */
    ALTDSS_CAPI_DLL void Solution_Set_MinIterations(int32_t Value);

    /*! 
    Solves the circuits for all the Actors created
    */
    ALTDSS_CAPI_DLL void Solution_SolveAll(void);

    ALTDSS_CAPI_DLL void Solution_Get_IncMatrix(int32_t** ResultPtr, int32_t* ResultDims);

    /*! 
    Same as Solution_Get_IncMatrix but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Solution_Get_IncMatrix_GR(void);

    ALTDSS_CAPI_DLL void Solution_Get_Laplacian(int32_t** ResultPtr, int32_t* ResultDims);

    /*! 
    Same as Solution_Get_Laplacian but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Solution_Get_Laplacian_GR(void);

    ALTDSS_CAPI_DLL void Solution_Get_BusLevels(int32_t** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Solution_Get_BusLevels but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Solution_Get_BusLevels_GR(void);

    ALTDSS_CAPI_DLL void Solution_Get_IncMatrixRows(char*** ResultPtr, int32_t* ResultDims);

    ALTDSS_CAPI_DLL void Solution_Get_IncMatrixCols(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Open or Close the switch. No effect if switch is locked.  However, Reset removes any lock and then closes the switch (shelf state).
    */
    ALTDSS_CAPI_DLL int32_t SwtControls_Get_Action(void);

    /*! 
    Array of strings with all SwtControl names in the active circuit.
    */
    ALTDSS_CAPI_DLL void SwtControls_Get_AllNames(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Time delay [s] between arming and opening or closing the switch.  Control may reset before actually operating the switch.
    */
    ALTDSS_CAPI_DLL double SwtControls_Get_Delay(void);

    /*! 
    Sets the first SwtControl active. Returns 0 if no more.
    */
    ALTDSS_CAPI_DLL int32_t SwtControls_Get_First(void);

    /*! 
    The lock prevents both manual and automatic switch operation.
    */
    ALTDSS_CAPI_DLL uint16_t SwtControls_Get_IsLocked(void);

    /*! 
    Sets a SwtControl active by Name.
    */
    ALTDSS_CAPI_DLL const char* SwtControls_Get_Name(void);

    /*! 
    Sets the next SwtControl active. Returns 0 if no more.
    */
    ALTDSS_CAPI_DLL int32_t SwtControls_Get_Next(void);

    /*! 
    Full name of the switched element.
    */
    ALTDSS_CAPI_DLL const char* SwtControls_Get_SwitchedObj(void);

    /*! 
    Terminal number where the switch is located on the SwitchedObj
    */
    ALTDSS_CAPI_DLL int32_t SwtControls_Get_SwitchedTerm(void);

    /*! 
    Open or Close the switch. No effect if switch is locked.  However, Reset removes any lock and then closes the switch (shelf state).
    */
    ALTDSS_CAPI_DLL void SwtControls_Set_Action(int32_t Value);

    /*! 
    Time delay [s] between arming and opening or closing the switch.  Control may reset before actually operating the switch.
    */
    ALTDSS_CAPI_DLL void SwtControls_Set_Delay(double Value);

    /*! 
    The lock prevents both manual and automatic switch operation.
    */
    ALTDSS_CAPI_DLL void SwtControls_Set_IsLocked(uint16_t Value);

    /*! 
    Sets a SwtControl active by Name.
    */
    ALTDSS_CAPI_DLL void SwtControls_Set_Name(const char* Value);

    /*! 
    Full name of the switched element.
    */
    ALTDSS_CAPI_DLL void SwtControls_Set_SwitchedObj(const char* Value);

    /*! 
    Terminal number where the switch is located on the SwitchedObj
    */
    ALTDSS_CAPI_DLL void SwtControls_Set_SwitchedTerm(int32_t Value);

    ALTDSS_CAPI_DLL int32_t SwtControls_Get_Count(void);

    /*! 
    Normal state of switch (see ActionCodes enum), ActionOpen or ActionClose
    */
    ALTDSS_CAPI_DLL int32_t SwtControls_Get_NormalState(void);

    ALTDSS_CAPI_DLL void SwtControls_Set_NormalState(int32_t Value);

    /*! 
    Force switch to specified state
    */
    ALTDSS_CAPI_DLL int32_t SwtControls_Get_State(void);

    /*! 
    Get Present state of switch
    */
    ALTDSS_CAPI_DLL void SwtControls_Set_State(int32_t Value);

    ALTDSS_CAPI_DLL void SwtControls_Reset(void);

    /*! 
    Input command string for the DSS.
    */
    ALTDSS_CAPI_DLL const char* Text_Get_Command(void);

    /*! 
    Input command string for the DSS.

    Since November 2024, this function also accepts multiple commands, separated by line breaks. The 
    results for each command, if not not empty, are collected and joined by line breaks in a big string,
    make available in the `Text.Result` API (`Text_Get_Result` in the low-level API).

    If you don't need the output of the individual commands, use the `Text.Commands` function or 
    equivalent (`Text_CommandBlock` in the low-level API). That alternative can provide more
    precise error messages, and can handle block comments.
    */
    ALTDSS_CAPI_DLL void Text_Set_Command(const char* Value);

    /*! 
    Result string for the last command.
    */
    ALTDSS_CAPI_DLL const char* Text_Get_Result(void);

    /*! 
    Number of loops
    */
    ALTDSS_CAPI_DLL int32_t Topology_Get_NumLoops(void);

    /*! 
    Returns index of the active branch
    */
    ALTDSS_CAPI_DLL int32_t Topology_Get_ActiveBranch(void);

    /*! 
    Array of all isolated branch names.
    */
    ALTDSS_CAPI_DLL void Topology_Get_AllIsolatedBranches(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Array of all looped element names, by pairs.
    */
    ALTDSS_CAPI_DLL void Topology_Get_AllLoopedPairs(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    MOve back toward the source, return index of new active branch, or 0 if no more.
    */
    ALTDSS_CAPI_DLL int32_t Topology_Get_BackwardBranch(void);

    /*! 
    Name of the active branch.
    */
    ALTDSS_CAPI_DLL const char* Topology_Get_BranchName(void);

    /*! 
    Sets the first branch active, returns 0 if none.
    */
    ALTDSS_CAPI_DLL int32_t Topology_Get_First(void);

    /*! 
    Move forward in the tree, return index of new active branch or 0 if no more
    */
    ALTDSS_CAPI_DLL int32_t Topology_Get_ForwardBranch(void);

    /*! 
    Move to looped branch, return index or 0 if none.
    */
    ALTDSS_CAPI_DLL int32_t Topology_Get_LoopedBranch(void);

    /*! 
    Sets the next branch active, returns 0 if no more.
    */
    ALTDSS_CAPI_DLL int32_t Topology_Get_Next(void);

    /*! 
    Number of isolated branches (PD elements and capacitors).
    */
    ALTDSS_CAPI_DLL int32_t Topology_Get_NumIsolatedBranches(void);

    /*! 
    Move to directly parallel branch, return index or 0 if none.
    */
    ALTDSS_CAPI_DLL int32_t Topology_Get_ParallelBranch(void);

    ALTDSS_CAPI_DLL void Topology_Set_BranchName(const char* Value);

    /*! 
    Array of all isolated load names.
    */
    ALTDSS_CAPI_DLL void Topology_Get_AllIsolatedLoads(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    First load at the active branch, return index or 0 if none.
    */
    ALTDSS_CAPI_DLL int32_t Topology_Get_FirstLoad(void);

    /*! 
    Next load at the active branch, return index or 0 if no more.
    */
    ALTDSS_CAPI_DLL int32_t Topology_Get_NextLoad(void);

    /*! 
    Number of isolated loads
    */
    ALTDSS_CAPI_DLL int32_t Topology_Get_NumIsolatedLoads(void);

    /*! 
    Topological depth of the active branch
    */
    ALTDSS_CAPI_DLL int32_t Topology_Get_ActiveLevel(void);

    ALTDSS_CAPI_DLL const char* Topology_Get_BusName(void);

    /*! 
    Set the active branch to one containing this bus, return index or 0 if not found
    */
    ALTDSS_CAPI_DLL void Topology_Set_BusName(const char* Value);

    /*! 
    Array of strings with all Transformer names in the active circuit.
    */
    ALTDSS_CAPI_DLL void Transformers_Get_AllNames(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Sets the first Transformer active. Returns 0 if no more.
    */
    ALTDSS_CAPI_DLL int32_t Transformers_Get_First(void);

    /*! 
    Active Winding delta or wye connection?
    */
    ALTDSS_CAPI_DLL uint16_t Transformers_Get_IsDelta(void);

    /*! 
    Active Winding kV rating.  Phase-phase for 2 or 3 phases, actual winding kV for 1 phase transformer.
    */
    ALTDSS_CAPI_DLL double Transformers_Get_kV(void);

    /*! 
    Active Winding kVA rating. On winding 1, this also determines normal and emergency current ratings for all windings.
    */
    ALTDSS_CAPI_DLL double Transformers_Get_kVA(void);

    /*! 
    Active Winding maximum tap in per-unit.
    */
    ALTDSS_CAPI_DLL double Transformers_Get_MaxTap(void);

    /*! 
    Active Winding minimum tap in per-unit.
    */
    ALTDSS_CAPI_DLL double Transformers_Get_MinTap(void);

    /*! 
    Sets a Transformer active by Name.
    */
    ALTDSS_CAPI_DLL const char* Transformers_Get_Name(void);

    /*! 
    Sets the next Transformer active. Returns 0 if no more.
    */
    ALTDSS_CAPI_DLL int32_t Transformers_Get_Next(void);

    /*! 
    Active Winding number of tap steps between MinTap and MaxTap.
    */
    ALTDSS_CAPI_DLL int32_t Transformers_Get_NumTaps(void);

    /*! 
    Number of windings on this transformer. Allocates memory; set or change this property first.
    */
    ALTDSS_CAPI_DLL int32_t Transformers_Get_NumWindings(void);

    /*! 
    Active Winding resistance in %
    */
    ALTDSS_CAPI_DLL double Transformers_Get_R(void);

    /*! 
    Active Winding neutral resistance [ohms] for wye connections. Set less than zero for ungrounded wye.
    */
    ALTDSS_CAPI_DLL double Transformers_Get_Rneut(void);

    /*! 
    Active Winding tap in per-unit.
    */
    ALTDSS_CAPI_DLL double Transformers_Get_Tap(void);

    /*! 
    Active Winding Number from 1..NumWindings. Update this before reading or setting a sequence of winding properties (R, Tap, kV, kVA, etc.)
    */
    ALTDSS_CAPI_DLL int32_t Transformers_Get_Wdg(void);

    /*! 
    Name of an XfrmCode that supplies electrical parameters for this Transformer.
    */
    ALTDSS_CAPI_DLL const char* Transformers_Get_XfmrCode(void);

    /*! 
    Percent reactance between windings 1 and 2, on winding 1 kVA base. Use for 2-winding or 3-winding transformers.
    */
    ALTDSS_CAPI_DLL double Transformers_Get_Xhl(void);

    /*! 
    Percent reactance between windings 1 and 3, on winding 1 kVA base.  Use for 3-winding transformers only.
    */
    ALTDSS_CAPI_DLL double Transformers_Get_Xht(void);

    /*! 
    Percent reactance between windings 2 and 3, on winding 1 kVA base. Use for 3-winding transformers only.
    */
    ALTDSS_CAPI_DLL double Transformers_Get_Xlt(void);

    /*! 
    Active Winding neutral reactance [ohms] for wye connections.
    */
    ALTDSS_CAPI_DLL double Transformers_Get_Xneut(void);

    /*! 
    Active Winding delta or wye connection?
    */
    ALTDSS_CAPI_DLL void Transformers_Set_IsDelta(uint16_t Value);

    /*! 
    Active Winding kV rating.  Phase-phase for 2 or 3 phases, actual winding kV for 1 phase transformer.
    */
    ALTDSS_CAPI_DLL void Transformers_Set_kV(double Value);

    /*! 
    Active Winding kVA rating. On winding 1, this also determines normal and emergency current ratings for all windings.
    */
    ALTDSS_CAPI_DLL void Transformers_Set_kVA(double Value);

    /*! 
    Active Winding maximum tap in per-unit.
    */
    ALTDSS_CAPI_DLL void Transformers_Set_MaxTap(double Value);

    /*! 
    Active Winding minimum tap in per-unit.
    */
    ALTDSS_CAPI_DLL void Transformers_Set_MinTap(double Value);

    /*! 
    Sets a Transformer active by Name.
    */
    ALTDSS_CAPI_DLL void Transformers_Set_Name(const char* Value);

    /*! 
    Active Winding number of tap steps between MinTap and MaxTap.
    */
    ALTDSS_CAPI_DLL void Transformers_Set_NumTaps(int32_t Value);

    /*! 
    Number of windings on this transformer. Allocates memory; set or change this property first.
    */
    ALTDSS_CAPI_DLL void Transformers_Set_NumWindings(int32_t Value);

    /*! 
    Active Winding resistance in %
    */
    ALTDSS_CAPI_DLL void Transformers_Set_R(double Value);

    /*! 
    Active Winding neutral resistance [ohms] for wye connections. Set less than zero for ungrounded wye.
    */
    ALTDSS_CAPI_DLL void Transformers_Set_Rneut(double Value);

    /*! 
    Active Winding tap in per-unit.
    */
    ALTDSS_CAPI_DLL void Transformers_Set_Tap(double Value);

    /*! 
    Active Winding Number from 1..NumWindings. Update this before reading or setting a sequence of winding properties (R, Tap, kV, kVA, etc.)
    */
    ALTDSS_CAPI_DLL void Transformers_Set_Wdg(int32_t Value);

    /*! 
    Name of an XfrmCode that supplies electrical parameters for this Transformer.
    */
    ALTDSS_CAPI_DLL void Transformers_Set_XfmrCode(const char* Value);

    /*! 
    Percent reactance between windings 1 and 2, on winding 1 kVA base. Use for 2-winding or 3-winding transformers.
    */
    ALTDSS_CAPI_DLL void Transformers_Set_Xhl(double Value);

    /*! 
    Percent reactance between windings 1 and 3, on winding 1 kVA base.  Use for 3-winding transformers only.
    */
    ALTDSS_CAPI_DLL void Transformers_Set_Xht(double Value);

    /*! 
    Percent reactance between windings 2 and 3, on winding 1 kVA base. Use for 3-winding transformers only.
    */
    ALTDSS_CAPI_DLL void Transformers_Set_Xlt(double Value);

    /*! 
    Active Winding neutral reactance [ohms] for wye connections.
    */
    ALTDSS_CAPI_DLL void Transformers_Set_Xneut(double Value);

    ALTDSS_CAPI_DLL int32_t Transformers_Get_Count(void);

    /*! 
    Complex array of voltages for active winding

    WARNING: If the transformer has open terminal(s), results may be wrong, i.e. avoid using this
    in those situations. For more information, see https://github.com/dss-extensions/dss-extensions/issues/24
    */
    ALTDSS_CAPI_DLL void Transformers_Get_WdgVoltages(double** ResultPtr, int32_t* ResultDims);

    /*! 
    Same as Transformers_Get_WdgVoltages but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Transformers_Get_WdgVoltages_GR(void);

    /*! 
    All Winding currents (ph1, wdg1, wdg2,... ph2, wdg1, wdg2 ...)

    WARNING: If the transformer has open terminal(s), results may be wrong, i.e. avoid using this
    in those situations. For more information, see https://github.com/dss-extensions/dss-extensions/issues/24
    */
    ALTDSS_CAPI_DLL void Transformers_Get_WdgCurrents(double** ResultPtr, int32_t* ResultDims);

    /*! 
    Same as Transformers_Get_WdgCurrents but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Transformers_Get_WdgCurrents_GR(void);

    /*! 
    All winding currents in CSV string form like the WdgCurrents property

    WARNING: If the transformer has open terminal(s), results may be wrong, i.e. avoid using this
    in those situations. For more information, see https://github.com/dss-extensions/dss-extensions/issues/24
    */
    ALTDSS_CAPI_DLL const char* Transformers_Get_strWdgCurrents(void);

    /*! 
    Transformer Core Type: 0=shell;1 = 1-phase; 3= 3-leg; 5= 5-leg
    */
    ALTDSS_CAPI_DLL int32_t Transformers_Get_CoreType(void);
    ALTDSS_CAPI_DLL void Transformers_Set_CoreType(int32_t Value);

    /*! 
    dc Resistance of active winding in ohms for GIC analysis
    */
    ALTDSS_CAPI_DLL double Transformers_Get_RdcOhms(void);
    ALTDSS_CAPI_DLL void Transformers_Set_RdcOhms(double Value);

    /*! 
    Returns a complex array of the 3 types of losses (total losses, load losses, no-load losses) for the active transformer, in VA
    */
    ALTDSS_CAPI_DLL void Transformers_Get_LossesByType(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Transformers_Get_LossesByType but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Transformers_Get_LossesByType_GR(void);

    /*! 
    Returns a complex array of the 3 types of losses (total losses, load losses, no-load losses) concatenated for the all transformers, in VA
    */
    ALTDSS_CAPI_DLL void Transformers_Get_AllLossesByType(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Transformers_Get_AllLossesByType but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Transformers_Get_AllLossesByType_GR(void);

    /*! 
    Names of all Vsource objects in the circuit
    */
    ALTDSS_CAPI_DLL void Vsources_Get_AllNames(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Number of Vsource Object
    */
    ALTDSS_CAPI_DLL int32_t Vsources_Get_Count(void);

    /*! 
    Sets the first VSOURCE to be active; Returns 0 if none
    */
    ALTDSS_CAPI_DLL int32_t Vsources_Get_First(void);

    /*! 
    Sets the next VSOURCE object to be active; returns zero if no more
    */
    ALTDSS_CAPI_DLL int32_t Vsources_Get_Next(void);

    /*! 
    Get Active VSOURCE name
    */
    ALTDSS_CAPI_DLL const char* Vsources_Get_Name(void);

    /*! 
    Set Active VSOURCE by Name
    */
    ALTDSS_CAPI_DLL void Vsources_Set_Name(const char* Value);

    /*! 
    Source Voltage in kV
    */
    ALTDSS_CAPI_DLL double Vsources_Get_BasekV(void);

    /*! 
    Source pu voltage.
    */
    ALTDSS_CAPI_DLL double Vsources_Get_pu(void);

    /*! 
    Source voltage in kV
    */
    ALTDSS_CAPI_DLL void Vsources_Set_BasekV(double Value);

    /*! 
    Per-unit value of source voltage based on kV
    */
    ALTDSS_CAPI_DLL void Vsources_Set_pu(double Value);

    /*! 
    Phase angle of first phase in degrees
    */
    ALTDSS_CAPI_DLL double Vsources_Get_AngleDeg(void);

    /*! 
    Source Frequency in Hz
    */
    ALTDSS_CAPI_DLL double Vsources_Get_Frequency(void);

    /*! 
    Number of Phases
    */
    ALTDSS_CAPI_DLL int32_t Vsources_Get_Phases(void);

    /*! 
    phase angle in degrees
    */
    ALTDSS_CAPI_DLL void Vsources_Set_AngleDeg(double Value);

    /*! 
    Source frequency in Hz
    */
    ALTDSS_CAPI_DLL void Vsources_Set_Frequency(double Value);

    /*! 
    Number of phases
    */
    ALTDSS_CAPI_DLL void Vsources_Set_Phases(int32_t Value);


    /*! 
    Array of strings with all XYCurve names.

    (API Extension)
    */
    ALTDSS_CAPI_DLL void XYCurves_Get_AllNames(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Number of XYCurve Objects
    */
    ALTDSS_CAPI_DLL int32_t XYCurves_Get_Count(void);

    /*! 
    Sets first XYcurve object active; returns 0 if none.
    */
    ALTDSS_CAPI_DLL int32_t XYCurves_Get_First(void);

    /*! 
    Name of active XYCurve Object
    */
    ALTDSS_CAPI_DLL const char* XYCurves_Get_Name(void);

    /*! 
    Advances to next XYCurve object; returns 0 if no more objects of this class
    */
    ALTDSS_CAPI_DLL int32_t XYCurves_Get_Next(void);

    /*! 
    Get Name of active XYCurve Object
    */
    ALTDSS_CAPI_DLL void XYCurves_Set_Name(const char* Value);

    /*! 
    Get/Set Number of points in X-Y curve
    */
    ALTDSS_CAPI_DLL int32_t XYCurves_Get_Npts(void);

    /*! 
    Get X values as an array of doubles.
    */
    ALTDSS_CAPI_DLL void XYCurves_Get_Xarray(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as XYCurves_Get_Xarray but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void XYCurves_Get_Xarray_GR(void);

    /*! 
    Get/Set Number of Points in X-Y curve
    */
    ALTDSS_CAPI_DLL void XYCurves_Set_Npts(int32_t Value);

    /*! 
    Set X values as an array of doubles. Remember to first set Npts to max number expected values.
    */
    ALTDSS_CAPI_DLL void XYCurves_Set_Xarray(const double* ValuePtr, int32_t ValueCount);

    /*! 
    Set X value or get interpolated value after setting Y
    */
    ALTDSS_CAPI_DLL double XYCurves_Get_x(void);

    /*! 
    Y value for present X or set this value then get corresponding X
    */
    ALTDSS_CAPI_DLL double XYCurves_Get_y(void);

    /*! 
    Get Y values in curve as an array of doubles
    */
    ALTDSS_CAPI_DLL void XYCurves_Get_Yarray(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as XYCurves_Get_Yarray but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void XYCurves_Get_Yarray_GR(void);

    ALTDSS_CAPI_DLL void XYCurves_Set_x(double Value);

    /*! 
    Set Y value or get interpolated Y value after setting X
    */
    ALTDSS_CAPI_DLL void XYCurves_Set_y(double Value);

    /*! 
    Set Y values as an array of doubles. Remember to first set Npts to max number expected values.
    */
    ALTDSS_CAPI_DLL void XYCurves_Set_Yarray(const double* ValuePtr, int32_t ValueCount);

    /*! 
    Factor to scale X values from original curve
    */
    ALTDSS_CAPI_DLL double XYCurves_Get_Xscale(void);

    /*! 
    Amount to shift X value from original curve
    */
    ALTDSS_CAPI_DLL double XYCurves_Get_Xshift(void);

    /*! 
    Factor to scale Y values from original curve
    */
    ALTDSS_CAPI_DLL double XYCurves_Get_Yscale(void);

    /*! 
    amount to shift Y valiue from original curve
    */
    ALTDSS_CAPI_DLL double XYCurves_Get_Yshift(void);

    /*! 
    Factor to scale X values from original curve
    */
    ALTDSS_CAPI_DLL void XYCurves_Set_Xscale(double Value);

    ALTDSS_CAPI_DLL void XYCurves_Set_Xshift(double Value);

    /*! 
    Amount to scale Y values from original curve. Represents a curve shift.
    */
    ALTDSS_CAPI_DLL void XYCurves_Set_Yscale(double Value);

    ALTDSS_CAPI_DLL void XYCurves_Set_Yshift(double Value);

    ALTDSS_CAPI_DLL void YMatrix_GetCompressedYMatrix(uint16_t factor, uint32_t *nBus, uint32_t *nNz, int32_t **ColPtr, int32_t **RowIdxPtr, double **cValsPtr);
    ALTDSS_CAPI_DLL void YMatrix_ZeroInjCurr(void);
    ALTDSS_CAPI_DLL void YMatrix_GetSourceInjCurrents(void);
    ALTDSS_CAPI_DLL void YMatrix_GetPCInjCurr(void);
    ALTDSS_CAPI_DLL void YMatrix_BuildYMatrixD(int32_t BuildOps, int32_t AllocateVI);
    ALTDSS_CAPI_DLL void YMatrix_AddInAuxCurrents(int32_t SType);
    ALTDSS_CAPI_DLL void YMatrix_getIpointer(double **IvectorPtr);
    ALTDSS_CAPI_DLL void YMatrix_getVpointer(double **VvectorPtr);
    ALTDSS_CAPI_DLL int32_t YMatrix_SolveSystem(double *NodeVPtr);
    ALTDSS_CAPI_DLL void YMatrix_Set_SystemYChanged(uint16_t arg);
    ALTDSS_CAPI_DLL uint16_t YMatrix_Get_SystemYChanged(void);
    ALTDSS_CAPI_DLL void YMatrix_Set_UseAuxCurrents(uint16_t arg);
    ALTDSS_CAPI_DLL uint16_t YMatrix_Get_UseAuxCurrents(void);


    /*! 
    Zmag (ohms) for Reduce Option for Z of short lines
    */
    ALTDSS_CAPI_DLL double ReduceCkt_Get_Zmag(void);
    ALTDSS_CAPI_DLL void ReduceCkt_Set_Zmag(double Value);

    /*! 
    Keep load flag (T/F) for Reduction options that remove branches
    */
    ALTDSS_CAPI_DLL uint16_t ReduceCkt_Get_KeepLoad(void);
    ALTDSS_CAPI_DLL void ReduceCkt_Set_KeepLoad(uint16_t Value);

    /*! 
    Edit String for RemoveBranches functions
    */
    ALTDSS_CAPI_DLL const char* ReduceCkt_Get_EditString(void);
    ALTDSS_CAPI_DLL void ReduceCkt_Set_EditString(const char* Value);

    /*! 
    Start element for Remove Branch function
    */
    ALTDSS_CAPI_DLL const char* ReduceCkt_Get_StartPDElement(void);
    ALTDSS_CAPI_DLL void ReduceCkt_Set_StartPDElement(const char* Value);

    /*! 
    Name of EnergyMeter to use for reduction
    */
    ALTDSS_CAPI_DLL const char* ReduceCkt_Get_EnergyMeter(void);
    ALTDSS_CAPI_DLL void ReduceCkt_Set_EnergyMeter(const char* Value);

    /*! 
    Save present (reduced) circuit
    Filename is listed in the Text Result interface
    */
    ALTDSS_CAPI_DLL void ReduceCkt_SaveCircuit(const char* CktName);

    /*! 
    Do Default Reduction algorithm
    */
    ALTDSS_CAPI_DLL void ReduceCkt_DoDefault(void);

    /*! 
    Do ShortLines algorithm: Set Zmag first if you don't want the default
    */
    ALTDSS_CAPI_DLL void ReduceCkt_DoShortLines(void);

    /*! 
    Reduce Dangling Algorithm; branches with nothing connected
    */
    ALTDSS_CAPI_DLL void ReduceCkt_DoDangling(void);

    /*!
    Break (disable) all the loops found in the active circuit.

    Disables one of the Line objects at the head of a loop to force the circuit to be radial.
    */
    ALTDSS_CAPI_DLL void ReduceCkt_DoLoopBreak(void);

    /*!
    Merge all parallel lines found in the circuit to facilitate its reduction.
    */
    ALTDSS_CAPI_DLL void ReduceCkt_DoParallelLines(void);

    /*!
    Merge Line objects in which the IsSwitch property is true with the down-line Line object.
    */
    ALTDSS_CAPI_DLL void ReduceCkt_DoSwitches(void);

    /*!
    Remove all 1-phase laterals in the active EnergyMeter's zone.

    Loads and other shunt elements are moved to the parent 3-phase bus.
    */
    ALTDSS_CAPI_DLL void ReduceCkt_Do1phLaterals(void);

    /*!
    Remove (disable) all branches down-line from the active PDElement. 
    
    Circuit must have an EnergyMeter on this branch.
    If KeepLoad=Y (default), a new Load element is defined and kW, kvar are set to present power flow solution for the first element eliminated. 
    The EditString is applied to each new Load element defined. 
    */
    ALTDSS_CAPI_DLL void ReduceCkt_DoBranchRemove(void);

    /*! 
    Array of names of all Storage objects.
    */
    ALTDSS_CAPI_DLL void Storages_Get_AllNames(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Sets first Storage to be active.  Returns 0 if none.
    */
    ALTDSS_CAPI_DLL int32_t Storages_Get_First(void);

    /*! 
    Sets next Storage to be active.  Returns 0 if no more.
    */
    ALTDSS_CAPI_DLL int32_t Storages_Get_Next(void);

    /*! 
    Number of Storage Objects in Active Circuit
    */
    ALTDSS_CAPI_DLL int32_t Storages_Get_Count(void);

    /*! 
    Get/Set active Storage by index into Storages list.  1..Count
    */
    ALTDSS_CAPI_DLL int32_t Storages_Get_idx(void);

    /*! 
    Get/Set active Storage by index into Storages list. 1..Count
    */
    ALTDSS_CAPI_DLL void Storages_Set_idx(int32_t Value);

    /*! 
    Gets the name of the current active Storage object.
    */
    ALTDSS_CAPI_DLL const char* Storages_Get_Name(void);

    /*! 
    Sets a Storage active by name.
    */
    ALTDSS_CAPI_DLL void Storages_Set_Name(const char* Value);

    /*! 
    Array of Storage energy meter register names
    
    See also the enum `GeneratorRegisters`.
    */
    ALTDSS_CAPI_DLL void Storages_Get_RegisterNames(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Array of values in Storage registers.
    */
    ALTDSS_CAPI_DLL void Storages_Get_RegisterValues(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as Storages_Get_RegisterValues but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Storages_Get_RegisterValues_GR(void);

    /*! 
    Per unit state of charge
    */
    ALTDSS_CAPI_DLL double Storages_Get_puSOC(void);

    /*! 
    Per unit state of charge
    */
    ALTDSS_CAPI_DLL void Storages_Set_puSOC(double Value);

    /*! 
    Get/set state: 0=Idling; 1=Discharging; -1=Charging;

    Related enumeration: StorageStates
    */
    ALTDSS_CAPI_DLL int32_t Storages_Get_State(void);

    /*! 
    Get/set state: 0=Idling; 1=Discharging; -1=Charging;

    Related enumeration: StorageStates
    */
    ALTDSS_CAPI_DLL void Storages_Set_State(int32_t Value);

    /*! 
    Current limit per phase for the IBR when operating in GFM mode.
    */
    ALTDSS_CAPI_DLL double Storages_Get_AmpLimit(void);

    /*! 
    Current limit per phase for the IBR when operating in GFM mode.
    */
    ALTDSS_CAPI_DLL void Storages_Set_AmpLimit(double Value);

    /*! 
    Use it for fine tuning the current limiter when active.
    */
    ALTDSS_CAPI_DLL double Storages_Get_AmpLimitGain(void);

    /*! 
    Use it for fine tuning the current limiter when active.
    */
    ALTDSS_CAPI_DLL void Storages_Set_AmpLimitGain(double Value);

    /*! 
    Dispatch trigger value for charging the Storage.
    */
    ALTDSS_CAPI_DLL double Storages_Get_ChargeTrigger(void);

    /*! 
    Dispatch trigger value for charging the Storage.
    */
    ALTDSS_CAPI_DLL void Storages_Set_ChargeTrigger(double Value);

    /*! 
    Control mode for the inverter. It can be one of {GFM = 1 | GFL* = 0}.
    */
    ALTDSS_CAPI_DLL int32_t Storages_Get_ControlMode(void);

    /*! 
    Control mode for the inverter. It can be one of {GFM = 1 | GFL* = 0}.
    */
    ALTDSS_CAPI_DLL void Storages_Set_ControlMode(int32_t Value);

    /*! 
    Dispatch trigger value for discharging the Storage.
    */
    ALTDSS_CAPI_DLL double Storages_Get_DischargeTrigger(void);

    /*! 
    Dispatch trigger value for discharging the Storage.
    */
    ALTDSS_CAPI_DLL void Storages_Set_DischargeTrigger(double Value);

    /*! 
    Percentage efficiency for CHARGING the Storage element.
    */
    ALTDSS_CAPI_DLL double Storages_Get_EffCharge(void);

    /*! 
    Percentage efficiency for CHARGING the Storage element.
    */
    ALTDSS_CAPI_DLL void Storages_Set_EffCharge(double Value);

    /*! 
    Percentage efficiency for DISCHARGING the Storage element.
    */
    ALTDSS_CAPI_DLL double Storages_Get_EffDischarge(void);

    /*! 
    Percentage efficiency for DISCHARGING the Storage element.
    */
    ALTDSS_CAPI_DLL void Storages_Set_EffDischarge(double Value);

    /*! 
    Proportional gain for the PI controller within the inverter.
    Use it to modify the controller response in dynamics simulation mode.
    */
    ALTDSS_CAPI_DLL double Storages_Get_Kp(void);

    /*! 
    Proportional gain for the PI controller within the inverter.
    Use it to modify the controller response in dynamics simulation mode.
    */
    ALTDSS_CAPI_DLL void Storages_Set_Kp(double Value);

    /*! 
    Nominal rated (1.0 per unit) voltage, kV, for Storage element.
    */
    ALTDSS_CAPI_DLL double Storages_Get_kV(void);

    /*! 
    Nominal rated (1.0 per unit) voltage, kV, for Storage element.
    */
    ALTDSS_CAPI_DLL void Storages_Set_kV(double Value);

    /*! 
    Inverter nameplate capability (in kVA). Used as the base for Dynamics mode and Harmonics mode values.
    */
    ALTDSS_CAPI_DLL double Storages_Get_kVA(void);

    /*! 
    Inverter nameplate capability (in kVA). Used as the base for Dynamics mode and Harmonics mode values.
    */
    ALTDSS_CAPI_DLL void Storages_Set_kVA(double Value);

    /*! 
    Get/set the requested kvar value. Final kvar is subjected to the inverter ratings. Sets inverter to operate in constant kvar mode.

    **Note:** reading the DSS property `kvar` returns the adjusted value, while this returns the original input value.
    */
    ALTDSS_CAPI_DLL double Storages_Get_kvar(void);

    /*! 
    Get/set the requested kvar value. Final kvar is subjected to the inverter ratings. Sets inverter to operate in constant kvar mode.

    **Note:** reading the DSS property `kvar` returns the adjusted value, while this returns the original input value.
    */
    ALTDSS_CAPI_DLL void Storages_Set_kvar(double Value);

    /*! 
    Rated voltage (kV) at the input of the inverter while the storage is discharging
    */
    ALTDSS_CAPI_DLL double Storages_Get_kVDC(void);

    /*! 
    Rated voltage (kV) at the input of the inverter while the storage is discharging
    */
    ALTDSS_CAPI_DLL void Storages_Set_kVDC(double Value);

    /*! 
    Get/set the requested kW value. Final kW is subjected to the inverter ratings.
    */
    ALTDSS_CAPI_DLL double Storages_Get_kW(void);

    /*! 
    Get/set the requested kW value. Final kW is subjected to the inverter ratings.
    */
    ALTDSS_CAPI_DLL void Storages_Set_kW(double Value);

    /*! 
    Rated Storage capacity in kWh.
    */
    ALTDSS_CAPI_DLL double Storages_Get_kWhRated(void);

    /*! 
    Rated Storage capacity in kWh.
    */
    ALTDSS_CAPI_DLL void Storages_Set_kWhRated(double Value);

    /*! 
    kW rating of power output. Base for Loadshapes when DispMode=Follow. Sets kVA property if it has not been specified yet.
    */
    ALTDSS_CAPI_DLL double Storages_Get_kWRated(void);

    /*! 
    kW rating of power output. Base for Loadshapes when DispMode=Follow. Sets kVA property if it has not been specified yet.
    */
    ALTDSS_CAPI_DLL void Storages_Set_kWRated(double Value);

    /*! 
    Limits current magnitude to Vminpu value for both 1-phase and 3-phase Storage similar to Generator Model 7.
    For 3-phase, limits the positive-sequence current but not the negative-sequence."
    */
    ALTDSS_CAPI_DLL uint16_t Storages_Get_LimitCurrent(void);

    /*! 
    Limits current magnitude to Vminpu value for both 1-phase and 3-phase Storage similar to Generator Model 7.
    For 3-phase, limits the positive-sequence current but not the negative-sequence."
    */
    ALTDSS_CAPI_DLL void Storages_Set_LimitCurrent(uint16_t Value);

    /*! 
    Get/set the requested PF value.
    */
    ALTDSS_CAPI_DLL double Storages_Get_PF(void);

    /*! 
    Get/set the requested PF value.
    */
    ALTDSS_CAPI_DLL void Storages_Set_PF(double Value);

    /*! 
    Tolerance (%) for the closed loop controller of the inverter
    */
    ALTDSS_CAPI_DLL double Storages_Get_PITol(void);

    /*! 
    Tolerance (%) for the closed loop controller of the inverter
    */
    ALTDSS_CAPI_DLL void Storages_Set_PITol(double Value);

    /*! 
    (Read only) Indicates whether the inverter entered (Yes) or not (No) into Safe Mode.
    */
    ALTDSS_CAPI_DLL int32_t Storages_Get_SafeMode(void);

    /*! 
    Indicates the voltage level (%) respect to the base voltage level for which the Inverter will operate.
    */
    ALTDSS_CAPI_DLL double Storages_Get_SafeVoltage(void);

    /*! 
    Indicates the voltage level (%) respect to the base voltage level for which the Inverter will operate.
    */
    ALTDSS_CAPI_DLL void Storages_Set_SafeVoltage(double Value);

    /*! 
    Time of day in fractional hours (0230 = 2.5) at which Storage element will automatically go into charge state.
    */
    ALTDSS_CAPI_DLL double Storages_Get_TimeChargeTrig(void);

    /*! 
    Time of day in fractional hours (0230 = 2.5) at which Storage element will automatically go into charge state.
    */
    ALTDSS_CAPI_DLL void Storages_Set_TimeChargeTrig(double Value);

    /*! 
    Indicates if the reactive power generation/absorption does not respect the inverter status
    */
    ALTDSS_CAPI_DLL int32_t Storages_Get_VarFollowInverter(void);

    /*! 
    Indicates if the reactive power generation/absorption does not respect the inverter status
    */
    ALTDSS_CAPI_DLL void Storages_Set_VarFollowInverter(int32_t Value);


    // API extensions
    ALTDSS_CAPI_DLL int32_t CNData_Get_Count(void);
    ALTDSS_CAPI_DLL int32_t CNData_Get_First(void);
    ALTDSS_CAPI_DLL int32_t CNData_Get_Next(void);
    ALTDSS_CAPI_DLL const char* CNData_Get_Name(void);
    ALTDSS_CAPI_DLL void CNData_Set_Name(const char* Value);
    ALTDSS_CAPI_DLL void CNData_Get_AllNames(char*** ResultPtr, int32_t* ResultDims);
    ALTDSS_CAPI_DLL double CNData_Get_Rdc(void);
    ALTDSS_CAPI_DLL void CNData_Set_Rdc(double Value);
    ALTDSS_CAPI_DLL double CNData_Get_Rac(void);
    ALTDSS_CAPI_DLL void CNData_Set_Rac(double Value);
    ALTDSS_CAPI_DLL double CNData_Get_GMRac(void);
    ALTDSS_CAPI_DLL void CNData_Set_GMRac(double Value);
    ALTDSS_CAPI_DLL int32_t CNData_Get_GMRUnits(void);
    ALTDSS_CAPI_DLL void CNData_Set_GMRUnits(int32_t Value);
    ALTDSS_CAPI_DLL double CNData_Get_Radius(void);
    ALTDSS_CAPI_DLL void CNData_Set_Radius(double Value);
    ALTDSS_CAPI_DLL int32_t CNData_Get_RadiusUnits(void);
    ALTDSS_CAPI_DLL void CNData_Set_RadiusUnits(int32_t Value);
    ALTDSS_CAPI_DLL int32_t CNData_Get_ResistanceUnits(void);
    ALTDSS_CAPI_DLL void CNData_Set_ResistanceUnits(int32_t Value);
    ALTDSS_CAPI_DLL double CNData_Get_Diameter(void);
    ALTDSS_CAPI_DLL void CNData_Set_Diameter(double Value);
    ALTDSS_CAPI_DLL double CNData_Get_NormAmps(void);
    ALTDSS_CAPI_DLL void CNData_Set_NormAmps(double Value);
    ALTDSS_CAPI_DLL double CNData_Get_EmergAmps(void);
    ALTDSS_CAPI_DLL void CNData_Set_EmergAmps(double Value);
    ALTDSS_CAPI_DLL double CNData_Get_EpsR(void);
    ALTDSS_CAPI_DLL void CNData_Set_EpsR(double Value);
    ALTDSS_CAPI_DLL double CNData_Get_InsLayer(void);
    ALTDSS_CAPI_DLL void CNData_Set_InsLayer(double Value);
    ALTDSS_CAPI_DLL double CNData_Get_DiaIns(void);
    ALTDSS_CAPI_DLL void CNData_Set_DiaIns(double Value);
    ALTDSS_CAPI_DLL double CNData_Get_DiaCable(void);
    ALTDSS_CAPI_DLL void CNData_Set_DiaCable(double Value);
    ALTDSS_CAPI_DLL int32_t CNData_Get_k(void);
    ALTDSS_CAPI_DLL void CNData_Set_k(int32_t Value);
    ALTDSS_CAPI_DLL double CNData_Get_DiaStrand(void);
    ALTDSS_CAPI_DLL void CNData_Set_DiaStrand(double Value);
    ALTDSS_CAPI_DLL double CNData_Get_GmrStrand(void);
    ALTDSS_CAPI_DLL void CNData_Set_GmrStrand(double Value);
    ALTDSS_CAPI_DLL double CNData_Get_RStrand(void);
    ALTDSS_CAPI_DLL void CNData_Set_RStrand(double Value);

    ALTDSS_CAPI_DLL int32_t LineGeometries_Get_Count(void);
    ALTDSS_CAPI_DLL int32_t LineGeometries_Get_First(void);
    ALTDSS_CAPI_DLL int32_t LineGeometries_Get_Next(void);
    ALTDSS_CAPI_DLL const char* LineGeometries_Get_Name(void);
    ALTDSS_CAPI_DLL void LineGeometries_Set_Name(const char* Value);
    ALTDSS_CAPI_DLL int32_t LineGeometries_Get_Nconds(void);
    ALTDSS_CAPI_DLL void LineGeometries_Set_Nconds(int32_t Value);
    ALTDSS_CAPI_DLL int32_t LineGeometries_Get_Phases(void);
    ALTDSS_CAPI_DLL void LineGeometries_Set_Phases(int32_t Value);
    ALTDSS_CAPI_DLL void LineGeometries_Get_Cmatrix(double** ResultPtr, int32_t* ResultDims, double Frequency, double Length, int32_t Units);
    ALTDSS_CAPI_DLL void LineGeometries_Get_Cmatrix_GR(double Frequency, double Length, int32_t Units);
    ALTDSS_CAPI_DLL void LineGeometries_Get_Rmatrix(double** ResultPtr, int32_t* ResultDims, double Frequency, double Length, int32_t Units);
    ALTDSS_CAPI_DLL void LineGeometries_Get_Rmatrix_GR(double Frequency, double Length, int32_t Units);
    ALTDSS_CAPI_DLL void LineGeometries_Get_Xmatrix(double** ResultPtr, int32_t* ResultDims, double Frequency, double Length, int32_t Units);
    ALTDSS_CAPI_DLL void LineGeometries_Get_Xmatrix_GR(double Frequency, double Length, int32_t Units);
    ALTDSS_CAPI_DLL void LineGeometries_Get_Zmatrix(double** ResultPtr, int32_t* ResultDims, double Frequency, double Length, int32_t Units);
    ALTDSS_CAPI_DLL void LineGeometries_Get_Zmatrix_GR(double Frequency, double Length, int32_t Units);
    ALTDSS_CAPI_DLL void LineGeometries_Get_Units(int32_t** ResultPtr, int32_t* ResultDims);
    ALTDSS_CAPI_DLL void LineGeometries_Get_Units_GR(void);
    ALTDSS_CAPI_DLL void LineGeometries_Set_Units(const int32_t* ValuePtr, int32_t ValueCount);
    ALTDSS_CAPI_DLL void LineGeometries_Get_Xcoords(double** ResultPtr, int32_t* ResultDims);
    ALTDSS_CAPI_DLL void LineGeometries_Get_Xcoords_GR(void);
    ALTDSS_CAPI_DLL void LineGeometries_Set_Xcoords(const double* ValuePtr, int32_t ValueCount);
    ALTDSS_CAPI_DLL void LineGeometries_Get_Ycoords(double** ResultPtr, int32_t* ResultDims);
    ALTDSS_CAPI_DLL void LineGeometries_Get_Ycoords_GR(void);
    ALTDSS_CAPI_DLL void LineGeometries_Set_Ycoords(const double* ValuePtr, int32_t ValueCount);
    ALTDSS_CAPI_DLL void LineGeometries_Get_Conductors(char*** ResultPtr, int32_t* ResultDims);
    ALTDSS_CAPI_DLL uint16_t LineGeometries_Get_Reduce(void);
    ALTDSS_CAPI_DLL void LineGeometries_Set_Reduce(uint16_t Value);
    ALTDSS_CAPI_DLL double LineGeometries_Get_RhoEarth(void);
    ALTDSS_CAPI_DLL void LineGeometries_Set_RhoEarth(double Value);
    ALTDSS_CAPI_DLL double LineGeometries_Get_NormAmps(void);
    ALTDSS_CAPI_DLL void LineGeometries_Set_NormAmps(double Value);
    ALTDSS_CAPI_DLL double LineGeometries_Get_EmergAmps(void);
    ALTDSS_CAPI_DLL void LineGeometries_Set_EmergAmps(double Value);
    ALTDSS_CAPI_DLL void LineGeometries_Get_AllNames(char*** ResultPtr, int32_t* ResultDims);

    ALTDSS_CAPI_DLL int32_t LineSpacings_Get_Count(void);
    ALTDSS_CAPI_DLL int32_t LineSpacings_Get_First(void);
    ALTDSS_CAPI_DLL int32_t LineSpacings_Get_Next(void);
    ALTDSS_CAPI_DLL const char* LineSpacings_Get_Name(void);
    ALTDSS_CAPI_DLL void LineSpacings_Set_Name(const char* Value);
    ALTDSS_CAPI_DLL int32_t LineSpacings_Get_Nconds(void);
    ALTDSS_CAPI_DLL void LineSpacings_Set_Nconds(int32_t Value);
    ALTDSS_CAPI_DLL int32_t LineSpacings_Get_Phases(void);
    ALTDSS_CAPI_DLL void LineSpacings_Set_Phases(int32_t Value);
    ALTDSS_CAPI_DLL int32_t LineSpacings_Get_Units(void);
    ALTDSS_CAPI_DLL void LineSpacings_Set_Units(int32_t Value);
    ALTDSS_CAPI_DLL void LineSpacings_Get_Xcoords(double** ResultPtr, int32_t* ResultDims);
    ALTDSS_CAPI_DLL void LineSpacings_Get_Xcoords_GR(void);
    ALTDSS_CAPI_DLL void LineSpacings_Set_Xcoords(const double* ValuePtr, int32_t ValueCount);
    ALTDSS_CAPI_DLL void LineSpacings_Get_Ycoords(double** ResultPtr, int32_t* ResultDims);
    ALTDSS_CAPI_DLL void LineSpacings_Get_Ycoords_GR(void);
    ALTDSS_CAPI_DLL void LineSpacings_Set_Ycoords(const double* ValuePtr, int32_t ValueCount);
    ALTDSS_CAPI_DLL void LineSpacings_Get_AllNames(char*** ResultPtr, int32_t* ResultDims);

    ALTDSS_CAPI_DLL int32_t Loads_Get_Phases(void);
    ALTDSS_CAPI_DLL void Loads_Set_Phases(int32_t Value);

    ALTDSS_CAPI_DLL void Reactors_Get_AllNames(char*** ResultPtr, int32_t* ResultDims);
    ALTDSS_CAPI_DLL const char* Reactors_Get_Name(void);
    ALTDSS_CAPI_DLL void Reactors_Set_Name(const char* Value);
    ALTDSS_CAPI_DLL int32_t Reactors_Get_First(void);
    ALTDSS_CAPI_DLL int32_t Reactors_Get_Next(void);
    ALTDSS_CAPI_DLL int32_t Reactors_Get_Count(void);
    ALTDSS_CAPI_DLL double Reactors_Get_kV(void);
    ALTDSS_CAPI_DLL void Reactors_Set_kV(double Value);
    ALTDSS_CAPI_DLL double Reactors_Get_kvar(void);
    ALTDSS_CAPI_DLL void Reactors_Set_kvar(double Value);
    ALTDSS_CAPI_DLL int32_t Reactors_Get_Phases(void);
    ALTDSS_CAPI_DLL void Reactors_Set_Phases(int32_t Integer);
    ALTDSS_CAPI_DLL uint16_t Reactors_Get_IsDelta(void);
    ALTDSS_CAPI_DLL void Reactors_Set_IsDelta(uint16_t Value);
    ALTDSS_CAPI_DLL uint16_t Reactors_Get_Parallel(void);
    ALTDSS_CAPI_DLL void Reactors_Set_Parallel(uint16_t Value);
    ALTDSS_CAPI_DLL double Reactors_Get_LmH(void);
    ALTDSS_CAPI_DLL void Reactors_Set_LmH(double Value);
    ALTDSS_CAPI_DLL const char* Reactors_Get_Bus1(void);
    ALTDSS_CAPI_DLL const char* Reactors_Get_Bus2(void);
    ALTDSS_CAPI_DLL void Reactors_Set_Bus1(const char* Value);
    ALTDSS_CAPI_DLL void Reactors_Set_Bus2(const char* Value);
    ALTDSS_CAPI_DLL double Reactors_Get_R(void);
    ALTDSS_CAPI_DLL void Reactors_Set_R(double Value);
    ALTDSS_CAPI_DLL double Reactors_Get_X(void);
    ALTDSS_CAPI_DLL void Reactors_Set_X(double Value);
    ALTDSS_CAPI_DLL double Reactors_Get_Rp(void);
    ALTDSS_CAPI_DLL void Reactors_Set_Rp(double Value);
    ALTDSS_CAPI_DLL const char* Reactors_Get_RCurve(void);
    ALTDSS_CAPI_DLL void Reactors_Set_RCurve(const char* Value);
    ALTDSS_CAPI_DLL const char* Reactors_Get_LCurve(void);
    ALTDSS_CAPI_DLL void Reactors_Set_LCurve(const char* Value);
    ALTDSS_CAPI_DLL void Reactors_Get_Rmatrix(double** ResultPtr, int32_t* ResultDims);
    ALTDSS_CAPI_DLL void Reactors_Get_Rmatrix_GR(void);
    ALTDSS_CAPI_DLL void Reactors_Set_Rmatrix(const double* ValuePtr, int32_t ValueCount);
    ALTDSS_CAPI_DLL void Reactors_Get_Xmatrix(double** ResultPtr, int32_t* ResultDims);
    ALTDSS_CAPI_DLL void Reactors_Get_Xmatrix_GR(void);
    ALTDSS_CAPI_DLL void Reactors_Set_Xmatrix(const double* ValuePtr, int32_t ValueCount);
    ALTDSS_CAPI_DLL void Reactors_Get_Z(double** ResultPtr, int32_t* ResultDims);
    ALTDSS_CAPI_DLL void Reactors_Get_Z_GR(void);
    ALTDSS_CAPI_DLL void Reactors_Set_Z(const double* ValuePtr, int32_t ValueCount);
    ALTDSS_CAPI_DLL void Reactors_Get_Z1(double** ResultPtr, int32_t* ResultDims);
    ALTDSS_CAPI_DLL void Reactors_Get_Z1_GR(void);
    ALTDSS_CAPI_DLL void Reactors_Set_Z1(const double* ValuePtr, int32_t ValueCount);
    ALTDSS_CAPI_DLL void Reactors_Get_Z2(double** ResultPtr, int32_t* ResultDims);
    ALTDSS_CAPI_DLL void Reactors_Get_Z2_GR(void);
    ALTDSS_CAPI_DLL void Reactors_Set_Z2(const double* ValuePtr, int32_t ValueCount);
    ALTDSS_CAPI_DLL void Reactors_Get_Z0(double** ResultPtr, int32_t* ResultDims);
    ALTDSS_CAPI_DLL void Reactors_Get_Z0_GR(void);
    ALTDSS_CAPI_DLL void Reactors_Set_Z0(const double* ValuePtr, int32_t ValueCount);
    ALTDSS_CAPI_DLL int32_t Reactors_Get_SpecType(void);

    ALTDSS_CAPI_DLL int32_t TSData_Get_Count(void);
    ALTDSS_CAPI_DLL int32_t TSData_Get_First(void);
    ALTDSS_CAPI_DLL int32_t TSData_Get_Next(void);
    ALTDSS_CAPI_DLL const char* TSData_Get_Name(void);
    ALTDSS_CAPI_DLL void TSData_Set_Name(const char* Value);
    ALTDSS_CAPI_DLL void TSData_Get_AllNames(char*** ResultPtr, int32_t* ResultDims);
    ALTDSS_CAPI_DLL double TSData_Get_Rdc(void);
    ALTDSS_CAPI_DLL void TSData_Set_Rdc(double Value);
    ALTDSS_CAPI_DLL double TSData_Get_Rac(void);
    ALTDSS_CAPI_DLL void TSData_Set_Rac(double Value);
    ALTDSS_CAPI_DLL double TSData_Get_GMRac(void);
    ALTDSS_CAPI_DLL void TSData_Set_GMRac(double Value);
    ALTDSS_CAPI_DLL int32_t TSData_Get_GMRUnits(void);
    ALTDSS_CAPI_DLL void TSData_Set_GMRUnits(int32_t Value);
    ALTDSS_CAPI_DLL double TSData_Get_Radius(void);
    ALTDSS_CAPI_DLL void TSData_Set_Radius(double Value);
    ALTDSS_CAPI_DLL int32_t TSData_Get_RadiusUnits(void);
    ALTDSS_CAPI_DLL void TSData_Set_RadiusUnits(int32_t Value);
    ALTDSS_CAPI_DLL int32_t TSData_Get_ResistanceUnits(void);
    ALTDSS_CAPI_DLL void TSData_Set_ResistanceUnits(int32_t Value);
    ALTDSS_CAPI_DLL double TSData_Get_Diameter(void);
    ALTDSS_CAPI_DLL void TSData_Set_Diameter(double Value);
    ALTDSS_CAPI_DLL double TSData_Get_NormAmps(void);
    ALTDSS_CAPI_DLL void TSData_Set_NormAmps(double Value);
    ALTDSS_CAPI_DLL double TSData_Get_EmergAmps(void);
    ALTDSS_CAPI_DLL void TSData_Set_EmergAmps(double Value);
    ALTDSS_CAPI_DLL double TSData_Get_EpsR(void);
    ALTDSS_CAPI_DLL void TSData_Set_EpsR(double Value);
    ALTDSS_CAPI_DLL double TSData_Get_InsLayer(void);
    ALTDSS_CAPI_DLL void TSData_Set_InsLayer(double Value);
    ALTDSS_CAPI_DLL double TSData_Get_DiaIns(void);
    ALTDSS_CAPI_DLL void TSData_Set_DiaIns(double Value);
    ALTDSS_CAPI_DLL double TSData_Get_DiaCable(void);
    ALTDSS_CAPI_DLL void TSData_Set_DiaCable(double Value);
    ALTDSS_CAPI_DLL double TSData_Get_DiaShield(void);
    ALTDSS_CAPI_DLL void TSData_Set_DiaShield(double Value);
    ALTDSS_CAPI_DLL double TSData_Get_TapeLayer(void);
    ALTDSS_CAPI_DLL void TSData_Set_TapeLayer(double Value);
    ALTDSS_CAPI_DLL double TSData_Get_TapeLap(void);
    ALTDSS_CAPI_DLL void TSData_Set_TapeLap(double Value);

    ALTDSS_CAPI_DLL int32_t WireData_Get_Count(void);
    ALTDSS_CAPI_DLL int32_t WireData_Get_First(void);
    ALTDSS_CAPI_DLL int32_t WireData_Get_Next(void);
    ALTDSS_CAPI_DLL const char* WireData_Get_Name(void);
    ALTDSS_CAPI_DLL void WireData_Set_Name(const char* Value);
    ALTDSS_CAPI_DLL void WireData_Get_AllNames(char*** ResultPtr, int32_t* ResultDims);
    ALTDSS_CAPI_DLL double WireData_Get_Rdc(void);
    ALTDSS_CAPI_DLL void WireData_Set_Rdc(double Value);
    ALTDSS_CAPI_DLL double WireData_Get_Rac(void);
    ALTDSS_CAPI_DLL void WireData_Set_Rac(double Value);
    ALTDSS_CAPI_DLL double WireData_Get_GMRac(void);
    ALTDSS_CAPI_DLL void WireData_Set_GMRac(double Value);
    ALTDSS_CAPI_DLL int32_t WireData_Get_GMRUnits(void);
    ALTDSS_CAPI_DLL void WireData_Set_GMRUnits(int32_t Value);
    ALTDSS_CAPI_DLL double WireData_Get_Radius(void);
    ALTDSS_CAPI_DLL void WireData_Set_Radius(double Value);
    ALTDSS_CAPI_DLL int32_t WireData_Get_RadiusUnits(void);
    ALTDSS_CAPI_DLL void WireData_Set_RadiusUnits(int32_t Value);
    ALTDSS_CAPI_DLL int32_t WireData_Get_ResistanceUnits(void);
    ALTDSS_CAPI_DLL void WireData_Set_ResistanceUnits(int32_t Value);
    ALTDSS_CAPI_DLL double WireData_Get_Diameter(void);
    ALTDSS_CAPI_DLL void WireData_Set_Diameter(double Value);
    ALTDSS_CAPI_DLL double WireData_Get_NormAmps(void);
    ALTDSS_CAPI_DLL void WireData_Set_NormAmps(double Value);
    ALTDSS_CAPI_DLL double WireData_Get_EmergAmps(void);
    ALTDSS_CAPI_DLL void WireData_Set_EmergAmps(double Value);
    ALTDSS_CAPI_DLL double WireData_Get_CapRadius(void);
    ALTDSS_CAPI_DLL void WireData_Set_CapRadius(double Value);

    /*! 
    Gets the index for the current active bus. Returns -1 if no active bus.
    
    (API Extension)
    */
    ALTDSS_CAPI_DLL int32_t Bus_Get_idx(void);

    /*! 
    Set the next bus as active. Returns -1 if no more buses, 0 otherwise.
    
    (API Extension)
    */
    ALTDSS_CAPI_DLL int32_t Bus_Get_Next(void);

    /*! 
    Gets/sets the DSS script error-handling behavior. If a warning or error
    occurs and early abortion is enabled (default), the processing of the
    script is always halted. Otherwise, the processing of the script continues
    until a major error occurs or it finishes.
    
    (API Extension)
    */
    ALTDSS_CAPI_DLL uint16_t Error_Get_EarlyAbort(void);
    ALTDSS_CAPI_DLL void Error_Set_EarlyAbort(uint16_t Value);
    
    /*! 
    Gets/controls the extended error behavior.
    Extended errors are errors derived from checks across the API to ensure
    a valid state. Although many of these checks are already present in the 
    original/official COM interface, the checks do not produce any error 
    message. An error value can be returned by a function but this value
    can, for many of the functions, be a valid value. As such, the user
    has no means to detect an invalid API call. 
    
    Extended errors use the Error interface to provide a more clear message
    and should help users, especially new users, to find usage issues earlier.
    
    The current default state is ON. For compatibility, the user can turn it
    off to restore the previous behavior.
    
    (API Extension)
    */
    ALTDSS_CAPI_DLL uint16_t Error_Get_ExtendedErrors(void);
    ALTDSS_CAPI_DLL void Error_Set_ExtendedErrors(uint16_t Value);

    ALTDSS_CAPI_DLL int32_t CNData_Get_idx(void);
    ALTDSS_CAPI_DLL void CNData_Set_idx(int32_t Value);
    ALTDSS_CAPI_DLL int32_t CapControls_Get_idx(void);
    ALTDSS_CAPI_DLL void CapControls_Set_idx(int32_t Value);
    ALTDSS_CAPI_DLL int32_t Capacitors_Get_idx(void);
    ALTDSS_CAPI_DLL void Capacitors_Set_idx(int32_t Value);
    ALTDSS_CAPI_DLL int32_t GICSources_Get_idx(void);
    ALTDSS_CAPI_DLL void GICSources_Set_idx(int32_t Value);
    ALTDSS_CAPI_DLL int32_t ISources_Get_idx(void);
    ALTDSS_CAPI_DLL void ISources_Set_idx(int32_t Value);
    ALTDSS_CAPI_DLL int32_t LineCodes_Get_idx(void);
    ALTDSS_CAPI_DLL void LineCodes_Set_idx(int32_t Value);
    ALTDSS_CAPI_DLL int32_t LineGeometries_Get_idx(void);
    ALTDSS_CAPI_DLL void LineGeometries_Set_idx(int32_t Value);
    ALTDSS_CAPI_DLL int32_t LineSpacings_Get_idx(void);
    ALTDSS_CAPI_DLL void LineSpacings_Set_idx(int32_t Value);
    ALTDSS_CAPI_DLL int32_t Lines_Get_idx(void);
    ALTDSS_CAPI_DLL void Lines_Set_idx(int32_t Value);
    ALTDSS_CAPI_DLL int32_t LoadShapes_Get_idx(void);
    ALTDSS_CAPI_DLL void LoadShapes_Set_idx(int32_t Value);
    ALTDSS_CAPI_DLL int32_t Meters_Get_idx(void);
    ALTDSS_CAPI_DLL void Meters_Set_idx(int32_t Value);
    ALTDSS_CAPI_DLL int32_t Monitors_Get_idx(void);
    ALTDSS_CAPI_DLL void Monitors_Set_idx(int32_t Value);
    ALTDSS_CAPI_DLL int32_t Reactors_Get_idx(void);
    ALTDSS_CAPI_DLL void Reactors_Set_idx(int32_t Value);
    ALTDSS_CAPI_DLL int32_t RegControls_Get_idx(void);
    ALTDSS_CAPI_DLL void RegControls_Set_idx(int32_t Value);
    ALTDSS_CAPI_DLL int32_t Sensors_Get_idx(void);
    ALTDSS_CAPI_DLL void Sensors_Set_idx(int32_t Value);
    ALTDSS_CAPI_DLL int32_t SwtControls_Get_idx(void);
    ALTDSS_CAPI_DLL void SwtControls_Set_idx(int32_t Value);
    ALTDSS_CAPI_DLL int32_t TSData_Get_idx(void);
    ALTDSS_CAPI_DLL void TSData_Set_idx(int32_t Value);
    ALTDSS_CAPI_DLL int32_t Transformers_Get_idx(void);
    ALTDSS_CAPI_DLL void Transformers_Set_idx(int32_t Value);
    ALTDSS_CAPI_DLL int32_t Vsources_Get_idx(void);
    ALTDSS_CAPI_DLL void Vsources_Set_idx(int32_t Value);
    ALTDSS_CAPI_DLL int32_t WireData_Get_idx(void);
    ALTDSS_CAPI_DLL void WireData_Set_idx(int32_t Value);
    ALTDSS_CAPI_DLL int32_t XYCurves_Get_idx(void);
    ALTDSS_CAPI_DLL void XYCurves_Set_idx(int32_t Value);


    /*! 
    Array of total losses (complex) in a selection of elements.
    Use the element indices (starting at 1) as parameter.

    (API Extension)
    */
    ALTDSS_CAPI_DLL void Circuit_Get_ElementLosses(double** ResultPtr, int32_t* ResultDims, int32_t *ElementsPtr, int32_t ElementsCount);
    /*! 
    Same as Circuit_Get_ElementLosses but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void Circuit_Get_ElementLosses_GR(int32_t *ElementsPtr, int32_t ElementsCount);


    /*! 
    Sets all numeric arrays for the active LoadShape.

    If ExternalMemory is 0/False, the data is copied, allocating memory.
    If ExternalMemory is 1/True, the data is NOT copied. The caller is required to keep the
    pointers alive while the LoadShape is used, as well as deallocating them later.

    If IsFloat32 is 0/False, the pointers are interpreted as pointers to float64/double precision numbers.
    Otherwise, the pointers are interpreted as pointers to float32/single precision numbers.
    
    Stride: number of elements to skip to reach the next indexed element.
    Stride is only used when ExternalMemory is 1/True (ignored otherwise).
    For non-contiguous series and transposed data. Depending on how much that is used, a row-major matrix is
    preferred over a column-major one.
    
    Remember to set MaxP and MaxQ accordingly.
    
    (API Extension)
    */
    ALTDSS_CAPI_DLL void LoadShapes_Set_Points(int32_t Npts, void *HoursPtr, void *PMultPtr, void *QMultPtr, uint16_t ExternalMemory, uint16_t IsFloat32, int32_t Stride);

    /*! 
    Converts the current LoadShape data to float32/single precision.
    If there is no data or the data is already represented using float32, nothing is done.
    
    (API Extension)
    */
    ALTDSS_CAPI_DLL void LoadShapes_UseFloat32(void);

    /*! 
    Converts the current LoadShape data to float64/double precision.
    If there is no data or the data is already represented using float64, nothing is done.
    
    (API Extension)
    */
    ALTDSS_CAPI_DLL void LoadShapes_UseFloat64(void);

    /*! 
    kW value at the time of max power. This is set automatically set upon reading in a loadshape, but for external-memory loadshapes, the user must provide the value.
    This property can also be used to override the value automatically computed or to retrieve the value computed.
    
    (API Extension)
    */
    ALTDSS_CAPI_DLL void LoadShapes_Set_MaxP(double Value);
  
    /*! 
    kW value at the time of max power. This is set automatically set upon reading in a loadshape, but for external-memory loadshapes, the user must provide the value.
    This property can also be used to override the value automatically computed or to retrieve the value computed.
    
    (API Extension)
    */
    ALTDSS_CAPI_DLL double LoadShapes_Get_MaxP(void);

    /*! 
    kW value at the time of max power. This is set automatically set upon reading in a loadshape, but for external-memory loadshapes, the user must provide the value.
    This property can also be used to override the value automatically computed or to retrieve the value computed.
    
    (API Extension)
    */
    ALTDSS_CAPI_DLL void LoadShapes_Set_MaxQ(double Value);

    /*! 
    kvar value at the time of max kW power.
    This is set automatically set upon reading in a loadshape, but for external-memory loadshapes, the user must provide the value.
    This property can also be used to override the value automatically computed or to retrieve the value computed.
    
    (API Extension)
    */
    ALTDSS_CAPI_DLL double LoadShapes_Get_MaxQ(void);

    /*! 
    Array of names of all WindGen objects.
    */
    ALTDSS_CAPI_DLL void WindGens_Get_AllNames(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Sets first WindGen to be active.  Returns 0 if none.
    */
    ALTDSS_CAPI_DLL int32_t WindGens_Get_First(void);

    /*! 
    Sets a WindGen active by name.
    */
    ALTDSS_CAPI_DLL const char* WindGens_Get_Name(void);

    /*! 
    Sets next WindGen to be active.  Returns 0 if no more.
    */
    ALTDSS_CAPI_DLL int32_t WindGens_Get_Next(void);

    /*! 
    Array of Names of all WindGen energy meter registers
    
    See also the enum `GeneratorRegisters`.
    */
    ALTDSS_CAPI_DLL void WindGens_Get_RegisterNames(char*** ResultPtr, int32_t* ResultDims);

    /*! 
    Array of values in WindGen energy meter registers.
    */
    ALTDSS_CAPI_DLL void WindGens_Get_RegisterValues(double** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as WindGens_Get_RegisterValues but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void WindGens_Get_RegisterValues_GR(void);

    /*! 
    Sets a WindGen active by name.
    */
    ALTDSS_CAPI_DLL void WindGens_Set_Name(const char* Value);

    /*! 
    Nominal rated (1.0 per unit) voltage for the active WindGen, in kV.
    */
    ALTDSS_CAPI_DLL double WindGens_Get_kV(void);

    /*! 
    Base kvar for the active WindGen.
    */
    ALTDSS_CAPI_DLL double WindGens_Get_kvar(void);

    /*! 
    Total base kW for the active WindGen.
    */
    ALTDSS_CAPI_DLL double WindGens_Get_kW(void);

    /*! 
    WindGen power factor. Power factor (pos. = producing vars).
    */
    ALTDSS_CAPI_DLL double WindGens_Get_PF(void);

    /*! 
    Nominal rated (1.0 per unit) voltage for the active WindGen, in kV.
    */
    ALTDSS_CAPI_DLL void WindGens_Set_kV(double Value);

    /*! 
    Base kvar for the active WindGen.
    */
    ALTDSS_CAPI_DLL void WindGens_Set_kvar(double Value);

    /*! 
    Total base kW for the active WindGen.
    */
    ALTDSS_CAPI_DLL void WindGens_Set_kW(double Value);

    /*! 
    WindGen power factor. Power factor (pos. = producing vars).
    */
    ALTDSS_CAPI_DLL void WindGens_Set_PF(double Value);

    /*! 
    Number of WindGen Objects in Active Circuit
    */
    ALTDSS_CAPI_DLL int32_t WindGens_Get_Count(void);

    /*! 
    Get/Set active WindGen by index into the list of wind generators.  1..Count
    */
    ALTDSS_CAPI_DLL int32_t WindGens_Get_idx(void);

    /*! 
    Get/Set active WindGen by index into the list of wind generators. 1..Count
    */
    ALTDSS_CAPI_DLL void WindGens_Set_idx(int32_t Value);

    /*! 
    KVA rating of the electrical machine in the WindGen.
    */
    ALTDSS_CAPI_DLL double WindGens_Get_kVA(void);

    /*! 
    KVA rating of the electrical machine in the WindGen.
    */
    ALTDSS_CAPI_DLL void WindGens_Set_kVA(double Value);

    /*! 
    Gearbox ratio
    */
    ALTDSS_CAPI_DLL double WindGens_Get_Ag(void);

    /*! 
    Gearbox ratio
    */
    ALTDSS_CAPI_DLL void WindGens_Set_Ag(double Value);

    /*! 
    Turbine performance coefficient.
    */
    ALTDSS_CAPI_DLL double WindGens_Get_Cp(void);

    /*! 
    Turbine performance coefficient.
    */
    ALTDSS_CAPI_DLL void WindGens_Set_Cp(double Value);

    /*! 
    Tip speed ratio
    */
    ALTDSS_CAPI_DLL double WindGens_Get_Lamda(void);

    /*! 
    Tip speed ratio
    */
    ALTDSS_CAPI_DLL void WindGens_Set_Lamda(double Value);

    /*! 
    Number of WTG in aggregation
    */
    ALTDSS_CAPI_DLL int32_t WindGens_Get_N_WTG(void);

    /*! 
    Number of WTG in aggregation
    */
    ALTDSS_CAPI_DLL void WindGens_Set_N_WTG(int32_t Value);

    /*! 
    Number of pole pairs of the induction generator
    */
    ALTDSS_CAPI_DLL int32_t WindGens_Get_NPoles(void);

    /*! 
    Number of pole pairs of the induction generator
    */
    ALTDSS_CAPI_DLL void WindGens_Set_NPoles(int32_t Value);

    /*! 
    Air density in kg/m3
    */
    ALTDSS_CAPI_DLL double WindGens_Get_pd(void);

    /*! 
    Air density in kg/m3
    */
    ALTDSS_CAPI_DLL void WindGens_Set_pd(double Value);

    /*! 
    Steady state output real power.
    */
    ALTDSS_CAPI_DLL double WindGens_Get_PSS(void);

    /*! 
    Steady state output real power.
    */
    ALTDSS_CAPI_DLL void WindGens_Set_PSS(double Value);

    /*! 
    Non-zero values enable reactive power and voltage control in the dynamic model.
    */
    ALTDSS_CAPI_DLL int32_t WindGens_Get_QFlag(void);

    /*! 
    Non-zero values enable reactive power and voltage control in the dynamic model.
    */
    ALTDSS_CAPI_DLL void WindGens_Set_QFlag(int32_t Value);

    /*! 
    Q control mode (0:Q, 1:PF, 2:VV).
    */
    ALTDSS_CAPI_DLL int32_t WindGens_Get_QMode(void);

    /*! 
    Q control mode (0:Q, 1:PF, 2:VV).
    */
    ALTDSS_CAPI_DLL void WindGens_Set_QMode(int32_t Value);

    /*! 
    Steady state output reactive power.
    */
    ALTDSS_CAPI_DLL double WindGens_Get_QSS(void);

    /*! 
    Steady state output reactive power.
    */
    ALTDSS_CAPI_DLL void WindGens_Set_QSS(double Value);

    /*! 
    Rotor radius in meters
    */
    ALTDSS_CAPI_DLL double WindGens_Get_Rad(void);

    /*! 
    Rotor radius in meters
    */
    ALTDSS_CAPI_DLL void WindGens_Set_Rad(double Value);

    /*! 
    Per unit Thevenin equivalent resistance (R).
    */
    ALTDSS_CAPI_DLL double WindGens_Get_RThev(void);

    /*! 
    Per unit Thevenin equivalent resistance (R).
    */
    ALTDSS_CAPI_DLL void WindGens_Set_RThev(double Value);

    /*! 
    Cut-in speed for the wind generator
    */
    ALTDSS_CAPI_DLL double WindGens_Get_VCutIn(void);

    /*! 
    Cut-in speed for the wind generator
    */
    ALTDSS_CAPI_DLL void WindGens_Set_VCutIn(double Value);

    /*! 
    Cut-out speed for the wind generator
    */
    ALTDSS_CAPI_DLL double WindGens_Get_VCutOut(void);

    /*! 
    Cut-out speed for the wind generator
    */
    ALTDSS_CAPI_DLL void WindGens_Set_VCutOut(double Value);

    /*! 
    Steady state voltage magnitude.
    */
    ALTDSS_CAPI_DLL double WindGens_Get_Vss(void);

    /*! 
    Steady state voltage magnitude.
    */
    ALTDSS_CAPI_DLL void WindGens_Set_Vss(double Value);

    /*! 
    Wind speed in m/s
    */
    ALTDSS_CAPI_DLL double WindGens_Get_WindSpeed(void);

    /*! 
    Wind speed in m/s
    */
    ALTDSS_CAPI_DLL void WindGens_Set_WindSpeed(double Value);

    /*! 
    Per unit Thevenin equivalent reactance (X).
    */
    ALTDSS_CAPI_DLL double WindGens_Get_XThev(void);
    
    /*! 
    Per unit Thevenin equivalent reactance (X).
    */
    ALTDSS_CAPI_DLL void WindGens_Set_XThev(double Value);

    /*! 
    Number of phases

    (API Extension)
    */
    ALTDSS_CAPI_DLL int32_t WindGens_Get_Phases(void);

    /*! 
    Number of phases

    (API Extension)
    */
    ALTDSS_CAPI_DLL void WindGens_Set_Phases(int32_t Value);

    /*! 
    Name of the loadshape for daily wind speed

    (API Extension)
    */
    ALTDSS_CAPI_DLL const char* WindGens_Get_daily(void);
    ALTDSS_CAPI_DLL void WindGens_Set_daily(const char* Value);

    /*! 
    Name of the loadshape for a duty cycle simulation.

    (API Extension)
    */
    ALTDSS_CAPI_DLL const char* WindGens_Get_duty(void);
    ALTDSS_CAPI_DLL void WindGens_Set_duty(const char* Value);

    /*! 
    Name of yearly loadshape

    (API Extension)
    */
    ALTDSS_CAPI_DLL const char* WindGens_Get_Yearly(void);
    ALTDSS_CAPI_DLL void WindGens_Set_Yearly(const char* Value);

    /*! 
    WindGen connection. True/1 if delta connection, False/0 if wye.

    (API Extension)
    */
    ALTDSS_CAPI_DLL uint16_t WindGens_Get_IsDelta(void);
    ALTDSS_CAPI_DLL void WindGens_Set_IsDelta(uint16_t Value);

    /*! 
    An arbitrary integer number representing the class of WindGen so that WindGen values may be segregated by class.

    (API Extension)
    */
    ALTDSS_CAPI_DLL int32_t WindGens_Get_Class_(void);
    ALTDSS_CAPI_DLL void WindGens_Set_Class_(int32_t Value);

    /*! 
    Bus to which the WindGen is connected. May include specific node specification.
    
    (API Extension)
    */
    ALTDSS_CAPI_DLL const char* WindGens_Get_Bus1(void);
    ALTDSS_CAPI_DLL void WindGens_Set_Bus1(const char* Value);

    /*! 
    Array of integers, a copy of the internal NodeRef of the CktElement.
    
    (API Extension)
    */
    ALTDSS_CAPI_DLL void CktElement_Get_NodeRef(int32_t** ResultPtr, int32_t* ResultDims);
    /*! 
    Same as CktElement_Get_NodeRef but using the global buffer interface for results
    */
    ALTDSS_CAPI_DLL void CktElement_Get_NodeRef_GR(void);

    /*! 
    Update and return the convergence flag. Used for external solver loops.
    */
    ALTDSS_CAPI_DLL uint16_t YMatrix_CheckConvergence(void);
    ALTDSS_CAPI_DLL void YMatrix_SetGeneratordQdV(void);

    ALTDSS_CAPI_DLL uint16_t YMatrix_Get_LoadsNeedUpdating(void);
    ALTDSS_CAPI_DLL void YMatrix_Set_LoadsNeedUpdating(uint16_t Value);
    ALTDSS_CAPI_DLL uint16_t YMatrix_Get_SolutionInitialized(void);
    ALTDSS_CAPI_DLL void YMatrix_Set_SolutionInitialized(uint16_t Value);

    ALTDSS_CAPI_DLL int32_t YMatrix_Get_Iteration(void);
    ALTDSS_CAPI_DLL void YMatrix_Set_Iteration(int32_t Value);
    ALTDSS_CAPI_DLL void *YMatrix_Get_Handle(void);

    ALTDSS_CAPI_DLL void YMatrix_Set_SolverOptions(uint64_t opts);
    ALTDSS_CAPI_DLL uint64_t YMatrix_Get_SolverOptions(void);
    ALTDSS_CAPI_DLL void YMatrix_SaveAsMarketFiles(const char* baseFileName);

    ALTDSS_CAPI_DLL void Text_CommandBlock(const char* Value);
    ALTDSS_CAPI_DLL void Text_CommandArray(const char** ValuePtr, int32_t ValueCount);

    /*! 
    Opens and prepares a ZIP file to be used by the DSS text parser.
    Currently, the ZIP format support is limited by what is provided in the Free Pascal distribution.
    Besides that, the full filenames inside the ZIP must be shorter than 256 characters.
    The limitations should be removed in a future revision.
    
    (API Extension)
    */
    ALTDSS_CAPI_DLL void ZIP_Open(const char* FileName);

    /*! 
    Runs a "Redirect" command inside the current (open) ZIP file.
    In the current implementation, all files required by the script must
    be present inside the ZIP, using relative paths. The only exceptions are
    memory-mapped files.

    (API Extension)
    */
    ALTDSS_CAPI_DLL void ZIP_Redirect(const char* FileInZip);

    /*! 
    Check if the given path name is present in the current ZIP file.
    
    (API Extension)
    */
    ALTDSS_CAPI_DLL uint16_t ZIP_Contains(const char* Name);
    
    /*! 
    List of strings consisting of all names match the regular expression provided in regexp.
    If no expression is provided, all names in the current open ZIP are returned.
    
    See https://regex.sorokin.engineer/en/latest/regular_expressions.html for information on 
    the expression syntax and options.

    (API Extension)
    */
    ALTDSS_CAPI_DLL void ZIP_List(char*** ResultPtr, int32_t *ResultCount, const char* RegExp);

    /*! 
    Extracts the contents of the file "FileName" from the current (open) ZIP file.
    Returns a byte-string.

    (API Extension)
    */
    ALTDSS_CAPI_DLL void ZIP_Extract(int8_t** ResultPtr, int32_t* ResultDims, const char* FileName);

    ALTDSS_CAPI_DLL void ZIP_Extract_GR(const char* FileName);

    /*! 
    Closes the current open ZIP file.
    
    (API Extension)
    */    
    ALTDSS_CAPI_DLL void ZIP_Close(void);

    /*! 
    Functions for the new API
    */

    /*!
    Return the pointer to the active element for each of the classic API classes

    (API Extension)
    */
    ALTDSS_CAPI_DLL void* ActiveClass_Get_Pointer(void);
    ALTDSS_CAPI_DLL void* Capacitors_Get_Pointer(void);
    ALTDSS_CAPI_DLL void* CapControls_Get_Pointer(void);
    ALTDSS_CAPI_DLL void* CktElement_Get_Pointer(void);
    ALTDSS_CAPI_DLL void* CNData_Get_Pointer(void);
    ALTDSS_CAPI_DLL void* DSSElement_Get_Pointer(void);
    ALTDSS_CAPI_DLL void* Fuses_Get_Pointer(void);
    ALTDSS_CAPI_DLL void* Generators_Get_Pointer(void);
    ALTDSS_CAPI_DLL void* GICSources_Get_Pointer(void);
    ALTDSS_CAPI_DLL void* ISources_Get_Pointer(void);
    ALTDSS_CAPI_DLL void* LineCodes_Get_Pointer(void);
    ALTDSS_CAPI_DLL void* LineGeometries_Get_Pointer(void);
    ALTDSS_CAPI_DLL void* LineSpacings_Get_Pointer(void);
    ALTDSS_CAPI_DLL void* Lines_Get_Pointer(void);
    ALTDSS_CAPI_DLL void* LoadShapes_Get_Pointer(void);
    ALTDSS_CAPI_DLL void* Loads_Get_Pointer(void);
    ALTDSS_CAPI_DLL void* Meters_Get_Pointer(void);
    ALTDSS_CAPI_DLL void* Monitors_Get_Pointer(void);
    ALTDSS_CAPI_DLL void* PVSystems_Get_Pointer(void);
    ALTDSS_CAPI_DLL void* Reactors_Get_Pointer(void);
    ALTDSS_CAPI_DLL void* Reclosers_Get_Pointer(void);
    ALTDSS_CAPI_DLL void* RegControls_Get_Pointer(void);
    ALTDSS_CAPI_DLL void* Relays_Get_Pointer(void);
    ALTDSS_CAPI_DLL void* Sensors_Get_Pointer(void);
    ALTDSS_CAPI_DLL void* Storages_Get_Pointer(void);
    ALTDSS_CAPI_DLL void* SwtControls_Get_Pointer(void);
    ALTDSS_CAPI_DLL void* Transformers_Get_Pointer(void);
    ALTDSS_CAPI_DLL void* TSData_Get_Pointer(void);
    ALTDSS_CAPI_DLL void* Vsources_Get_Pointer(void);
    ALTDSS_CAPI_DLL void* WireData_Get_Pointer(void);
    ALTDSS_CAPI_DLL void* XYCurves_Get_Pointer(void);
    ALTDSS_CAPI_DLL void* WindGens_Get_Pointer(void);

#ifdef __cplusplus
} // extern "C"
#ifdef ALTDSS_CAPI_NAMESPACE
} } // namespace altdss::capi
#endif
#endif
#endif
