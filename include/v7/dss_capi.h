#ifndef DSS_CAPI_V7_DLL_H
#define DSS_CAPI_V7_DLL_H
#define DSS_CAPI_V7_VERSION "0.10.0"
#ifndef DSS_CAPI_V7_DLL
//#define DSS_CAPI_V7_DLL __declspec(dllimport)
#define DSS_CAPI_V7_DLL
#endif

#ifdef __cplusplus
#    ifdef _MSC_VER
#       if _MSC_VER <= 1500
#           include "../stdint_compat.h"
#       else
#           include <cstdint>
#       endif
#    else
#        include <cstdint>
#    endif
#else
#    ifdef _MSC_VER
#       if _MSC_VER <= 1500
#           include "../stdint_compat.h"
#       else
#           include <stdint.h>
#       endif
#    else
#        include <stdint.h>
#    endif
#endif

#ifdef __cplusplus
extern "C" {
#else
#endif
    DSS_CAPI_V7_DLL void DSS_ResetStringBuffer(void);
    DSS_CAPI_V7_DLL void DSS_Dispose_PByte(int8_t** p);
    DSS_CAPI_V7_DLL void DSS_Dispose_PDouble(double** p);
    DSS_CAPI_V7_DLL void DSS_Dispose_PInteger(int32_t** p);
    DSS_CAPI_V7_DLL void DSS_Dispose_PPAnsiChar(char ***p, int32_t cnt);
    DSS_CAPI_V7_DLL char* DSS_Get_PAnsiChar(void *p, int32_t index);

    /*
    Dispose temporary buffer data in the global result (GR) pointers
    */
    DSS_CAPI_V7_DLL void DSS_DisposeGRData(void);

    /*
    Get references to the global result (GR) pointers, used in
    the *_GR variations of most getter functions

    The returned values in the DataPtrs will contain pointers to the global variables that contains the actual pointers.
    The CountPtrs are not reallocated during the execution, so the returned values contain the actual pointer values.
    */
    DSS_CAPI_V7_DLL void DSS_GetGRPointers(
        char**** DataPtr_PPAnsiChar,
        double*** DataPtr_PDouble,
        int32_t*** DataPtr_PInteger,
        int8_t*** DataPtr_PByte,
        int32_t** CountPtr_PPAnsiChar,
        int32_t** CountPtr_PDouble,
        int32_t** CountPtr_PInteger,
        int32_t** CountPtr_PByte
    );


    DSS_CAPI_V7_DLL void DSS_NewCircuit(char* Value);

    /*
    Array of strings consisting of all element names in the active class.
    */
    DSS_CAPI_V7_DLL void ActiveClass_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as ActiveClass_Get_AllNames but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void ActiveClass_Get_AllNames_GR(void);

    /*
    Sets first element in the active class to be the active DSS object. If object is a CktElement, ActiveCktELment also points to this element. Returns 0 if none.
    */
    DSS_CAPI_V7_DLL int32_t ActiveClass_Get_First(void);

    /*
    Sets next element in active class to be the active DSS object. If object is a CktElement, ActiveCktElement also points to this element.  Returns 0 if no more.
    */
    DSS_CAPI_V7_DLL int32_t ActiveClass_Get_Next(void);

    /*
    Name of the Active Element of the Active Class
    */
    DSS_CAPI_V7_DLL char* ActiveClass_Get_Name(void);

    DSS_CAPI_V7_DLL void ActiveClass_Set_Name(char* Value);

    /*
    Number of elements in this class. Same as Count property.
    */
    DSS_CAPI_V7_DLL int32_t ActiveClass_Get_NumElements(void);

    /*
    Returns name of active class.
    */
    DSS_CAPI_V7_DLL char* ActiveClass_Get_ActiveClassName(void);

    /*
    Number of elements in Active Class. Same as NumElements Property.
    */
    DSS_CAPI_V7_DLL int32_t ActiveClass_Get_Count(void);

    /*
    Name of Bus
    */
    DSS_CAPI_V7_DLL char* Bus_Get_Name(void);

    /*
    Number of Nodes this bus.
    */
    DSS_CAPI_V7_DLL int32_t Bus_Get_NumNodes(void);

    /*
    Double Array of sequence voltages at this bus.
    */
    DSS_CAPI_V7_DLL void Bus_Get_SeqVoltages(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Bus_Get_SeqVoltages but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Bus_Get_SeqVoltages_GR(void);

    /*
    Complex array of voltages at this bus.
    */
    DSS_CAPI_V7_DLL void Bus_Get_Voltages(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Bus_Get_Voltages but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Bus_Get_Voltages_GR(void);

    /*
    Integer Array of Node Numbers defined at the bus in same order as the voltages.
    */
    DSS_CAPI_V7_DLL void Bus_Get_Nodes(int32_t** ResultPtr, int32_t* ResultCount);
    /*
    Same as Bus_Get_Nodes but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Bus_Get_Nodes_GR(void);

    /*
    Short circuit currents at bus; Complex Array.
    */
    DSS_CAPI_V7_DLL void Bus_Get_Isc(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Bus_Get_Isc but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Bus_Get_Isc_GR(void);

    /*
    Open circuit voltage; Complex array.
    */
    DSS_CAPI_V7_DLL void Bus_Get_Voc(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Bus_Get_Voc but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Bus_Get_Voc_GR(void);

    /*
    Base voltage at bus in kV
    */
    DSS_CAPI_V7_DLL double Bus_Get_kVBase(void);

    /*
    Complex Array of pu voltages at the bus.
    */
    DSS_CAPI_V7_DLL void Bus_Get_puVoltages(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Bus_Get_puVoltages but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Bus_Get_puVoltages_GR(void);

    /*
    Complex Zero-Sequence short circuit impedance at bus.
    */
    DSS_CAPI_V7_DLL void Bus_Get_Zsc0(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Bus_Get_Zsc0 but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Bus_Get_Zsc0_GR(void);

    /*
    Complex Positive-Sequence short circuit impedance at bus..
    */
    DSS_CAPI_V7_DLL void Bus_Get_Zsc1(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Bus_Get_Zsc1 but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Bus_Get_Zsc1_GR(void);

    /*
    Complex array of Zsc matrix at bus. Column by column.
    */
    DSS_CAPI_V7_DLL void Bus_Get_ZscMatrix(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Bus_Get_ZscMatrix but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Bus_Get_ZscMatrix_GR(void);

    DSS_CAPI_V7_DLL uint16_t Bus_ZscRefresh(void);

    /*
    Complex array of Ysc matrix at bus. Column by column.
    */
    DSS_CAPI_V7_DLL void Bus_Get_YscMatrix(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Bus_Get_YscMatrix but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Bus_Get_YscMatrix_GR(void);

    /*
    False=0 else True. Indicates whether a coordinate has been defined for this bus
    */
    DSS_CAPI_V7_DLL uint16_t Bus_Get_Coorddefined(void);

    /*
    X Coordinate for bus (double)
    */
    DSS_CAPI_V7_DLL double Bus_Get_x(void);

    /*
    X Coordinate for bus (double)
    */
    DSS_CAPI_V7_DLL void Bus_Set_x(double Value);

    /*
    Y coordinate for bus(double)
    */
    DSS_CAPI_V7_DLL double Bus_Get_y(void);

    /*
    Y coordinate for bus(double)
    */
    DSS_CAPI_V7_DLL void Bus_Set_y(double Value);

    /*
    Distance from energymeter (if non-zero)
    */
    DSS_CAPI_V7_DLL double Bus_Get_Distance(void);

    DSS_CAPI_V7_DLL int32_t Bus_GetUniqueNodeNumber(int32_t StartNumber);

    /*
    Complex Double array of Sequence Voltages (0, 1, 2) at this Bus.
    */
    DSS_CAPI_V7_DLL void Bus_Get_CplxSeqVoltages(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Bus_Get_CplxSeqVoltages but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Bus_Get_CplxSeqVoltages_GR(void);

    /*
    Average interruption duration, hr.
    */
    DSS_CAPI_V7_DLL double Bus_Get_Int_Duration(void);

    /*
    Accumulated failure rate downstream from this bus; faults per year
    */
    DSS_CAPI_V7_DLL double Bus_Get_Lambda(void);

    /*
    Accumulated customer outage durations
    */
    DSS_CAPI_V7_DLL double Bus_Get_Cust_Duration(void);

    /*
    Annual number of customer-interruptions from this bus
    */
    DSS_CAPI_V7_DLL double Bus_Get_Cust_Interrupts(void);

    /*
    Total numbers of customers served downline from this bus
    */
    DSS_CAPI_V7_DLL int32_t Bus_Get_N_Customers(void);

    /*
    Number of interruptions this bus per year
    */
    DSS_CAPI_V7_DLL double Bus_Get_N_interrupts(void);

    /*
    Returns Complex array of pu L-L voltages for 2- and 3-phase buses. Returns -1.0 for 1-phase bus. If more than 3 phases, returns only 3 phases.
    */
    DSS_CAPI_V7_DLL void Bus_Get_puVLL(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Bus_Get_puVLL but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Bus_Get_puVLL_GR(void);

    /*
    For 2- and 3-phase buses, returns array of complex numbers represetin L-L voltages in volts. Returns -1.0 for 1-phase bus. If more than 3 phases, returns only first 3.
    */
    DSS_CAPI_V7_DLL void Bus_Get_VLL(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Bus_Get_VLL but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Bus_Get_VLL_GR(void);

    /*
    Array of doubles containig voltage magnitude, angle pairs in per unit
    */
    DSS_CAPI_V7_DLL void Bus_Get_puVmagAngle(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Bus_Get_puVmagAngle but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Bus_Get_puVmagAngle_GR(void);

    /*
    Variant Array of doubles containing voltages in Magnitude (VLN), angle (deg)
    */
    DSS_CAPI_V7_DLL void Bus_Get_VMagAngle(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Bus_Get_VMagAngle but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Bus_Get_VMagAngle_GR(void);

    /*
    Total length of line downline from this bus, in miles. For recloser siting algorithm.
    */
    DSS_CAPI_V7_DLL double Bus_Get_TotalMiles(void);

    /*
    Integer ID of the feeder section in which this bus is located.
    */
    DSS_CAPI_V7_DLL int32_t Bus_Get_SectionID(void);

    /*
    Array of strings with all Capacitor names in the circuit.
    */
    DSS_CAPI_V7_DLL void Capacitors_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as Capacitors_Get_AllNames but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Capacitors_Get_AllNames_GR(void);

    /*
    Sets the first Capacitor active. Returns 0 if no more.
    */
    DSS_CAPI_V7_DLL int32_t Capacitors_Get_First(void);

    /*
    Delta connection or wye?
    */
    DSS_CAPI_V7_DLL uint16_t Capacitors_Get_IsDelta(void);

    /*
    Bank kV rating. Use LL for 2 or 3 phases, or actual can rating for 1 phase.
    */
    DSS_CAPI_V7_DLL double Capacitors_Get_kV(void);

    /*
    Total bank KVAR, distributed equally among phases and steps.
    */
    DSS_CAPI_V7_DLL double Capacitors_Get_kvar(void);

    /*
    Sets the active Capacitor by Name.
    */
    DSS_CAPI_V7_DLL char* Capacitors_Get_Name(void);

    /*
    Sets the next Capacitor active. Returns 0 if no more.
    */
    DSS_CAPI_V7_DLL int32_t Capacitors_Get_Next(void);

    /*
    Number of steps (default 1) for distributing and switching the total bank kVAR.
    */
    DSS_CAPI_V7_DLL int32_t Capacitors_Get_NumSteps(void);

    /*
    Delta connection or wye?
    */
    DSS_CAPI_V7_DLL void Capacitors_Set_IsDelta(uint16_t Value);

    /*
    Bank kV rating. Use LL for 2 or 3 phases, or actual can rating for 1 phase.
    */
    DSS_CAPI_V7_DLL void Capacitors_Set_kV(double Value);

    /*
    Total bank KVAR, distributed equally among phases and steps.
    */
    DSS_CAPI_V7_DLL void Capacitors_Set_kvar(double Value);

    /*
    Sets the active Capacitor by Name.
    */
    DSS_CAPI_V7_DLL void Capacitors_Set_Name(char* Value);

    /*
    Number of steps (default 1) for distributing and switching the total bank kVAR.
    */
    DSS_CAPI_V7_DLL void Capacitors_Set_NumSteps(int32_t Value);

    /*
    Number of Capacitor objects in active circuit.
    */
    DSS_CAPI_V7_DLL int32_t Capacitors_Get_Count(void);

    DSS_CAPI_V7_DLL uint16_t Capacitors_AddStep(void);

    DSS_CAPI_V7_DLL uint16_t Capacitors_SubtractStep(void);

    /*
    Number of Steps available in cap bank to be switched ON.
    */
    DSS_CAPI_V7_DLL int32_t Capacitors_Get_AvailableSteps(void);

    /*
    A array of  integer [0..numsteps-1] indicating state of each step. If value is -1 an error has occurred.
    */
    DSS_CAPI_V7_DLL void Capacitors_Get_States(int32_t** ResultPtr, int32_t* ResultCount);
    /*
    Same as Capacitors_Get_States but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Capacitors_Get_States_GR(void);

    /*
    Array of integer [0 ..numSteps-1] indicating the state of each step
    */
    DSS_CAPI_V7_DLL void Capacitors_Set_States(int32_t* ValuePtr, int32_t ValueCount);

    DSS_CAPI_V7_DLL void Capacitors_Open(void);

    DSS_CAPI_V7_DLL void Capacitors_Close(void);

    /*
    Array of strings with all CapControl names.
    */
    DSS_CAPI_V7_DLL void CapControls_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as CapControls_Get_AllNames but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void CapControls_Get_AllNames_GR(void);

    /*
    Name of the Capacitor that is controlled.
    */
    DSS_CAPI_V7_DLL char* CapControls_Get_Capacitor(void);

    /*
    Transducer ratio from pirmary current to control current.
    */
    DSS_CAPI_V7_DLL double CapControls_Get_CTratio(void);

    DSS_CAPI_V7_DLL double CapControls_Get_DeadTime(void);

    /*
    Time delay [s] to switch on after arming.  Control may reset before actually switching.
    */
    DSS_CAPI_V7_DLL double CapControls_Get_Delay(void);

    /*
    Time delay [s] before swithcing off a step. Control may reset before actually switching.
    */
    DSS_CAPI_V7_DLL double CapControls_Get_DelayOff(void);

    /*
    Sets the first CapControl as active. Return 0 if none.
    */
    DSS_CAPI_V7_DLL int32_t CapControls_Get_First(void);

    /*
    Type of automatic controller.
    */
    DSS_CAPI_V7_DLL int32_t CapControls_Get_Mode(void);

    /*
    Full name of the element that PT and CT are connected to.
    */
    DSS_CAPI_V7_DLL char* CapControls_Get_MonitoredObj(void);

    /*
    Terminal number on the element that PT and CT are connected to.
    */
    DSS_CAPI_V7_DLL int32_t CapControls_Get_MonitoredTerm(void);

    /*
    Sets a CapControl active by name.
    */
    DSS_CAPI_V7_DLL char* CapControls_Get_Name(void);

    /*
    Gets the next CapControl in the circut. Returns 0 if none.
    */
    DSS_CAPI_V7_DLL int32_t CapControls_Get_Next(void);

    /*
    Threshold to switch off a step. See Mode for units.
    */
    DSS_CAPI_V7_DLL double CapControls_Get_OFFSetting(void);

    /*
    Threshold to arm or switch on a step.  See Mode for units.
    */
    DSS_CAPI_V7_DLL double CapControls_Get_ONSetting(void);

    /*
    Transducer ratio from primary feeder to control voltage.
    */
    DSS_CAPI_V7_DLL double CapControls_Get_PTratio(void);

    /*
    Enables Vmin and Vmax to override the control Mode
    */
    DSS_CAPI_V7_DLL uint16_t CapControls_Get_UseVoltOverride(void);

    /*
    With VoltOverride, swtich off whenever PT voltage exceeds this level.
    */
    DSS_CAPI_V7_DLL double CapControls_Get_Vmax(void);

    /*
    With VoltOverride, switch ON whenever PT voltage drops below this level.
    */
    DSS_CAPI_V7_DLL double CapControls_Get_Vmin(void);

    /*
    Name of the Capacitor that is controlled.
    */
    DSS_CAPI_V7_DLL void CapControls_Set_Capacitor(char* Value);

    /*
    Transducer ratio from pirmary current to control current.
    */
    DSS_CAPI_V7_DLL void CapControls_Set_CTratio(double Value);

    DSS_CAPI_V7_DLL void CapControls_Set_DeadTime(double Value);

    /*
    Time delay [s] to switch on after arming.  Control may reset before actually switching.
    */
    DSS_CAPI_V7_DLL void CapControls_Set_Delay(double Value);

    /*
    Time delay [s] before swithcing off a step. Control may reset before actually switching.
    */
    DSS_CAPI_V7_DLL void CapControls_Set_DelayOff(double Value);

    /*
    Type of automatic controller.
    */
    DSS_CAPI_V7_DLL void CapControls_Set_Mode(int32_t Value);

    /*
    Full name of the element that PT and CT are connected to.
    */
    DSS_CAPI_V7_DLL void CapControls_Set_MonitoredObj(char* Value);

    /*
    Terminal number on the element that PT and CT are connected to.
    */
    DSS_CAPI_V7_DLL void CapControls_Set_MonitoredTerm(int32_t Value);

    /*
    Sets a CapControl active by name.
    */
    DSS_CAPI_V7_DLL void CapControls_Set_Name(char* Value);

    /*
    Threshold to switch off a step. See Mode for units.
    */
    DSS_CAPI_V7_DLL void CapControls_Set_OFFSetting(double Value);

    /*
    Threshold to arm or switch on a step.  See Mode for units.
    */
    DSS_CAPI_V7_DLL void CapControls_Set_ONSetting(double Value);

    /*
    Transducer ratio from primary feeder to control voltage.
    */
    DSS_CAPI_V7_DLL void CapControls_Set_PTratio(double Value);

    /*
    Enables Vmin and Vmax to override the control Mode
    */
    DSS_CAPI_V7_DLL void CapControls_Set_UseVoltOverride(uint16_t Value);

    /*
    With VoltOverride, swtich off whenever PT voltage exceeds this level.
    */
    DSS_CAPI_V7_DLL void CapControls_Set_Vmax(double Value);

    /*
    With VoltOverride, switch ON whenever PT voltage drops below this level.
    */
    DSS_CAPI_V7_DLL void CapControls_Set_Vmin(double Value);

    /*
    Number of CapControls in Active Circuit
    */
    DSS_CAPI_V7_DLL int32_t CapControls_Get_Count(void);

    DSS_CAPI_V7_DLL void CapControls_Reset(void);

    /*
    Name of the active circuit.
    */
    DSS_CAPI_V7_DLL char* Circuit_Get_Name(void);

    /*
    Total number of Buses in the circuit.
    */
    DSS_CAPI_V7_DLL int32_t Circuit_Get_NumBuses(void);

    /*
    Number of CktElements in the circuit.
    */
    DSS_CAPI_V7_DLL int32_t Circuit_Get_NumCktElements(void);

    /*
    Total number of nodes in the circuit.
    */
    DSS_CAPI_V7_DLL int32_t Circuit_Get_NumNodes(void);

    /*
    Complex total line losses in the circuit
    */
    DSS_CAPI_V7_DLL void Circuit_Get_LineLosses(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Circuit_Get_LineLosses but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Circuit_Get_LineLosses_GR(void);

    /*
    Total losses in active circuit, complex number (two-element array of double).
    */
    DSS_CAPI_V7_DLL void Circuit_Get_Losses(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Circuit_Get_Losses but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Circuit_Get_Losses_GR(void);

    /*
    Array of magnitudes (doubles) of voltages at all buses
    */
    DSS_CAPI_V7_DLL void Circuit_Get_AllBusVmag(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Circuit_Get_AllBusVmag but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Circuit_Get_AllBusVmag_GR(void);

    /*
    Complex array of all bus, node voltages from most recent solution
    */
    DSS_CAPI_V7_DLL void Circuit_Get_AllBusVolts(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Circuit_Get_AllBusVolts but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Circuit_Get_AllBusVolts_GR(void);

    /*
    Array of strings containing Full Name of all elements.
    */
    DSS_CAPI_V7_DLL void Circuit_Get_AllElementNames(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as Circuit_Get_AllElementNames but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Circuit_Get_AllElementNames_GR(void);

    /*
    Complex losses in all transformers designated to substations.
    */
    DSS_CAPI_V7_DLL void Circuit_Get_SubstationLosses(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Circuit_Get_SubstationLosses but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Circuit_Get_SubstationLosses_GR(void);

    /*
    Total power, watts delivered to the circuit
    */
    DSS_CAPI_V7_DLL void Circuit_Get_TotalPower(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Circuit_Get_TotalPower but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Circuit_Get_TotalPower_GR(void);

    DSS_CAPI_V7_DLL void Circuit_Disable(char* Name);

    DSS_CAPI_V7_DLL void Circuit_Enable(char* Name);

    DSS_CAPI_V7_DLL int32_t Circuit_FirstPCElement(void);

    DSS_CAPI_V7_DLL int32_t Circuit_FirstPDElement(void);

    DSS_CAPI_V7_DLL int32_t Circuit_NextPCElement(void);

    DSS_CAPI_V7_DLL int32_t Circuit_NextPDElement(void);

    /*
    Array of strings containing names of all buses in circuit (see AllNodeNames).
    */
    DSS_CAPI_V7_DLL void Circuit_Get_AllBusNames(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as Circuit_Get_AllBusNames but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Circuit_Get_AllBusNames_GR(void);

    /*
    Array of total losses (complex) in each circuit element
    */
    DSS_CAPI_V7_DLL void Circuit_Get_AllElementLosses(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Circuit_Get_AllElementLosses but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Circuit_Get_AllElementLosses_GR(void);

    DSS_CAPI_V7_DLL void Circuit_Sample(void);

    DSS_CAPI_V7_DLL void Circuit_SaveSample(void);

    DSS_CAPI_V7_DLL int32_t Circuit_SetActiveElement(char* FullName);

    DSS_CAPI_V7_DLL double Circuit_Capacity(double Start, double Increment);

    /*
    Double Array of all bus voltages (each node) magnitudes in Per unit
    */
    DSS_CAPI_V7_DLL void Circuit_Get_AllBusVmagPu(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Circuit_Get_AllBusVmagPu but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Circuit_Get_AllBusVmagPu_GR(void);

    DSS_CAPI_V7_DLL int32_t Circuit_SetActiveBus(char* BusName);

    DSS_CAPI_V7_DLL int32_t Circuit_SetActiveBusi(int32_t BusIndex);

    /*
    Array of strings containing full name of each node in system in same order as returned by AllBusVolts, etc.
    */
    DSS_CAPI_V7_DLL void Circuit_Get_AllNodeNames(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as Circuit_Get_AllNodeNames but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Circuit_Get_AllNodeNames_GR(void);

    /*
    System Y matrix (after a solution has been performed)
    */
    DSS_CAPI_V7_DLL void Circuit_Get_SystemY(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Circuit_Get_SystemY but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Circuit_Get_SystemY_GR(void);

    /*
    Returns distance from each bus to parent EnergyMeter. Corresponds to sequence in AllBusNames.
    */
    DSS_CAPI_V7_DLL void Circuit_Get_AllBusDistances(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Circuit_Get_AllBusDistances but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Circuit_Get_AllBusDistances_GR(void);

    /*
    Returns an array of distances from parent EnergyMeter for each Node. Corresponds to AllBusVMag sequence.
    */
    DSS_CAPI_V7_DLL void Circuit_Get_AllNodeDistances(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Circuit_Get_AllNodeDistances but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Circuit_Get_AllNodeDistances_GR(void);

    /*
    Returns an array of doubles representing the distances to parent EnergyMeter. Sequence of array corresponds to other node ByPhase properties.
    */
    DSS_CAPI_V7_DLL void Circuit_Get_AllNodeDistancesByPhase(double** ResultPtr, int32_t* ResultCount, int32_t Phase);
    /*
    Same as Circuit_Get_AllNodeDistancesByPhase but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Circuit_Get_AllNodeDistancesByPhase_GR(int32_t Phase);

    /*
    Returns Array of doubles represent voltage magnitudes for nodes on the specified phase.
    */
    DSS_CAPI_V7_DLL void Circuit_Get_AllNodeVmagByPhase(double** ResultPtr, int32_t* ResultCount, int32_t Phase);
    /*
    Same as Circuit_Get_AllNodeVmagByPhase but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Circuit_Get_AllNodeVmagByPhase_GR(int32_t Phase);

    /*
    Returns array of per unit voltage magnitudes for each node by phase
    */
    DSS_CAPI_V7_DLL void Circuit_Get_AllNodeVmagPUByPhase(double** ResultPtr, int32_t* ResultCount, int32_t Phase);
    /*
    Same as Circuit_Get_AllNodeVmagPUByPhase but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Circuit_Get_AllNodeVmagPUByPhase_GR(int32_t Phase);

    /*
    Return array of strings of the node names for the By Phase criteria. Sequence corresponds to other ByPhase properties.
    */
    DSS_CAPI_V7_DLL void Circuit_Get_AllNodeNamesByPhase(char*** ResultPtr, int32_t* ResultCount, int32_t Phase);
    /*
    Same as Circuit_Get_AllNodeNamesByPhase but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Circuit_Get_AllNodeNamesByPhase_GR(int32_t Phase);

    DSS_CAPI_V7_DLL int32_t Circuit_SetActiveClass(char* ClassName);

    DSS_CAPI_V7_DLL int32_t Circuit_FirstElement(void);

    DSS_CAPI_V7_DLL int32_t Circuit_NextElement(void);

    DSS_CAPI_V7_DLL void Circuit_UpdateStorage(void);

    /*
    Sets Parent PD element, if any, to be the active circuit element and returns index>0; Returns 0 if it fails or not applicable.
    */
    DSS_CAPI_V7_DLL int32_t Circuit_Get_ParentPDElement(void);

    DSS_CAPI_V7_DLL void Circuit_EndOfTimeStepUpdate(void);

    /*
    Array of strings containing the names of the nodes in the same order as the Y matrix
    */
    DSS_CAPI_V7_DLL void Circuit_Get_YNodeOrder(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as Circuit_Get_YNodeOrder but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Circuit_Get_YNodeOrder_GR(void);

    /*
    Array of doubles containing complex injection currents for the present solution. Is is the "I" vector of I=YV
    */
    DSS_CAPI_V7_DLL void Circuit_Get_YCurrents(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Circuit_Get_YCurrents but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Circuit_Get_YCurrents_GR(void);

    /*
    Complex array of actual node voltages in same order as SystemY matrix.
    */
    DSS_CAPI_V7_DLL void Circuit_Get_YNodeVarray(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Circuit_Get_YNodeVarray but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Circuit_Get_YNodeVarray_GR(void);
    DSS_CAPI_V7_DLL void Circuit_SetCktElementName(char* Value);
    DSS_CAPI_V7_DLL void Circuit_SetCktElementIndex(int32_t Value);

    /*
    Array of strings. Get  Bus definitions to which each terminal is connected. 0-based array.
    */
    DSS_CAPI_V7_DLL void CktElement_Get_BusNames(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as CktElement_Get_BusNames but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void CktElement_Get_BusNames_GR(void);

    /*
    Full Name of Active Circuit Element
    */
    DSS_CAPI_V7_DLL char* CktElement_Get_Name(void);

    /*
    Number of Conductors per Terminal
    */
    DSS_CAPI_V7_DLL int32_t CktElement_Get_NumConductors(void);

    /*
    Number of Phases
    */
    DSS_CAPI_V7_DLL int32_t CktElement_Get_NumPhases(void);

    /*
    Number of Terminals this Circuit Element
    */
    DSS_CAPI_V7_DLL int32_t CktElement_Get_NumTerminals(void);

    /*
    Array of strings. Set Bus definitions for each terminal is connected.
    */
    DSS_CAPI_V7_DLL void CktElement_Set_BusNames(char** ValuePtr, int32_t ValueCount);

    /*
    Complex array of currents into each conductor of each terminal
    */
    DSS_CAPI_V7_DLL void CktElement_Get_Currents(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as CktElement_Get_Currents but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void CktElement_Get_Currents_GR(void);

    /*
    Complex array of voltages at terminals
    */
    DSS_CAPI_V7_DLL void CktElement_Get_Voltages(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as CktElement_Get_Voltages but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void CktElement_Get_Voltages_GR(void);

    /*
    Emergency Ampere Rating for PD elements
    */
    DSS_CAPI_V7_DLL double CktElement_Get_EmergAmps(void);

    /*
    Boolean indicating that element is currently in the circuit.
    */
    DSS_CAPI_V7_DLL uint16_t CktElement_Get_Enabled(void);

    /*
    Total losses in the element: two-element complex array
    */
    DSS_CAPI_V7_DLL void CktElement_Get_Losses(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as CktElement_Get_Losses but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void CktElement_Get_Losses_GR(void);

    /*
    Normal ampere rating for PD Elements
    */
    DSS_CAPI_V7_DLL double CktElement_Get_NormalAmps(void);

    /*
    Complex array of losses by phase
    */
    DSS_CAPI_V7_DLL void CktElement_Get_PhaseLosses(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as CktElement_Get_PhaseLosses but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void CktElement_Get_PhaseLosses_GR(void);

    /*
    Complex array of powers into each conductor of each terminal
    */
    DSS_CAPI_V7_DLL void CktElement_Get_Powers(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as CktElement_Get_Powers but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void CktElement_Get_Powers_GR(void);

    /*
    Double array of symmetrical component currents into each 3-phase terminal
    */
    DSS_CAPI_V7_DLL void CktElement_Get_SeqCurrents(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as CktElement_Get_SeqCurrents but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void CktElement_Get_SeqCurrents_GR(void);

    /*
    Double array of sequence powers into each 3-phase teminal
    */
    DSS_CAPI_V7_DLL void CktElement_Get_SeqPowers(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as CktElement_Get_SeqPowers but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void CktElement_Get_SeqPowers_GR(void);

    /*
    Double array of symmetrical component voltages at each 3-phase terminal
    */
    DSS_CAPI_V7_DLL void CktElement_Get_SeqVoltages(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as CktElement_Get_SeqVoltages but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void CktElement_Get_SeqVoltages_GR(void);

    DSS_CAPI_V7_DLL void CktElement_Close(int32_t Term, int32_t Phs);

    DSS_CAPI_V7_DLL void CktElement_Open(int32_t Term, int32_t Phs);

    /*
    Emergency Ampere Rating
    */
    DSS_CAPI_V7_DLL void CktElement_Set_EmergAmps(double Value);

    /*
    Boolean indicating that element is currently in the circuit.
    */
    DSS_CAPI_V7_DLL void CktElement_Set_Enabled(uint16_t Value);

    /*
    Normal ampere rating
    */
    DSS_CAPI_V7_DLL void CktElement_Set_NormalAmps(double Value);

    DSS_CAPI_V7_DLL uint16_t CktElement_IsOpen(int32_t Term, int32_t Phs);

    /*
    Array containing all property names of the active device.
    */
    DSS_CAPI_V7_DLL void CktElement_Get_AllPropertyNames(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as CktElement_Get_AllPropertyNames but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void CktElement_Get_AllPropertyNames_GR(void);

    /*
    Number of Properties this Circuit Element.
    */
    DSS_CAPI_V7_DLL int32_t CktElement_Get_NumProperties(void);

    /*
    Residual currents for each terminal: (mag, angle)
    */
    DSS_CAPI_V7_DLL void CktElement_Get_Residuals(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as CktElement_Get_Residuals but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void CktElement_Get_Residuals_GR(void);

    /*
    YPrim matrix, column order, complex numbers (paired)
    */
    DSS_CAPI_V7_DLL void CktElement_Get_Yprim(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as CktElement_Get_Yprim but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void CktElement_Get_Yprim_GR(void);

    /*
    Display name of the object (not necessarily unique)
    */
    DSS_CAPI_V7_DLL char* CktElement_Get_DisplayName(void);

    /*
    globally unique identifier for this object
    */
    DSS_CAPI_V7_DLL char* CktElement_Get_GUID(void);

    /*
    Pointer to this object
    */
    DSS_CAPI_V7_DLL int32_t CktElement_Get_Handle(void);

    /*
    Display name of the object (not necessarily unique)
    */
    DSS_CAPI_V7_DLL void CktElement_Set_DisplayName(char* Value);

    /*
    Full name of the i-th controller attached to this element. Ex: str = Controller(2).  See NumControls to determine valid index range
    */
    DSS_CAPI_V7_DLL char* CktElement_Get_Controller(int32_t idx);

    /*
    Name of the Energy Meter this element is assigned to.
    */
    DSS_CAPI_V7_DLL char* CktElement_Get_EnergyMeter(void);

    /*
    This element has a CapControl or RegControl attached.
    */
    DSS_CAPI_V7_DLL uint16_t CktElement_Get_HasVoltControl(void);

    /*
    This element has a SwtControl attached.
    */
    DSS_CAPI_V7_DLL uint16_t CktElement_Get_HasSwitchControl(void);

    /*
    Complex double array of Sequence Voltage for all terminals of active circuit element.
    */
    DSS_CAPI_V7_DLL void CktElement_Get_CplxSeqVoltages(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as CktElement_Get_CplxSeqVoltages but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void CktElement_Get_CplxSeqVoltages_GR(void);

    /*
    Complex double array of Sequence Currents for all conductors of all terminals of active circuit element.
    */
    DSS_CAPI_V7_DLL void CktElement_Get_CplxSeqCurrents(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as CktElement_Get_CplxSeqCurrents but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void CktElement_Get_CplxSeqCurrents_GR(void);

    /*
    Array of strings listing all the published variable names, if a PCElement. Otherwise, null string.
    */
    DSS_CAPI_V7_DLL void CktElement_Get_AllVariableNames(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as CktElement_Get_AllVariableNames but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void CktElement_Get_AllVariableNames_GR(void);

    /*
    Array of doubles. Values of state variables of active element if PC element.
    */
    DSS_CAPI_V7_DLL void CktElement_Get_AllVariableValues(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as CktElement_Get_AllVariableValues but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void CktElement_Get_AllVariableValues_GR(void);

    /*
    For PCElement, get the value of a variable by name. If Code>0 Then no variable by this name or not a PCelement.
    */
    DSS_CAPI_V7_DLL double CktElement_Get_Variable(char* MyVarName, int32_t *Code);

    /*
    For PCElement, get the value of a variable by integer index.
    */
    DSS_CAPI_V7_DLL double CktElement_Get_Variablei(int32_t Idx, int32_t *Code);

    /*
    Array of integer containing the node numbers (representing phases, for example) for each conductor of each terminal.
    */
    DSS_CAPI_V7_DLL void CktElement_Get_NodeOrder(int32_t** ResultPtr, int32_t* ResultCount);
    /*
    Same as CktElement_Get_NodeOrder but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void CktElement_Get_NodeOrder_GR(void);

    /*
    True if a recloser, relay, or fuse controlling this ckt element. OCP = Overcurrent Protection
    */
    DSS_CAPI_V7_DLL uint16_t CktElement_Get_HasOCPDevice(void);

    /*
    Number of controls connected to this device. Use to determine valid range for index into Controller array.
    */
    DSS_CAPI_V7_DLL int32_t CktElement_Get_NumControls(void);

    /*
    Index into Controller list of OCP Device controlling this CktElement
    */
    DSS_CAPI_V7_DLL int32_t CktElement_Get_OCPDevIndex(void);

    /*
    0=None; 1=Fuse; 2=Recloser; 3=Relay;  Type of OCP controller device
    */
    DSS_CAPI_V7_DLL int32_t CktElement_Get_OCPDevType(void);

    /*
    Currents in magnitude, angle format as a array of doubles.
    */
    DSS_CAPI_V7_DLL void CktElement_Get_CurrentsMagAng(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as CktElement_Get_CurrentsMagAng but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void CktElement_Get_CurrentsMagAng_GR(void);

    /*
    Voltages at each conductor in magnitude, angle form as array of doubles.
    */
    DSS_CAPI_V7_DLL void CktElement_Get_VoltagesMagAng(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as CktElement_Get_VoltagesMagAng but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void CktElement_Get_VoltagesMagAng_GR(void);

    /*
    Convert real and imaginary doubles to Array of doubles
    */
    DSS_CAPI_V7_DLL void CmathLib_Get_cmplx(double** ResultPtr, int32_t* ResultCount, double RealPart, double ImagPart);
    /*
    Same as CmathLib_Get_cmplx but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void CmathLib_Get_cmplx_GR(double RealPart, double ImagPart);

    /*
    Return abs value of complex number given in real and imag doubles
    */
    DSS_CAPI_V7_DLL double CmathLib_Get_cabs(double realpart, double imagpart);

    /*
    Returns the angle, in degrees, of a complex number specified as two doubles: Realpart and imagpart.
    */
    DSS_CAPI_V7_DLL double CmathLib_Get_cdang(double RealPart, double ImagPart);

    /*
    Convert complex number to magnitude and angle, degrees. Returns array of two doubles.
    */
    DSS_CAPI_V7_DLL void CmathLib_Get_ctopolardeg(double** ResultPtr, int32_t* ResultCount, double RealPart, double ImagPart);
    /*
    Same as CmathLib_Get_ctopolardeg but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void CmathLib_Get_ctopolardeg_GR(double RealPart, double ImagPart);

    /*
    Convert magnitude, angle in degrees to a complex number. Returns Array of two doubles.
    */
    DSS_CAPI_V7_DLL void CmathLib_Get_pdegtocomplex(double** ResultPtr, int32_t* ResultCount, double magnitude, double angle);
    /*
    Same as CmathLib_Get_pdegtocomplex but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void CmathLib_Get_pdegtocomplex_GR(double magnitude, double angle);

    /*
    Multiply two complex numbers: (a1, b1) * (a2, b2). Returns result as a array of two doubles.
    */
    DSS_CAPI_V7_DLL void CmathLib_Get_cmul(double** ResultPtr, int32_t* ResultCount, double a1, double b1, double a2, double b2);
    /*
    Same as CmathLib_Get_cmul but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void CmathLib_Get_cmul_GR(double a1, double b1, double a2, double b2);

    /*
    Divide two complex number: (a1, b1)/(a2, b2). Returns array of two doubles representing complex result.
    */
    DSS_CAPI_V7_DLL void CmathLib_Get_cdiv(double** ResultPtr, int32_t* ResultCount, double a1, double b1, double a2, double b2);
    /*
    Same as CmathLib_Get_cdiv but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void CmathLib_Get_cdiv_GR(double a1, double b1, double a2, double b2);

    DSS_CAPI_V7_DLL void CtrlQueue_ClearQueue(void);

    DSS_CAPI_V7_DLL void CtrlQueue_Delete(int32_t ActionHandle);

    /*
    Code for the active action. Long integer code to tell the control device what to do
    */
    DSS_CAPI_V7_DLL int32_t CtrlQueue_Get_ActionCode(void);

    /*
    Handle (User defined) to device that must act on the pending action.
    */
    DSS_CAPI_V7_DLL int32_t CtrlQueue_Get_DeviceHandle(void);

    /*
    Number of Actions on the current actionlist (that have been popped off the control queue by CheckControlActions)
    */
    DSS_CAPI_V7_DLL int32_t CtrlQueue_Get_NumActions(void);

    DSS_CAPI_V7_DLL void CtrlQueue_Show(void);

    DSS_CAPI_V7_DLL void CtrlQueue_ClearActions(void);

    /*
    Pops next action off the action list and makes it the active action. Returns zero if none.
    */
    DSS_CAPI_V7_DLL int32_t CtrlQueue_Get_PopAction(void);

    /*
    Set the active action by index
    */
    DSS_CAPI_V7_DLL void CtrlQueue_Set_Action(int32_t Param1);

    /*
    Number of items on the OpenDSS control Queue
    */
    DSS_CAPI_V7_DLL int32_t CtrlQueue_Get_QueueSize(void);

    DSS_CAPI_V7_DLL void CtrlQueue_DoAllQueue(void);

    /*
    Array of strings containing the entire queue in CSV format
    */
    DSS_CAPI_V7_DLL void CtrlQueue_Get_Queue(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as CtrlQueue_Get_Queue but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void CtrlQueue_Get_Queue_GR(void);

    /*
    Number of Circuits currently defined
    */
    DSS_CAPI_V7_DLL int32_t DSS_Get_NumCircuits(void);

    DSS_CAPI_V7_DLL void DSS_ClearAll(void);

    /*
    Get version string for the DSS.
    */
    DSS_CAPI_V7_DLL char* DSS_Get_Version(void);

    DSS_CAPI_V7_DLL uint16_t DSS_Start(int32_t code);

    /*
    List of DSS intrinsic classes (names of the classes)
    */
    DSS_CAPI_V7_DLL void DSS_Get_Classes(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as DSS_Get_Classes but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void DSS_Get_Classes_GR(void);

    /*
    List of user-defined classes
    */
    DSS_CAPI_V7_DLL void DSS_Get_UserClasses(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as DSS_Get_UserClasses but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void DSS_Get_UserClasses_GR(void);

    /*
    Number of DSS intrinsic classes
    */
    DSS_CAPI_V7_DLL int32_t DSS_Get_NumClasses(void);

    /*
    Number of user-defined classes
    */
    DSS_CAPI_V7_DLL int32_t DSS_Get_NumUserClasses(void);

    /*
    DSS Data File Path.  Default path for reports, etc. from DSS
    */
    DSS_CAPI_V7_DLL char* DSS_Get_DataPath(void);

    /*
    DSS Data File Path.  Default path for reports, etc. from DSS
    */
    DSS_CAPI_V7_DLL void DSS_Set_DataPath(char* Value);

    DSS_CAPI_V7_DLL void DSS_Reset(void);

    /*
    Returns the path name for the default text editor.
    */
    DSS_CAPI_V7_DLL char* DSS_Get_DefaultEditor(void);

    DSS_CAPI_V7_DLL int32_t DSS_SetActiveClass(char* ClassName);
    DSS_CAPI_V7_DLL uint16_t DSS_Get_AllowForms(void);
    DSS_CAPI_V7_DLL void DSS_Set_AllowForms(uint16_t Value);

    /*
    Array of strings containing the names of all properties for the active DSS object.
    */
    DSS_CAPI_V7_DLL void DSSElement_Get_AllPropertyNames(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as DSSElement_Get_AllPropertyNames but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void DSSElement_Get_AllPropertyNames_GR(void);

    /*
    Full Name of Active DSS Object (general element or circuit element).
    */
    DSS_CAPI_V7_DLL char* DSSElement_Get_Name(void);

    /*
    Number of Properties for the active DSS object.
    */
    DSS_CAPI_V7_DLL int32_t DSSElement_Get_NumProperties(void);

    DSS_CAPI_V7_DLL void DSSimComs_BusVoltagepu(double** ResultPtr, int32_t* ResultCount, size_t Index);
    /*
    Same as DSSimComs_BusVoltagepu but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void DSSimComs_BusVoltagepu_GR(size_t Index);

    DSS_CAPI_V7_DLL void DSSimComs_BusVoltage(double** ResultPtr, int32_t* ResultCount, size_t Index);
    /*
    Same as DSSimComs_BusVoltage but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void DSSimComs_BusVoltage_GR(size_t Index);

    DSS_CAPI_V7_DLL void DSSProgress_Close(void);

    /*
    Caption to appear on the bottom of the DSS Progress form.
    */
    DSS_CAPI_V7_DLL void DSSProgress_Set_Caption(char* Value);

    /*
    Percent progress to indicate [0..100]
    */
    DSS_CAPI_V7_DLL void DSSProgress_Set_PctProgress(int32_t Value);

    DSS_CAPI_V7_DLL void DSSProgress_Show(void);

    /*
    Description of the property.
    */
    DSS_CAPI_V7_DLL char* DSSProperty_Get_Description(void);

    /*
    Name of Property
    */
    DSS_CAPI_V7_DLL char* DSSProperty_Get_Name(void);

    DSS_CAPI_V7_DLL char* DSSProperty_Get_Val(void);

    DSS_CAPI_V7_DLL void DSSProperty_Set_Val(char* Value);
    DSS_CAPI_V7_DLL void DSSProperty_Set_Name(char* Value);
    DSS_CAPI_V7_DLL void DSSProperty_Set_Index(int32_t Value);

    /*
    Get i-th command
    */
    DSS_CAPI_V7_DLL char* DSS_Executive_Get_Command(int32_t i);

    /*
    Number of DSS Executive Commands
    */
    DSS_CAPI_V7_DLL int32_t DSS_Executive_Get_NumCommands(void);

    /*
    Number of DSS Executive Options
    */
    DSS_CAPI_V7_DLL int32_t DSS_Executive_Get_NumOptions(void);

    /*
    Get i-th option
    */
    DSS_CAPI_V7_DLL char* DSS_Executive_Get_Option(int32_t i);

    /*
    Get help string for i-th command
    */
    DSS_CAPI_V7_DLL char* DSS_Executive_Get_CommandHelp(int32_t i);

    /*
    Get help string for i-th option
    */
    DSS_CAPI_V7_DLL char* DSS_Executive_Get_OptionHelp(int32_t i);

    /*
    Get present value of i-th option
    */
    DSS_CAPI_V7_DLL char* DSS_Executive_Get_OptionValue(int32_t i);

    /*
    Description of error for last operation
    */
    DSS_CAPI_V7_DLL char* Error_Get_Description(void);

    /*
    Error Number (returns current value and then resets to zero)
    */
    DSS_CAPI_V7_DLL int32_t Error_Get_Number(void);

    /*
    Integer pointer to the Error Number. Remember to reset its value to zero after the error treatment.
    */
    DSS_CAPI_V7_DLL int32_t* Error_Get_NumberPtr(void);

    /*
    Array of strings containing names of all Fuses in the circuit
    */
    DSS_CAPI_V7_DLL void Fuses_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as Fuses_Get_AllNames but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Fuses_Get_AllNames_GR(void);

    /*
    Number of Fuse elements in the circuit
    */
    DSS_CAPI_V7_DLL int32_t Fuses_Get_Count(void);

    /*
    Set the first Fuse to be the active fuse. Returns 0 if none.
    */
    DSS_CAPI_V7_DLL int32_t Fuses_Get_First(void);

    /*
    Get the name of the active Fuse element
    */
    DSS_CAPI_V7_DLL char* Fuses_Get_Name(void);

    /*
    Advance the active Fuse element pointer to the next fuse. Returns 0 if no more fuses.
    */
    DSS_CAPI_V7_DLL int32_t Fuses_Get_Next(void);

    /*
    Set the active Fuse element by name.
    */
    DSS_CAPI_V7_DLL void Fuses_Set_Name(char* Value);

    /*
    Full name of the circuit element to which the fuse is connected.
    */
    DSS_CAPI_V7_DLL char* Fuses_Get_MonitoredObj(void);

    /*
    Terminal number to which the fuse is connected.
    */
    DSS_CAPI_V7_DLL int32_t Fuses_Get_MonitoredTerm(void);

    /*
    Full name of the circuit element switch that the fuse controls. Defaults to the MonitoredObj.
    */
    DSS_CAPI_V7_DLL char* Fuses_Get_SwitchedObj(void);

    /*
    Full name of the circuit element to which the fuse is connected.
    */
    DSS_CAPI_V7_DLL void Fuses_Set_MonitoredObj(char* Value);

    /*
    Number of the terminal to which the fuse is connected
    */
    DSS_CAPI_V7_DLL void Fuses_Set_MonitoredTerm(int32_t Value);

    /*
    Full name of the circuit element switch that the fuse controls. Defaults to MonitoredObj.
    */
    DSS_CAPI_V7_DLL void Fuses_Set_SwitchedObj(char* Value);

    /*
    Number of the terminal containing the switch controlled by the fuse.
    */
    DSS_CAPI_V7_DLL int32_t Fuses_Get_SwitchedTerm(void);

    /*
    Number of the terminal of the controlled element containing the switch controlled by the fuse.
    */
    DSS_CAPI_V7_DLL void Fuses_Set_SwitchedTerm(int32_t Value);

    /*
    Name of the TCCcurve object that determines fuse blowing.
    */
    DSS_CAPI_V7_DLL char* Fuses_Get_TCCcurve(void);

    /*
    Name of the TCCcurve object that determines fuse blowing.
    */
    DSS_CAPI_V7_DLL void Fuses_Set_TCCcurve(char* Value);

    /*
    Multiplier or actual amps for the TCCcurve object. Defaults to 1.0.  Multipliy current values of TCC curve by this to get actual amps.
    */
    DSS_CAPI_V7_DLL double Fuses_Get_RatedCurrent(void);

    /*
    Multiplier or actual fuse amps for the TCC curve. Defaults to 1.0. Has to correspond to the Current axis of TCCcurve object.
    */
    DSS_CAPI_V7_DLL void Fuses_Set_RatedCurrent(double Value);

    /*
    A fixed delay time in seconds added to the fuse blowing time determined by the TCC curve. Default is 0.
    */
    DSS_CAPI_V7_DLL double Fuses_Get_Delay(void);

    DSS_CAPI_V7_DLL void Fuses_Open(void);

    DSS_CAPI_V7_DLL void Fuses_Close(void);

    /*
    Fixed delay time in seconds added to the fuse blowing time to represent fuse clear or other delay.
    */
    DSS_CAPI_V7_DLL void Fuses_Set_Delay(double Value);

    DSS_CAPI_V7_DLL uint16_t Fuses_IsBlown(void);

    /*
    Get/set active fuse by index into the list of fuses. 1 based: 1..count
    */
    DSS_CAPI_V7_DLL int32_t Fuses_Get_idx(void);

    /*
    Set Fuse active by index into the list of fuses. 1..count
    */
    DSS_CAPI_V7_DLL void Fuses_Set_idx(int32_t Value);

    /*
    Number of phases, this fuse.
    */
    DSS_CAPI_V7_DLL int32_t Fuses_Get_NumPhases(void);

    /*
    Array of names of all Generator objects.
    */
    DSS_CAPI_V7_DLL void Generators_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as Generators_Get_AllNames but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Generators_Get_AllNames_GR(void);

    /*
    Sets first Generator to be active.  Returns 0 if none.
    */
    DSS_CAPI_V7_DLL int32_t Generators_Get_First(void);

    /*
    Sets a generator active by name.
    */
    DSS_CAPI_V7_DLL char* Generators_Get_Name(void);

    /*
    Sets next Generator to be active.  Returns 0 if no more.
    */
    DSS_CAPI_V7_DLL int32_t Generators_Get_Next(void);

    /*
    Array of Names of all generator energy meter registers
    */
    DSS_CAPI_V7_DLL void Generators_Get_RegisterNames(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as Generators_Get_RegisterNames but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Generators_Get_RegisterNames_GR(void);

    /*
    Array of valus in generator energy meter registers.
    */
    DSS_CAPI_V7_DLL void Generators_Get_RegisterValues(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Generators_Get_RegisterValues but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Generators_Get_RegisterValues_GR(void);

    /*
    Indicates whether the generator is forced ON regardles of other dispatch criteria.
    */
    DSS_CAPI_V7_DLL uint16_t Generators_Get_ForcedON(void);

    /*
    Indicates whether the generator is forced ON regardles of other dispatch criteria.
    */
    DSS_CAPI_V7_DLL void Generators_Set_ForcedON(uint16_t Value);

    /*
    Sets a generator active by name.
    */
    DSS_CAPI_V7_DLL void Generators_Set_Name(char* Value);

    /*
    Voltage base for the active generator, kV
    */
    DSS_CAPI_V7_DLL double Generators_Get_kV(void);

    /*
    kvar output for the active generator. Updates power factor based on present kW value.
    */
    DSS_CAPI_V7_DLL double Generators_Get_kvar(void);

    /*
    kW output for the active generator. kvar is updated for current power factor.
    */
    DSS_CAPI_V7_DLL double Generators_Get_kW(void);

    /*
    Power factor (pos. = producing vars). Updates kvar based on present kW value.
    */
    DSS_CAPI_V7_DLL double Generators_Get_PF(void);

    /*
    Number of phases
    */
    DSS_CAPI_V7_DLL int32_t Generators_Get_Phases(void);

    /*
    Voltage base for the active generator, kV
    */
    DSS_CAPI_V7_DLL void Generators_Set_kV(double Value);

    /*
    kvar output for the active generator. Updates power factor based on present kW.
    */
    DSS_CAPI_V7_DLL void Generators_Set_kvar(double Value);

    /*
    kW output for the active generator. kvar is updated for current power factor
    */
    DSS_CAPI_V7_DLL void Generators_Set_kW(double Value);

    /*
    Power factor (pos. = producing vars). Updates kvar based on present kW value.
    */
    DSS_CAPI_V7_DLL void Generators_Set_PF(double Value);

    /*
    Number of phases
    */
    DSS_CAPI_V7_DLL void Generators_Set_Phases(int32_t Value);

    /*
    Number of Generator Objects in Active Circuit
    */
    DSS_CAPI_V7_DLL int32_t Generators_Get_Count(void);

    /*
    Get/Set active Generator by index into generators list.  1..Count
    */
    DSS_CAPI_V7_DLL int32_t Generators_Get_idx(void);

    /*
    Get/Set active Generator by index into generators list. 1..Count
    */
    DSS_CAPI_V7_DLL void Generators_Set_idx(int32_t Value);

    /*
    Generator Model
    */
    DSS_CAPI_V7_DLL int32_t Generators_Get_Model(void);

    /*
    Generator Model
    */
    DSS_CAPI_V7_DLL void Generators_Set_Model(int32_t Value);

    /*
    kVA rating of the generator
    */
    DSS_CAPI_V7_DLL double Generators_Get_kVArated(void);

    /*
    KVA Rating of the generator
    */
    DSS_CAPI_V7_DLL void Generators_Set_kVArated(double Value);

    /*
    vmaxpu for Generator model
    */
    DSS_CAPI_V7_DLL double Generators_Get_Vmaxpu(void);

    /*
    Vminpu for Generator model
    */
    DSS_CAPI_V7_DLL double Generators_Get_Vminpu(void);

    /*
    Vmaxpu for generator model
    */
    DSS_CAPI_V7_DLL void Generators_Set_Vmaxpu(double Value);

    /*
    Vminpu for Generator model
    */
    DSS_CAPI_V7_DLL void Generators_Set_Vminpu(double Value);

    /*
    Array of strings containing names of all ISOURCE elements.
    */
    DSS_CAPI_V7_DLL void ISources_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as ISources_Get_AllNames but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void ISources_Get_AllNames_GR(void);

    /*
    Count: Number of ISOURCE elements.
    */
    DSS_CAPI_V7_DLL int32_t ISources_Get_Count(void);

    /*
    Set the First ISOURCE to be active; returns Zero if none.
    */
    DSS_CAPI_V7_DLL int32_t ISources_Get_First(void);

    /*
    Sets the next ISOURCE element to be the active one. Returns Zero if no more.
    */
    DSS_CAPI_V7_DLL int32_t ISources_Get_Next(void);

    /*
    Get name of active ISOURCE
    */
    DSS_CAPI_V7_DLL char* ISources_Get_Name(void);

    /*
    Set Active ISOURCE by name
    */
    DSS_CAPI_V7_DLL void ISources_Set_Name(char* Value);

    /*
    Get the magnitude of the ISOURCE in amps
    */
    DSS_CAPI_V7_DLL double ISources_Get_Amps(void);

    /*
    Set the magnitude of the ISOURCE, amps
    */
    DSS_CAPI_V7_DLL void ISources_Set_Amps(double Value);

    /*
    Phase angle for ISOURCE, degrees
    */
    DSS_CAPI_V7_DLL double ISources_Get_AngleDeg(void);

    /*
    The present frequency of the ISOURCE, Hz
    */
    DSS_CAPI_V7_DLL double ISources_Get_Frequency(void);

    /*
    Phase angle for ISOURCE, degrees
    */
    DSS_CAPI_V7_DLL void ISources_Set_AngleDeg(double Value);

    /*
    Set the present frequency for the ISOURCE
    */
    DSS_CAPI_V7_DLL void ISources_Set_Frequency(double Value);

    /*
    Number of LineCodes
    */
    DSS_CAPI_V7_DLL int32_t LineCodes_Get_Count(void);

    DSS_CAPI_V7_DLL int32_t LineCodes_Get_First(void);

    DSS_CAPI_V7_DLL int32_t LineCodes_Get_Next(void);

    /*
    Name of active LineCode
    */
    DSS_CAPI_V7_DLL char* LineCodes_Get_Name(void);

    DSS_CAPI_V7_DLL void LineCodes_Set_Name(char* Value);

    /*
    Flag denoting whether impedance data were entered in symmetrical components
    */
    DSS_CAPI_V7_DLL uint16_t LineCodes_Get_IsZ1Z0(void);

    DSS_CAPI_V7_DLL int32_t LineCodes_Get_Units(void);

    DSS_CAPI_V7_DLL void LineCodes_Set_Units(int32_t Value);

    /*
    Number of Phases
    */
    DSS_CAPI_V7_DLL int32_t LineCodes_Get_Phases(void);

    /*
    Number of Phases
    */
    DSS_CAPI_V7_DLL void LineCodes_Set_Phases(int32_t Value);

    /*
    Positive-sequence resistance ohms per unit length
    */
    DSS_CAPI_V7_DLL double LineCodes_Get_R1(void);

    DSS_CAPI_V7_DLL void LineCodes_Set_R1(double Value);

    /*
    Posiive-sequence reactance, ohms per unit length
    */
    DSS_CAPI_V7_DLL double LineCodes_Get_X1(void);

    DSS_CAPI_V7_DLL void LineCodes_Set_X1(double Value);

    /*
    Zero-Sequence Resistance, ohms per unit length
    */
    DSS_CAPI_V7_DLL double LineCodes_Get_R0(void);

    /*
    Zero Sequence Reactance, Ohms per unit length
    */
    DSS_CAPI_V7_DLL double LineCodes_Get_X0(void);

    DSS_CAPI_V7_DLL void LineCodes_Set_R0(double Value);

    DSS_CAPI_V7_DLL void LineCodes_Set_X0(double Value);

    /*
    Zero-sequence capacitance, nF per unit length
    */
    DSS_CAPI_V7_DLL double LineCodes_Get_C0(void);

    /*
    Positive-sequence capacitance, nF per unit length
    */
    DSS_CAPI_V7_DLL double LineCodes_Get_C1(void);

    DSS_CAPI_V7_DLL void LineCodes_Set_C0(double Value);

    DSS_CAPI_V7_DLL void LineCodes_Set_C1(double Value);

    /*
    Capacitance matrix, nF per unit length
    */
    DSS_CAPI_V7_DLL void LineCodes_Get_Cmatrix(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as LineCodes_Get_Cmatrix but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void LineCodes_Get_Cmatrix_GR(void);

    /*
    Resistance matrix, ohms per unit length
    */
    DSS_CAPI_V7_DLL void LineCodes_Get_Rmatrix(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as LineCodes_Get_Rmatrix but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void LineCodes_Get_Rmatrix_GR(void);

    /*
    Reactance matrix, ohms per unit length
    */
    DSS_CAPI_V7_DLL void LineCodes_Get_Xmatrix(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as LineCodes_Get_Xmatrix but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void LineCodes_Get_Xmatrix_GR(void);

    DSS_CAPI_V7_DLL void LineCodes_Set_Cmatrix(double* ValuePtr, int32_t ValueCount);

    DSS_CAPI_V7_DLL void LineCodes_Set_Rmatrix(double* ValuePtr, int32_t ValueCount);

    DSS_CAPI_V7_DLL void LineCodes_Set_Xmatrix(double* ValuePtr, int32_t ValueCount);

    /*
    Normal Ampere rating
    */
    DSS_CAPI_V7_DLL double LineCodes_Get_NormAmps(void);

    DSS_CAPI_V7_DLL void LineCodes_Set_NormAmps(double Value);

    /*
    Emergency ampere rating
    */
    DSS_CAPI_V7_DLL double LineCodes_Get_EmergAmps(void);

    DSS_CAPI_V7_DLL void LineCodes_Set_EmergAmps(double Value);

    /*
    Array of strings with names of all devices
    */
    DSS_CAPI_V7_DLL void LineCodes_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as LineCodes_Get_AllNames but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void LineCodes_Get_AllNames_GR(void);

    /*
    Names of all Line Objects
    */
    DSS_CAPI_V7_DLL void Lines_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as Lines_Get_AllNames but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Lines_Get_AllNames_GR(void);

    /*
    Name of bus for terminal 1.
    */
    DSS_CAPI_V7_DLL char* Lines_Get_Bus1(void);

    /*
    Name of bus for terminal 2.
    */
    DSS_CAPI_V7_DLL char* Lines_Get_Bus2(void);

    /*
    Invoking this property sets the first element active.  Returns 0 if no lines.  Otherwise, index of the line element.
    */
    DSS_CAPI_V7_DLL int32_t Lines_Get_First(void);

    /*
    Length of line section in units compatible with the LineCode definition.
    */
    DSS_CAPI_V7_DLL double Lines_Get_Length(void);

    /*
    Name of LineCode object that defines the impedances.
    */
    DSS_CAPI_V7_DLL char* Lines_Get_LineCode(void);

    /*
    Specify the name of the Line element to set it active.
    */
    DSS_CAPI_V7_DLL char* Lines_Get_Name(void);

    /*
    Invoking this property advances to the next Line element active.  Returns 0 if no more lines.  Otherwise, index of the line element.
    */
    DSS_CAPI_V7_DLL int32_t Lines_Get_Next(void);

    /*
    Number of Phases, this Line element.
    */
    DSS_CAPI_V7_DLL int32_t Lines_Get_Phases(void);

    /*
    Positive Sequence resistance, ohms per unit length.
    */
    DSS_CAPI_V7_DLL double Lines_Get_R1(void);

    /*
    Positive Sequence reactance, ohms per unit length.
    */
    DSS_CAPI_V7_DLL double Lines_Get_X1(void);

    DSS_CAPI_V7_DLL int32_t Lines_New(char* Name);

    /*
    Name of bus for terminal 1.
    */
    DSS_CAPI_V7_DLL void Lines_Set_Bus1(char* Value);

    /*
    Name of bus for terminal 2.
    */
    DSS_CAPI_V7_DLL void Lines_Set_Bus2(char* Value);

    /*
    Length of line section in units compatible with the LineCode definition.
    */
    DSS_CAPI_V7_DLL void Lines_Set_Length(double Value);

    /*
    Name of LineCode object that defines the impedances.
    */
    DSS_CAPI_V7_DLL void Lines_Set_LineCode(char* Value);

    /*
    Specify the name of the Line element to set it active.
    */
    DSS_CAPI_V7_DLL void Lines_Set_Name(char* Value);

    /*
    Number of Phases, this Line element.
    */
    DSS_CAPI_V7_DLL void Lines_Set_Phases(int32_t Value);

    /*
    Positive Sequence resistance, ohms per unit length.
    */
    DSS_CAPI_V7_DLL void Lines_Set_R1(double Value);

    /*
    Positive Sequence reactance, ohms per unit length.
    */
    DSS_CAPI_V7_DLL void Lines_Set_X1(double Value);

    /*
    Zero Sequence capacitance, nanofarads per unit length.
    */
    DSS_CAPI_V7_DLL double Lines_Get_C0(void);

    /*
    Positive Sequence capacitance, nanofarads per unit length.
    */
    DSS_CAPI_V7_DLL double Lines_Get_C1(void);

    DSS_CAPI_V7_DLL void Lines_Get_Cmatrix(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Lines_Get_Cmatrix but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Lines_Get_Cmatrix_GR(void);

    /*
    Zero Sequence resistance, ohms per unit length.
    */
    DSS_CAPI_V7_DLL double Lines_Get_R0(void);

    /*
    Resistance matrix (full), ohms per unit length. Array of doubles.
    */
    DSS_CAPI_V7_DLL void Lines_Get_Rmatrix(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Lines_Get_Rmatrix but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Lines_Get_Rmatrix_GR(void);

    /*
    Zero Sequence reactance ohms per unit length.
    */
    DSS_CAPI_V7_DLL double Lines_Get_X0(void);

    DSS_CAPI_V7_DLL void Lines_Get_Xmatrix(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Lines_Get_Xmatrix but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Lines_Get_Xmatrix_GR(void);

    /*
    Zero Sequence capacitance, nanofarads per unit length.
    */
    DSS_CAPI_V7_DLL void Lines_Set_C0(double Value);

    /*
    Positive Sequence capacitance, nanofarads per unit length.
    */
    DSS_CAPI_V7_DLL void Lines_Set_C1(double Value);

    DSS_CAPI_V7_DLL void Lines_Set_Cmatrix(double* ValuePtr, int32_t ValueCount);

    /*
    Zero Sequence resistance, ohms per unit length.
    */
    DSS_CAPI_V7_DLL void Lines_Set_R0(double Value);

    /*
    Resistance matrix (full), ohms per unit length. Array of doubles.
    */
    DSS_CAPI_V7_DLL void Lines_Set_Rmatrix(double* ValuePtr, int32_t ValueCount);

    /*
    Zero Sequence reactance ohms per unit length.
    */
    DSS_CAPI_V7_DLL void Lines_Set_X0(double Value);

    DSS_CAPI_V7_DLL void Lines_Set_Xmatrix(double* ValuePtr, int32_t ValueCount);

    /*
    Emergency (maximum) ampere rating of Line.
    */
    DSS_CAPI_V7_DLL double Lines_Get_EmergAmps(void);

    /*
    Normal ampere rating of Line.
    */
    DSS_CAPI_V7_DLL double Lines_Get_NormAmps(void);

    /*
    Emergency (maximum) ampere rating of Line.
    */
    DSS_CAPI_V7_DLL void Lines_Set_EmergAmps(double Value);

    /*
    Normal ampere rating of Line.
    */
    DSS_CAPI_V7_DLL void Lines_Set_NormAmps(double Value);

    /*
    Line geometry code
    */
    DSS_CAPI_V7_DLL char* Lines_Get_Geometry(void);

    /*
    Line geometry code
    */
    DSS_CAPI_V7_DLL void Lines_Set_Geometry(char* Value);

    /*
    Earth return resistance value used to compute line impedances at power frequency
    */
    DSS_CAPI_V7_DLL double Lines_Get_Rg(void);

    /*
    Earth Resistivity, m-ohms
    */
    DSS_CAPI_V7_DLL double Lines_Get_Rho(void);

    /*
    Earth return reactance value used to compute line impedances at power frequency
    */
    DSS_CAPI_V7_DLL double Lines_Get_Xg(void);

    /*
    Earth return resistance value used to compute line impedances at power frequency
    */
    DSS_CAPI_V7_DLL void Lines_Set_Rg(double Value);

    /*
    Earth Resistivity, m-ohms
    */
    DSS_CAPI_V7_DLL void Lines_Set_Rho(double Value);

    /*
    Earth return reactance value used to compute line impedances at power frequency
    */
    DSS_CAPI_V7_DLL void Lines_Set_Xg(double Value);

    /*
    Yprimitive: Does Nothing at present on Put; Dangerous
    */
    DSS_CAPI_V7_DLL void Lines_Get_Yprim(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Lines_Get_Yprim but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Lines_Get_Yprim_GR(void);

    /*
    Yprimitive: Does Nothing at present on Put; Dangerous
    */
    DSS_CAPI_V7_DLL void Lines_Set_Yprim(double* ValuePtr, int32_t ValueCount);

    /*
    Number of customers on this line section.
    */
    DSS_CAPI_V7_DLL int32_t Lines_Get_NumCust(void);

    /*
    Total Number of customers served from this line section.
    */
    DSS_CAPI_V7_DLL int32_t Lines_Get_TotalCust(void);

    /*
    Sets Parent of the active Line to be the active line. Returns 0 if no parent or action fails.
    */
    DSS_CAPI_V7_DLL int32_t Lines_Get_Parent(void);

    /*
    Number of Line objects in Active Circuit.
    */
    DSS_CAPI_V7_DLL int32_t Lines_Get_Count(void);

    /*
    Line spacing code
    */
    DSS_CAPI_V7_DLL char* Lines_Get_Spacing(void);

    /*
    Line spacing code
    */
    DSS_CAPI_V7_DLL void Lines_Set_Spacing(char* Value);

    DSS_CAPI_V7_DLL int32_t Lines_Get_Units(void);

    DSS_CAPI_V7_DLL void Lines_Set_Units(int32_t Value);

    /*
    Array of strings containing all Load names
    */
    DSS_CAPI_V7_DLL void Loads_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as Loads_Get_AllNames but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Loads_Get_AllNames_GR(void);

    /*
    Set first Load element to be active; returns 0 if none.
    */
    DSS_CAPI_V7_DLL int32_t Loads_Get_First(void);

    DSS_CAPI_V7_DLL int32_t Loads_Get_idx(void);

    /*
    Set active load by name.
    */
    DSS_CAPI_V7_DLL char* Loads_Get_Name(void);

    /*
    Sets next Load element to be active; returns 0 of none else index of active load.
    */
    DSS_CAPI_V7_DLL int32_t Loads_Get_Next(void);

    DSS_CAPI_V7_DLL void Loads_Set_idx(int32_t Value);

    /*
    Set active load by name.
    */
    DSS_CAPI_V7_DLL void Loads_Set_Name(char* Value);

    /*
    Set kV rating for active Load. For 2 or more phases set Line-Line kV. Else actual kV across terminals.
    */
    DSS_CAPI_V7_DLL double Loads_Get_kV(void);

    /*
    Set kvar for active Load. Updates PF based in present kW.
    */
    DSS_CAPI_V7_DLL double Loads_Get_kvar(void);

    /*
    Set kW for active Load. Updates kvar based on present PF.
    */
    DSS_CAPI_V7_DLL double Loads_Get_kW(void);

    /*
    Set Power Factor for Active Load. Specify leading PF as negative. Updates kvar based on kW value
    */
    DSS_CAPI_V7_DLL double Loads_Get_PF(void);

    /*
    Set kV rating for active Load. For 2 or more phases set Line-Line kV. Else actual kV across terminals.
    */
    DSS_CAPI_V7_DLL void Loads_Set_kV(double Value);

    /*
    Set kvar for active Load. Updates PF based on present kW.
    */
    DSS_CAPI_V7_DLL void Loads_Set_kvar(double Value);

    /*
    Set kW for active Load. Updates kvar based on present PF.
    */
    DSS_CAPI_V7_DLL void Loads_Set_kW(double Value);

    /*
    Set Power Factor for Active Load. Specify leading PF as negative. Updates kvar based on present value of kW.
    */
    DSS_CAPI_V7_DLL void Loads_Set_PF(double Value);

    /*
    Number of Load objects in active circuit.
    */
    DSS_CAPI_V7_DLL int32_t Loads_Get_Count(void);

    /*
    Factor for allocating loads by connected xfkva
    */
    DSS_CAPI_V7_DLL double Loads_Get_AllocationFactor(void);

    /*
    Factor relates average to peak kw.  Used for allocation with kwh and kwhdays/
    */
    DSS_CAPI_V7_DLL double Loads_Get_Cfactor(void);

    DSS_CAPI_V7_DLL int32_t Loads_Get_Class_(void);

    /*
    Name of a loadshape with both Mult and Qmult, for CVR factors as a function of time.
    */
    DSS_CAPI_V7_DLL char* Loads_Get_CVRcurve(void);

    /*
    Percent reduction in Q for percent reduction in V. Must be used with dssLoadModelCVR.
    */
    DSS_CAPI_V7_DLL double Loads_Get_CVRvars(void);

    /*
    Percent reduction in P for percent reduction in V. Must be used with dssLoadModelCVR.
    */
    DSS_CAPI_V7_DLL double Loads_Get_CVRwatts(void);

    /*
    Name of the loadshape for a daily load profile.
    */
    DSS_CAPI_V7_DLL char* Loads_Get_daily(void);

    /*
    Name of the loadshape for a duty cycle simulation.
    */
    DSS_CAPI_V7_DLL char* Loads_Get_duty(void);

    /*
    Name of the growthshape curve for yearly load growth factors.
    */
    DSS_CAPI_V7_DLL char* Loads_Get_Growth(void);

    /*
    Delta loads are connected line-to-line.
    */
    DSS_CAPI_V7_DLL uint16_t Loads_Get_IsDelta(void);

    /*
    Base load kva. Also defined kw and kvar or pf input, or load allocation by kwh or xfkva.
    */
    DSS_CAPI_V7_DLL double Loads_Get_kva(void);

    /*
    kwh billed for this period. Can be used with Cfactor for load allocation.
    */
    DSS_CAPI_V7_DLL double Loads_Get_kwh(void);

    /*
    Length of kwh billing period for average demand calculation. Default 30.
    */
    DSS_CAPI_V7_DLL double Loads_Get_kwhdays(void);

    /*
    The Load Model defines variation of P and Q with voltage.
    */
    DSS_CAPI_V7_DLL int32_t Loads_Get_Model(void);

    /*
    Number of customers in this load, defaults to one.
    */
    DSS_CAPI_V7_DLL int32_t Loads_Get_NumCust(void);

    /*
    Average percent of nominal load in Monte Carlo studies; only if no loadshape defined for this load.
    */
    DSS_CAPI_V7_DLL double Loads_Get_PctMean(void);

    /*
    Percent standard deviation for Monte Carlo load studies; if there is no loadshape assigned to this load.
    */
    DSS_CAPI_V7_DLL double Loads_Get_PctStdDev(void);

    /*
    Neutral resistance for wye-connected loads.
    */
    DSS_CAPI_V7_DLL double Loads_Get_Rneut(void);

    /*
    Name of harmonic current spectrrum shape.
    */
    DSS_CAPI_V7_DLL char* Loads_Get_Spectrum(void);

    /*
    Response to load multipliers: Fixed (growth only), Exempt (no LD curve), Variable (all).
    */
    DSS_CAPI_V7_DLL int32_t Loads_Get_Status(void);

    /*
    Maximum per-unit voltage to use the load model. Above this, constant Z applies.
    */
    DSS_CAPI_V7_DLL double Loads_Get_Vmaxpu(void);

    /*
    Minimum voltage for unserved energy (UE) evaluation.
    */
    DSS_CAPI_V7_DLL double Loads_Get_Vminemerg(void);

    /*
    Minimum voltage for energy exceeding normal (EEN) evaluations.
    */
    DSS_CAPI_V7_DLL double Loads_Get_Vminnorm(void);

    /*
    Minimum voltage to apply the load model. Below this, constant Z is used.
    */
    DSS_CAPI_V7_DLL double Loads_Get_Vminpu(void);

    /*
    Rated service transformer kVA for load allocation, using AllocationFactor. Affects kW, kvar, and pf.
    */
    DSS_CAPI_V7_DLL double Loads_Get_xfkVA(void);

    /*
    Neutral reactance for wye-connected loads.
    */
    DSS_CAPI_V7_DLL double Loads_Get_Xneut(void);

    /*
    Name of yearly duration loadshape
    */
    DSS_CAPI_V7_DLL char* Loads_Get_Yearly(void);

    DSS_CAPI_V7_DLL void Loads_Set_AllocationFactor(double Value);

    DSS_CAPI_V7_DLL void Loads_Set_Cfactor(double Value);

    DSS_CAPI_V7_DLL void Loads_Set_Class_(int32_t Value);

    DSS_CAPI_V7_DLL void Loads_Set_CVRcurve(char* Value);

    DSS_CAPI_V7_DLL void Loads_Set_CVRvars(double Value);

    DSS_CAPI_V7_DLL void Loads_Set_CVRwatts(double Value);

    DSS_CAPI_V7_DLL void Loads_Set_daily(char* Value);

    DSS_CAPI_V7_DLL void Loads_Set_duty(char* Value);

    DSS_CAPI_V7_DLL void Loads_Set_Growth(char* Value);

    DSS_CAPI_V7_DLL void Loads_Set_IsDelta(uint16_t Value);

    DSS_CAPI_V7_DLL void Loads_Set_kva(double Value);

    DSS_CAPI_V7_DLL void Loads_Set_kwh(double Value);

    DSS_CAPI_V7_DLL void Loads_Set_kwhdays(double Value);

    DSS_CAPI_V7_DLL void Loads_Set_Model(int32_t Value);

    DSS_CAPI_V7_DLL void Loads_Set_NumCust(int32_t Value);

    DSS_CAPI_V7_DLL void Loads_Set_PctMean(double Value);

    DSS_CAPI_V7_DLL void Loads_Set_PctStdDev(double Value);

    DSS_CAPI_V7_DLL void Loads_Set_Rneut(double Value);

    DSS_CAPI_V7_DLL void Loads_Set_Spectrum(char* Value);

    DSS_CAPI_V7_DLL void Loads_Set_Status(int32_t Value);

    DSS_CAPI_V7_DLL void Loads_Set_Vmaxpu(double Value);

    DSS_CAPI_V7_DLL void Loads_Set_Vminemerg(double Value);

    DSS_CAPI_V7_DLL void Loads_Set_Vminnorm(double Value);

    DSS_CAPI_V7_DLL void Loads_Set_Vminpu(double Value);

    DSS_CAPI_V7_DLL void Loads_Set_xfkVA(double Value);

    DSS_CAPI_V7_DLL void Loads_Set_Xneut(double Value);

    DSS_CAPI_V7_DLL void Loads_Set_Yearly(char* Value);

    /*
    Array of 7  doubles with values for ZIPV property of the LOAD object
    */
    DSS_CAPI_V7_DLL void Loads_Get_ZIPV(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Loads_Get_ZIPV but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Loads_Get_ZIPV_GR(void);

    DSS_CAPI_V7_DLL void Loads_Set_ZIPV(double* ValuePtr, int32_t ValueCount);

    DSS_CAPI_V7_DLL double Loads_Get_pctSeriesRL(void);

    /*
    Percent of Load that is modeled as series R-L for harmonics studies
    */
    DSS_CAPI_V7_DLL void Loads_Set_pctSeriesRL(double Value);

    /*
    Relative Weighting factor for the active LOAD
    */
    DSS_CAPI_V7_DLL double Loads_Get_RelWeight(void);

    /*
    Relative Weighting factor for the active LOAD
    */
    DSS_CAPI_V7_DLL void Loads_Set_RelWeight(double Value);

    /*
    Get the Name of the active Loadshape
    */
    DSS_CAPI_V7_DLL char* LoadShapes_Get_Name(void);

    /*
    Set the active Loadshape by name
    */
    DSS_CAPI_V7_DLL void LoadShapes_Set_Name(char* Value);

    /*
    Number of Loadshape objects currently defined in Loadshape collection
    */
    DSS_CAPI_V7_DLL int32_t LoadShapes_Get_Count(void);

    /*
    Set the first loadshape active and return integer index of the loadshape. Returns 0 if none.
    */
    DSS_CAPI_V7_DLL int32_t LoadShapes_Get_First(void);

    /*
    Advance active Loadshape to the next on in the collection. Returns 0 if no more loadshapes.
    */
    DSS_CAPI_V7_DLL int32_t LoadShapes_Get_Next(void);

    /*
    Array of strings containing names of all Loadshape objects currently defined.
    */
    DSS_CAPI_V7_DLL void LoadShapes_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as LoadShapes_Get_AllNames but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void LoadShapes_Get_AllNames_GR(void);

    /*
    Get Number of points in active Loadshape.
    */
    DSS_CAPI_V7_DLL int32_t LoadShapes_Get_Npts(void);

    /*
    Array of Doubles for the P multiplier in the Loadshape.
    */
    DSS_CAPI_V7_DLL void LoadShapes_Get_Pmult(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as LoadShapes_Get_Pmult but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void LoadShapes_Get_Pmult_GR(void);

    /*
    Array of doubles containing the Q multipliers.
    */
    DSS_CAPI_V7_DLL void LoadShapes_Get_Qmult(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as LoadShapes_Get_Qmult but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void LoadShapes_Get_Qmult_GR(void);

    /*
    Set number of points to allocate for active Loadshape.
    */
    DSS_CAPI_V7_DLL void LoadShapes_Set_Npts(int32_t Value);

    /*
    Array of doubles containing the P array for the Loadshape.
    */
    DSS_CAPI_V7_DLL void LoadShapes_Set_Pmult(double* ValuePtr, int32_t ValueCount);

    /*
    Array of doubles containing the Q multipliers.
    */
    DSS_CAPI_V7_DLL void LoadShapes_Set_Qmult(double* ValuePtr, int32_t ValueCount);

    DSS_CAPI_V7_DLL void LoadShapes_Normalize(void);

    /*
    Time array in hours correscponding to P and Q multipliers when the Interval=0.
    */
    DSS_CAPI_V7_DLL void LoadShapes_Get_TimeArray(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as LoadShapes_Get_TimeArray but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void LoadShapes_Get_TimeArray_GR(void);

    /*
    Time array in hours correscponding to P and Q multipliers when the Interval=0.
    */
    DSS_CAPI_V7_DLL void LoadShapes_Set_TimeArray(double* ValuePtr, int32_t ValueCount);

    /*
    Fixed interval time value, hours
    */
    DSS_CAPI_V7_DLL double LoadShapes_Get_HrInterval(void);

    /*
    Fixed Interval time value, in minutes
    */
    DSS_CAPI_V7_DLL double LoadShapes_Get_MinInterval(void);

    DSS_CAPI_V7_DLL double LoadShapes_Get_sInterval(void);

    /*
    Fixed interval time value, hours.
    */
    DSS_CAPI_V7_DLL void LoadShapes_Set_HrInterval(double Value);

    /*
    Fixed Interval time value, in minutes
    */
    DSS_CAPI_V7_DLL void LoadShapes_Set_MinInterval(double Value);

    /*
    Fixed interval data time interval, seconds
    */
    DSS_CAPI_V7_DLL void LoadShapes_Set_Sinterval(double Value);

    DSS_CAPI_V7_DLL int32_t LoadShapes_New(char* Name);

    DSS_CAPI_V7_DLL double LoadShapes_Get_PBase(void);

    /*
    Base for normalizing Q curve. If left at zero, the peak value is used.
    */
    DSS_CAPI_V7_DLL double LoadShapes_Get_Qbase(void);

    DSS_CAPI_V7_DLL void LoadShapes_Set_PBase(double Value);

    /*
    Base for normalizing Q curve. If left at zero, the peak value is used.
    */
    DSS_CAPI_V7_DLL void LoadShapes_Set_Qbase(double Value);

    /*
    T/F flag to let Loads know to use the actual value in the curve rather than use the value as a multiplier.
    */
    DSS_CAPI_V7_DLL uint16_t LoadShapes_Get_UseActual(void);

    /*
    T/F flag to let Loads know to use the actual value in the curve rather than use the value as a multiplier.
    */
    DSS_CAPI_V7_DLL void LoadShapes_Set_UseActual(uint16_t Value);

    /*
    Array of all energy Meter names
    */
    DSS_CAPI_V7_DLL void Meters_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as Meters_Get_AllNames but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Meters_Get_AllNames_GR(void);

    /*
    Set the first energy Meter active. Returns 0 if none.
    */
    DSS_CAPI_V7_DLL int32_t Meters_Get_First(void);

    /*
    Get/Set the active meter  name.
    */
    DSS_CAPI_V7_DLL char* Meters_Get_Name(void);

    /*
    Sets the next energy Meter active.  Returns 0 if no more.
    */
    DSS_CAPI_V7_DLL int32_t Meters_Get_Next(void);

    /*
    Array of strings containing the names of the registers.
    */
    DSS_CAPI_V7_DLL void Meters_Get_RegisterNames(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as Meters_Get_RegisterNames but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Meters_Get_RegisterNames_GR(void);

    /*
    Array of all the values contained in the Meter registers for the active Meter.
    */
    DSS_CAPI_V7_DLL void Meters_Get_RegisterValues(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Meters_Get_RegisterValues but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Meters_Get_RegisterValues_GR(void);

    DSS_CAPI_V7_DLL void Meters_Reset(void);

    DSS_CAPI_V7_DLL void Meters_ResetAll(void);

    DSS_CAPI_V7_DLL void Meters_Sample(void);

    DSS_CAPI_V7_DLL void Meters_Save(void);

    /*
    Set a meter to be active by name.
    */
    DSS_CAPI_V7_DLL void Meters_Set_Name(char* Value);

    /*
    Totals of all registers of all meters
    */
    DSS_CAPI_V7_DLL void Meters_Get_Totals(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Meters_Get_Totals but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Meters_Get_Totals_GR(void);

    /*
    Array of doubles to set values of Peak Current property
    */
    DSS_CAPI_V7_DLL void Meters_Get_Peakcurrent(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Meters_Get_Peakcurrent but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Meters_Get_Peakcurrent_GR(void);

    /*
    Array of doubles to set values of Peak Current property
    */
    DSS_CAPI_V7_DLL void Meters_Set_Peakcurrent(double* ValuePtr, int32_t ValueCount);

    /*
    Set the magnitude of the real part of the Calculated Current (normally determined by solution) for the Meter to force some behavior on Load Allocation
    */
    DSS_CAPI_V7_DLL void Meters_Get_CalcCurrent(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Meters_Get_CalcCurrent but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Meters_Get_CalcCurrent_GR(void);

    /*
    Set the magnitude of the real part of the Calculated Current (normally determined by solution) for the Meter to force some behavior on Load Allocation
    */
    DSS_CAPI_V7_DLL void Meters_Set_CalcCurrent(double* ValuePtr, int32_t ValueCount);

    /*
    Array of doubles: set the phase allocation factors for the active meter.
    */
    DSS_CAPI_V7_DLL void Meters_Get_AllocFactors(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Meters_Get_AllocFactors but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Meters_Get_AllocFactors_GR(void);

    /*
    Array of doubles: set the phase allocation factors for the active meter.
    */
    DSS_CAPI_V7_DLL void Meters_Set_AllocFactors(double* ValuePtr, int32_t ValueCount);

    /*
    Set Name of metered element
    */
    DSS_CAPI_V7_DLL char* Meters_Get_MeteredElement(void);

    /*
    set Number of Metered Terminal
    */
    DSS_CAPI_V7_DLL int32_t Meters_Get_MeteredTerminal(void);

    /*
    Set Name of metered element
    */
    DSS_CAPI_V7_DLL void Meters_Set_MeteredElement(char* Value);

    /*
    set Number of Metered Terminal
    */
    DSS_CAPI_V7_DLL void Meters_Set_MeteredTerminal(int32_t Value);

    /*
    Global Flag in the DSS to indicate if Demand Interval (DI) files have been properly opened.
    */
    DSS_CAPI_V7_DLL uint16_t Meters_Get_DIFilesAreOpen(void);

    DSS_CAPI_V7_DLL void Meters_CloseAllDIFiles(void);

    DSS_CAPI_V7_DLL void Meters_OpenAllDIFiles(void);

    DSS_CAPI_V7_DLL void Meters_SampleAll(void);

    DSS_CAPI_V7_DLL void Meters_SaveAll(void);

    /*
    Array of names of all zone end elements.
    */
    DSS_CAPI_V7_DLL void Meters_Get_AllEndElements(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as Meters_Get_AllEndElements but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Meters_Get_AllEndElements_GR(void);

    /*
    Number of zone end elements in the active meter zone.
    */
    DSS_CAPI_V7_DLL int32_t Meters_Get_CountEndElements(void);

    /*
    Number of Energy Meters in the Active Circuit
    */
    DSS_CAPI_V7_DLL int32_t Meters_Get_Count(void);

    /*
    Wide string list of all branches in zone of the active energymeter object.
    */
    DSS_CAPI_V7_DLL void Meters_Get_AllBranchesInZone(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as Meters_Get_AllBranchesInZone but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Meters_Get_AllBranchesInZone_GR(void);

    /*
    Number of branches in Active energymeter zone. (Same as sequencelist size)
    */
    DSS_CAPI_V7_DLL int32_t Meters_Get_CountBranches(void);

    /*
    Returns SAIFI for this meter's Zone. Execute Reliability Calc method first.
    */
    DSS_CAPI_V7_DLL double Meters_Get_SAIFI(void);

    /*
    Get/set Index into Meter's SequenceList that contains branch pointers in lexical order. Earlier index guaranteed to be upline from later index. Sets PDelement active.
    */
    DSS_CAPI_V7_DLL int32_t Meters_Get_SequenceIndex(void);

    /*
    Get/set Index into Meter's SequenceList that contains branch pointers in lexical order. Earlier index guaranteed to be upline from later index. Sets PDelement active.
    */
    DSS_CAPI_V7_DLL void Meters_Set_SequenceIndex(int32_t Value);

    /*
    SAIFI based on kW rather than number of customers. Get after reliability calcs.
    */
    DSS_CAPI_V7_DLL double Meters_Get_SAIFIKW(void);

    DSS_CAPI_V7_DLL void Meters_DoReliabilityCalc(uint16_t AssumeRestoration);

    /*
    Size of Sequence List
    */
    DSS_CAPI_V7_DLL int32_t Meters_Get_SeqListSize(void);

    /*
    Total Number of customers in this zone (downline from the EnergyMeter)
    */
    DSS_CAPI_V7_DLL int32_t Meters_Get_TotalCustomers(void);

    /*
    SAIDI for this meter's zone. Execute DoReliabilityCalc first.
    */
    DSS_CAPI_V7_DLL double Meters_Get_SAIDI(void);

    /*
    Total customer interruptions for this Meter zone based on reliability calcs.
    */
    DSS_CAPI_V7_DLL double Meters_Get_CustInterrupts(void);

    /*
    Number of feeder sections in this meter's zone
    */
    DSS_CAPI_V7_DLL int32_t Meters_Get_NumSections(void);

    DSS_CAPI_V7_DLL void Meters_SetActiveSection(int32_t SectIdx);

    /*
    Average Repair time in this section of the meter zone
    */
    DSS_CAPI_V7_DLL double Meters_Get_AvgRepairTime(void);

    /*
    Sum of Fault Rate time Repair Hrs in this section of the meter zone
    */
    DSS_CAPI_V7_DLL double Meters_Get_FaultRateXRepairHrs(void);

    /*
    Number of branches (lines) in this section
    */
    DSS_CAPI_V7_DLL int32_t Meters_Get_NumSectionBranches(void);

    /*
    Number of Customers in the active section.
    */
    DSS_CAPI_V7_DLL int32_t Meters_Get_NumSectionCustomers(void);

    /*
    Type of OCP device. 1=Fuse; 2=Recloser; 3=Relay
    */
    DSS_CAPI_V7_DLL int32_t Meters_Get_OCPDeviceType(void);

    /*
    Sum of the branch fault rates in this section of the meter's zone
    */
    DSS_CAPI_V7_DLL double Meters_Get_SumBranchFltRates(void);

    /*
    SequenceIndex of the branch at the head of this section
    */
    DSS_CAPI_V7_DLL int32_t Meters_Get_SectSeqIdx(void);

    /*
    Total Customers downline from this section
    */
    DSS_CAPI_V7_DLL int32_t Meters_Get_SectTotalCust(void);

    /*
    Array of all Monitor Names
    */
    DSS_CAPI_V7_DLL void Monitors_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as Monitors_Get_AllNames but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Monitors_Get_AllNames_GR(void);

    /*
    Name of CSV file associated with active Monitor.
    */
    DSS_CAPI_V7_DLL char* Monitors_Get_FileName(void);

    /*
    Sets the first Monitor active.  Returns 0 if no monitors.
    */
    DSS_CAPI_V7_DLL int32_t Monitors_Get_First(void);

    /*
    Set Monitor mode (bitmask integer - see DSS Help)
    */
    DSS_CAPI_V7_DLL int32_t Monitors_Get_Mode(void);

    /*
    Sets the active Monitor object by name
    */
    DSS_CAPI_V7_DLL char* Monitors_Get_Name(void);

    /*
    Sets next monitor active.  Returns 0 if no more.
    */
    DSS_CAPI_V7_DLL int32_t Monitors_Get_Next(void);

    DSS_CAPI_V7_DLL void Monitors_Reset(void);

    DSS_CAPI_V7_DLL void Monitors_ResetAll(void);

    DSS_CAPI_V7_DLL void Monitors_Sample(void);

    DSS_CAPI_V7_DLL void Monitors_Save(void);

    /*
    Set Monitor mode (bitmask integer - see DSS Help)
    */
    DSS_CAPI_V7_DLL void Monitors_Set_Mode(int32_t Value);

    DSS_CAPI_V7_DLL void Monitors_Show(void);

    /*
    Sets the active Monitor object by name
    */
    DSS_CAPI_V7_DLL void Monitors_Set_Name(char* Value);

    /*
    Byte Array containing monitor stream values. Make sure a "save" is done first (standard solution modes do this automatically)
    */
    DSS_CAPI_V7_DLL void Monitors_Get_ByteStream(int8_t** ResultPtr, int32_t* ResultCount);
    /*
    Same as Monitors_Get_ByteStream but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Monitors_Get_ByteStream_GR(void);

    /*
    Number of Samples in Monitor at Present
    */
    DSS_CAPI_V7_DLL int32_t Monitors_Get_SampleCount(void);

    DSS_CAPI_V7_DLL void Monitors_SampleAll(void);

    DSS_CAPI_V7_DLL void Monitors_SaveAll(void);

    /*
    Number of Monitors
    */
    DSS_CAPI_V7_DLL int32_t Monitors_Get_Count(void);

    DSS_CAPI_V7_DLL void Monitors_Process(void);

    DSS_CAPI_V7_DLL void Monitors_ProcessAll(void);

    /*
    Array of doubles for the specified channel  (usage: MyArray = DSSMonitor.Channel(i)) A Save or SaveAll  should be executed first. Done automatically by most standard solution modes.
    */
    DSS_CAPI_V7_DLL void Monitors_Get_Channel(double** ResultPtr, int32_t* ResultCount, int32_t Index);
    /*
    Same as Monitors_Get_Channel but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Monitors_Get_Channel_GR(int32_t Index);

    /*
    Array of doubles containing frequency values for harmonics mode solutions; Empty for time mode solutions (use dblHour)
    */
    DSS_CAPI_V7_DLL void Monitors_Get_dblFreq(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Monitors_Get_dblFreq but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Monitors_Get_dblFreq_GR(void);

    /*
    Array of doubles containgin time value in hours for time-sampled monitor values; Empty if frequency-sampled values for harmonics solution  (see dblFreq)
    */
    DSS_CAPI_V7_DLL void Monitors_Get_dblHour(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Monitors_Get_dblHour but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Monitors_Get_dblHour_GR(void);

    /*
    Monitor File Version (integer)
    */
    DSS_CAPI_V7_DLL int32_t Monitors_Get_FileVersion(void);

    /*
    Header string;  Array of strings containing Channel names
    */
    DSS_CAPI_V7_DLL void Monitors_Get_Header(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as Monitors_Get_Header but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Monitors_Get_Header_GR(void);

    /*
    Number of Channels in the active Monitor
    */
    DSS_CAPI_V7_DLL int32_t Monitors_Get_NumChannels(void);

    /*
    Size of each record in ByteStream (Integer). Same as NumChannels.
    */
    DSS_CAPI_V7_DLL int32_t Monitors_Get_RecordSize(void);

    /*
    Full object name of element being monitored.
    */
    DSS_CAPI_V7_DLL char* Monitors_Get_Element(void);

    /*
    Full object name of element being monitored.
    */
    DSS_CAPI_V7_DLL void Monitors_Set_Element(char* Value);

    /*
    Terminal number of element being monitored
    */
    DSS_CAPI_V7_DLL int32_t Monitors_Get_Terminal(void);

    /*
    Terminal number of element being monitored.
    */
    DSS_CAPI_V7_DLL void Monitors_Set_Terminal(int32_t Value);

    /*
    String to be parsed. Loading this string resets the Parser to the beginning of the line. Then parse off the tokens in sequence.
    */
    DSS_CAPI_V7_DLL char* Parser_Get_CmdString(void);

    /*
    String to be parsed. Loading this string resets the Parser to the beginning of the line. Then parse off the tokens in sequence.
    */
    DSS_CAPI_V7_DLL void Parser_Set_CmdString(char* Value);

    /*
    Get next token and return tag name (before = sign) if any. See AutoIncrement.
    */
    DSS_CAPI_V7_DLL char* Parser_Get_NextParam(void);

    /*
    Default is FALSE. If TRUE parser automatically advances to next token after DblValue, IntValue, or StrValue. Simpler when you don't need to check for parameter names.
    */
    DSS_CAPI_V7_DLL uint16_t Parser_Get_AutoIncrement(void);

    /*
    Default is FALSE. If TRUE parser automatically advances to next token after DblValue, IntValue, or StrValue. Simpler when you don't need to check for parameter names.
    */
    DSS_CAPI_V7_DLL void Parser_Set_AutoIncrement(uint16_t Value);

    /*
    Return next parameter as a double.
    */
    DSS_CAPI_V7_DLL double Parser_Get_DblValue(void);

    /*
    Return next parameter as a long integer.
    */
    DSS_CAPI_V7_DLL int32_t Parser_Get_IntValue(void);

    /*
    Return next parameter as a string
    */
    DSS_CAPI_V7_DLL char* Parser_Get_StrValue(void);

    /*
    Get the characters used for White space in the command string.  Default is blank and Tab.
    */
    DSS_CAPI_V7_DLL char* Parser_Get_WhiteSpace(void);

    /*
    Set the characters used for White space in the command string.  Default is blank and Tab.
    */
    DSS_CAPI_V7_DLL void Parser_Set_WhiteSpace(char* Value);

    /*
    Get String containing the the characters for Quoting in OpenDSS scripts. Matching pairs defined in EndQuote. Default is "'([{.
    */
    DSS_CAPI_V7_DLL char* Parser_Get_BeginQuote(void);

    /*
    String containing characters, in order, that match the beginning quote characters in BeginQuote. Default is "')]}
    */
    DSS_CAPI_V7_DLL char* Parser_Get_EndQuote(void);

    /*
    Set String containing the the characters for Quoting in OpenDSS scripts. Matching pairs defined in EndQuote. Default is "'([{.
    */
    DSS_CAPI_V7_DLL void Parser_Set_BeginQuote(char* Value);

    /*
    String containing characters, in order, that match the beginning quote characters in BeginQuote. Default is "')]}
    */
    DSS_CAPI_V7_DLL void Parser_Set_EndQuote(char* Value);

    /*
    String defining hard delimiters used to separate token on the command string. Default is , and =. The = separates token name from token value. These override whitesspace to separate tokens.
    */
    DSS_CAPI_V7_DLL char* Parser_Get_Delimiters(void);

    /*
    String defining hard delimiters used to separate token on the command string. Default is , and =. The = separates token name from token value. These override whitesspace to separate tokens.
    */
    DSS_CAPI_V7_DLL void Parser_Set_Delimiters(char* Value);

    DSS_CAPI_V7_DLL void Parser_ResetDelimiters(void);

    /*
    Returns token as array of doubles. For parsing quoted array syntax.
    */
    DSS_CAPI_V7_DLL void Parser_Get_Vector(double** ResultPtr, int32_t* ResultCount, int32_t ExpectedSize);
    /*
    Same as Parser_Get_Vector but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Parser_Get_Vector_GR(int32_t ExpectedSize);

    /*
    Use this property to parse a Matrix token in OpenDSS format.  Returns square matrix of order specified. Order same as default Fortran order: column by column.
    */
    DSS_CAPI_V7_DLL void Parser_Get_Matrix(double** ResultPtr, int32_t* ResultCount, int32_t ExpectedOrder);
    /*
    Same as Parser_Get_Matrix but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Parser_Get_Matrix_GR(int32_t ExpectedOrder);

    /*
    Use this property to parse a matrix token specified in lower triangle form. Symmetry is forced.
    */
    DSS_CAPI_V7_DLL void Parser_Get_SymMatrix(double** ResultPtr, int32_t* ResultCount, int32_t ExpectedOrder);
    /*
    Same as Parser_Get_SymMatrix but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Parser_Get_SymMatrix_GR(int32_t ExpectedOrder);

    /*
    Number of PD elements (including disabled elements)
    */
    DSS_CAPI_V7_DLL int32_t PDElements_Get_Count(void);

    /*
    Get/Set Number of failures per year. For LINE elements: Number of failures per unit length per year.
    */
    DSS_CAPI_V7_DLL double PDElements_Get_FaultRate(void);

    /*
    Set the first enabled PD element to be the active element.  Returns 0 if none found.
    */
    DSS_CAPI_V7_DLL int32_t PDElements_Get_First(void);

    /*
    Variant boolean indicating of PD element should be treated as a shunt element rather than a series element. Applies to Capacitor and Reactor elements in particular.
    */
    DSS_CAPI_V7_DLL uint16_t PDElements_Get_IsShunt(void);

    /*
    Advance to the next PD element in the circuit. Enabled elements only. Returns 0 when no more elements.
    */
    DSS_CAPI_V7_DLL int32_t PDElements_Get_Next(void);

    /*
    Get/Set percent of faults that are permanent (require repair). Otherwise, fault is assumed to be transient/temporary.
    */
    DSS_CAPI_V7_DLL double PDElements_Get_pctPermanent(void);

    DSS_CAPI_V7_DLL void PDElements_Set_FaultRate(double Value);

    DSS_CAPI_V7_DLL void PDElements_Set_pctPermanent(double Value);

    /*
    Get/Set name of active PD Element. Returns null string if active element is not PDElement type.
    */
    DSS_CAPI_V7_DLL char* PDElements_Get_Name(void);

    DSS_CAPI_V7_DLL void PDElements_Set_Name(char* Value);

    /*
    accummulated failure rate for this branch on downline
    */
    DSS_CAPI_V7_DLL double PDElements_Get_AccumulatedL(void);

    /*
    Failure rate for this branch. Faults per year including length of line.
    */
    DSS_CAPI_V7_DLL double PDElements_Get_Lambda(void);

    /*
    Number of customers, this branch
    */
    DSS_CAPI_V7_DLL int32_t PDElements_Get_Numcustomers(void);

    /*
    Sets the parent PD element to be the active circuit element.  Returns 0 if no more elements upline.
    */
    DSS_CAPI_V7_DLL int32_t PDElements_Get_ParentPDElement(void);

    /*
    Average repair time for this element in hours
    */
    DSS_CAPI_V7_DLL double PDElements_Get_RepairTime(void);

    /*
    Total number of customers from this branch to the end of the zone
    */
    DSS_CAPI_V7_DLL int32_t PDElements_Get_Totalcustomers(void);

    /*
    Number of the terminal of active PD element that is on the "from" side. This is set after the meter zone is determined.
    */
    DSS_CAPI_V7_DLL int32_t PDElements_Get_FromTerminal(void);

    /*
    Total miles of line from this element to the end of the zone. For recloser siting algorithm.
    */
    DSS_CAPI_V7_DLL double PDElements_Get_TotalMiles(void);

    /*
    Integer ID of the feeder section that this PDElement branch is part of
    */
    DSS_CAPI_V7_DLL int32_t PDElements_Get_SectionID(void);

    /*
    Average repair time for this element in hours
    */
    DSS_CAPI_V7_DLL void PDElements_Set_RepairTime(double Value);

    /*
    Vairant array of strings with all PVSystem names
    */
    DSS_CAPI_V7_DLL void PVSystems_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as PVSystems_Get_AllNames but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void PVSystems_Get_AllNames_GR(void);

    /*
    Variant Array of PVSYSTEM energy meter register names
    */
    DSS_CAPI_V7_DLL void PVSystems_Get_RegisterNames(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as PVSystems_Get_RegisterNames but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void PVSystems_Get_RegisterNames_GR(void);

    /*
    Array of doubles containing values in PVSystem registers.
    */
    DSS_CAPI_V7_DLL void PVSystems_Get_RegisterValues(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as PVSystems_Get_RegisterValues but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void PVSystems_Get_RegisterValues_GR(void);

    /*
    Set first PVSystem active; returns 0 if none.
    */
    DSS_CAPI_V7_DLL int32_t PVSystems_Get_First(void);

    /*
    Sets next PVSystem active; returns 0 if no more.
    */
    DSS_CAPI_V7_DLL int32_t PVSystems_Get_Next(void);

    /*
    Number of PVSystems
    */
    DSS_CAPI_V7_DLL int32_t PVSystems_Get_Count(void);

    /*
    Get/set active PVSystem by index;  1..Count
    */
    DSS_CAPI_V7_DLL int32_t PVSystems_Get_idx(void);

    /*
    Get/Set Active PVSystem by index:  1.. Count
    */
    DSS_CAPI_V7_DLL void PVSystems_Set_idx(int32_t Value);

    /*
    Get the name of the active PVSystem
    */
    DSS_CAPI_V7_DLL char* PVSystems_Get_Name(void);

    /*
    Set the name of the active PVSystem
    */
    DSS_CAPI_V7_DLL void PVSystems_Set_Name(char* Value);

    /*
    Get the present value of the Irradiance property in W/sq-m
    */
    DSS_CAPI_V7_DLL double PVSystems_Get_Irradiance(void);

    /*
    Set the present Irradiance value in W/sq-m
    */
    DSS_CAPI_V7_DLL void PVSystems_Set_Irradiance(double Value);

    /*
    Get kvar value
    */
    DSS_CAPI_V7_DLL double PVSystems_Get_kvar(void);

    /*
    Get Rated kVA of the PVSystem
    */
    DSS_CAPI_V7_DLL double PVSystems_Get_kVArated(void);

    /*
    get kW output
    */
    DSS_CAPI_V7_DLL double PVSystems_Get_kW(void);

    /*
    Get Power factor
    */
    DSS_CAPI_V7_DLL double PVSystems_Get_PF(void);

    /*
    Set kva rated
    */
    DSS_CAPI_V7_DLL void PVSystems_Set_kVArated(double Value);

    /*
    Set PF
    */
    DSS_CAPI_V7_DLL void PVSystems_Set_PF(double Value);

    /*
    Set kvar output value
    */
    DSS_CAPI_V7_DLL void PVSystems_Set_kvar(double Value);

    /*
    Array of strings with names of all Reclosers in Active Circuit
    */
    DSS_CAPI_V7_DLL void Reclosers_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as Reclosers_Get_AllNames but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Reclosers_Get_AllNames_GR(void);

    /*
    Number of Reclosers in active circuit.
    */
    DSS_CAPI_V7_DLL int32_t Reclosers_Get_Count(void);

    /*
    Set First Recloser to be Active Ckt Element. Returns 0 if none.
    */
    DSS_CAPI_V7_DLL int32_t Reclosers_Get_First(void);

    /*
    Get Name of active Recloser or set the active Recloser by name.
    */
    DSS_CAPI_V7_DLL char* Reclosers_Get_Name(void);

    /*
    Iterate to the next recloser in the circuit. Returns zero if no more.
    */
    DSS_CAPI_V7_DLL int32_t Reclosers_Get_Next(void);

    DSS_CAPI_V7_DLL void Reclosers_Set_Name(char* Value);

    /*
    Terminal number of Monitored object for the Recloser
    */
    DSS_CAPI_V7_DLL int32_t Reclosers_Get_MonitoredTerm(void);

    DSS_CAPI_V7_DLL void Reclosers_Set_MonitoredTerm(int32_t Value);

    /*
    Full name of the circuit element that is being switched by the Recloser.
    */
    DSS_CAPI_V7_DLL char* Reclosers_Get_SwitchedObj(void);

    DSS_CAPI_V7_DLL void Reclosers_Set_SwitchedObj(char* Value);

    /*
    Full name of object this Recloser is monitoring.
    */
    DSS_CAPI_V7_DLL char* Reclosers_Get_MonitoredObj(void);

    /*
    Terminal number of the controlled device being switched by the Recloser
    */
    DSS_CAPI_V7_DLL int32_t Reclosers_Get_SwitchedTerm(void);

    /*
    Set monitored object by full name.
    */
    DSS_CAPI_V7_DLL void Reclosers_Set_MonitoredObj(char* Value);

    DSS_CAPI_V7_DLL void Reclosers_Set_SwitchedTerm(int32_t Value);

    /*
    Number of fast shots
    */
    DSS_CAPI_V7_DLL int32_t Reclosers_Get_NumFast(void);

    /*
    Variant Array of Doubles: reclose intervals, s, between shots.
    */
    DSS_CAPI_V7_DLL void Reclosers_Get_RecloseIntervals(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Reclosers_Get_RecloseIntervals but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Reclosers_Get_RecloseIntervals_GR(void);

    /*
    Number of shots to lockout (fast + delayed)
    */
    DSS_CAPI_V7_DLL int32_t Reclosers_Get_Shots(void);

    DSS_CAPI_V7_DLL void Reclosers_Set_NumFast(int32_t Value);

    DSS_CAPI_V7_DLL void Reclosers_Set_Shots(int32_t Value);

    /*
    Phase trip curve multiplier or actual amps
    */
    DSS_CAPI_V7_DLL double Reclosers_Get_PhaseTrip(void);

    /*
    Phase Trip multiplier or actual amps
    */
    DSS_CAPI_V7_DLL void Reclosers_Set_PhaseTrip(double Value);

    /*
    Ground (3I0) instantaneous trip setting - curve multipler or actual amps.
    */
    DSS_CAPI_V7_DLL double Reclosers_Get_GroundInst(void);

    /*
    Ground (3I0) trip multiplier or actual amps
    */
    DSS_CAPI_V7_DLL double Reclosers_Get_GroundTrip(void);

    /*
    Phase instantaneous curve multipler or actual amps
    */
    DSS_CAPI_V7_DLL double Reclosers_Get_PhaseInst(void);

    /*
    Ground (3I0) trip instantaneous multiplier or actual amps
    */
    DSS_CAPI_V7_DLL void Reclosers_Set_GroundInst(double Value);

    DSS_CAPI_V7_DLL void Reclosers_Set_GroundTrip(double Value);

    DSS_CAPI_V7_DLL void Reclosers_Set_PhaseInst(double Value);

    DSS_CAPI_V7_DLL void Reclosers_Close(void);

    DSS_CAPI_V7_DLL void Reclosers_Open(void);

    /*
    Get/Set the active Recloser by index into the recloser list.  1..Count
    */
    DSS_CAPI_V7_DLL int32_t Reclosers_Get_idx(void);

    /*
    Get/Set the Active Recloser by index into the recloser list. 1..Count
    */
    DSS_CAPI_V7_DLL void Reclosers_Set_idx(int32_t Value);

    /*
    Array of strings containing all RegControl names
    */
    DSS_CAPI_V7_DLL void RegControls_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as RegControls_Get_AllNames but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void RegControls_Get_AllNames_GR(void);

    /*
    CT primary ampere rating (secondary is 0.2 amperes)
    */
    DSS_CAPI_V7_DLL double RegControls_Get_CTPrimary(void);

    /*
    Time delay [s] after arming before the first tap change. Control may reset before actually changing taps.
    */
    DSS_CAPI_V7_DLL double RegControls_Get_Delay(void);

    /*
    Sets the first RegControl active. Returns 0 if none.
    */
    DSS_CAPI_V7_DLL int32_t RegControls_Get_First(void);

    /*
    Regulation bandwidth in forward direciton, centered on Vreg
    */
    DSS_CAPI_V7_DLL double RegControls_Get_ForwardBand(void);

    /*
    LDC R setting in Volts
    */
    DSS_CAPI_V7_DLL double RegControls_Get_ForwardR(void);

    /*
    Target voltage in the forward direction, on PT secondary base.
    */
    DSS_CAPI_V7_DLL double RegControls_Get_ForwardVreg(void);

    /*
    LDC X setting in Volts
    */
    DSS_CAPI_V7_DLL double RegControls_Get_ForwardX(void);

    /*
    Time delay is inversely adjsuted, proportinal to the amount of voltage outside the regulating band.
    */
    DSS_CAPI_V7_DLL uint16_t RegControls_Get_IsInverseTime(void);

    /*
    Regulator can use different settings in the reverse direction.  Usually not applicable to substation transformers.
    */
    DSS_CAPI_V7_DLL uint16_t RegControls_Get_IsReversible(void);

    /*
    Maximum tap change per iteration in STATIC solution mode. 1 is more realistic, 16 is the default for a faster soluiton.
    */
    DSS_CAPI_V7_DLL int32_t RegControls_Get_MaxTapChange(void);

    /*
    Name of a remote regulated bus, in lieu of LDC settings
    */
    DSS_CAPI_V7_DLL char* RegControls_Get_MonitoredBus(void);

    /*
    Get/set Active RegControl  name
    */
    DSS_CAPI_V7_DLL char* RegControls_Get_Name(void);

    /*
    Sets the next RegControl active. Returns 0 if none.
    */
    DSS_CAPI_V7_DLL int32_t RegControls_Get_Next(void);

    /*
    PT ratio for voltage control settings
    */
    DSS_CAPI_V7_DLL double RegControls_Get_PTratio(void);

    /*
    Bandwidth in reverse direction, centered on reverse Vreg.
    */
    DSS_CAPI_V7_DLL double RegControls_Get_ReverseBand(void);

    /*
    Reverse LDC R setting in Volts.
    */
    DSS_CAPI_V7_DLL double RegControls_Get_ReverseR(void);

    /*
    Target voltage in the revese direction, on PT secondary base.
    */
    DSS_CAPI_V7_DLL double RegControls_Get_ReverseVreg(void);

    /*
    Reverse LDC X setting in volts.
    */
    DSS_CAPI_V7_DLL double RegControls_Get_ReverseX(void);

    /*
    Time delay [s] for subsequent tap changes in a set. Control may reset before actually changing taps.
    */
    DSS_CAPI_V7_DLL double RegControls_Get_TapDelay(void);

    /*
    Tapped winding number
    */
    DSS_CAPI_V7_DLL int32_t RegControls_Get_TapWinding(void);

    /*
    Name of the transformer this regulator controls
    */
    DSS_CAPI_V7_DLL char* RegControls_Get_Transformer(void);

    /*
    First house voltage limit on PT secondary base.  Setting to 0 disables this function.
    */
    DSS_CAPI_V7_DLL double RegControls_Get_VoltageLimit(void);

    /*
    Winding number for PT and CT connections
    */
    DSS_CAPI_V7_DLL int32_t RegControls_Get_Winding(void);

    DSS_CAPI_V7_DLL int32_t RegControls_Get_TapNumber(void);

    /*
    CT primary ampere rating (secondary is 0.2 amperes)
    */
    DSS_CAPI_V7_DLL void RegControls_Set_CTPrimary(double Value);

    /*
    Time delay [s] after arming before the first tap change. Control may reset before actually changing taps.
    */
    DSS_CAPI_V7_DLL void RegControls_Set_Delay(double Value);

    /*
    Regulation bandwidth in forward direciton, centered on Vreg
    */
    DSS_CAPI_V7_DLL void RegControls_Set_ForwardBand(double Value);

    /*
    LDC R setting in Volts
    */
    DSS_CAPI_V7_DLL void RegControls_Set_ForwardR(double Value);

    /*
    Target voltage in the forward direction, on PT secondary base.
    */
    DSS_CAPI_V7_DLL void RegControls_Set_ForwardVreg(double Value);

    /*
    LDC X setting in Volts
    */
    DSS_CAPI_V7_DLL void RegControls_Set_ForwardX(double Value);

    /*
    Time delay is inversely adjsuted, proportinal to the amount of voltage outside the regulating band.
    */
    DSS_CAPI_V7_DLL void RegControls_Set_IsInverseTime(uint16_t Value);

    /*
    Regulator can use different settings in the reverse direction.  Usually not applicable to substation transformers.
    */
    DSS_CAPI_V7_DLL void RegControls_Set_IsReversible(uint16_t Value);

    /*
    Maximum tap change per iteration in STATIC solution mode. 1 is more realistic, 16 is the default for a faster soluiton.
    */
    DSS_CAPI_V7_DLL void RegControls_Set_MaxTapChange(int32_t Value);

    /*
    Name of a remote regulated bus, in lieu of LDC settings
    */
    DSS_CAPI_V7_DLL void RegControls_Set_MonitoredBus(char* Value);

    /*
    Sets a RegControl active by name
    */
    DSS_CAPI_V7_DLL void RegControls_Set_Name(char* Value);

    /*
    PT ratio for voltage control settings
    */
    DSS_CAPI_V7_DLL void RegControls_Set_PTratio(double Value);

    /*
    Bandwidth in reverse direction, centered on reverse Vreg.
    */
    DSS_CAPI_V7_DLL void RegControls_Set_ReverseBand(double Value);

    /*
    Reverse LDC R setting in Volts.
    */
    DSS_CAPI_V7_DLL void RegControls_Set_ReverseR(double Value);

    /*
    Target voltage in the revese direction, on PT secondary base.
    */
    DSS_CAPI_V7_DLL void RegControls_Set_ReverseVreg(double Value);

    /*
    Reverse LDC X setting in volts.
    */
    DSS_CAPI_V7_DLL void RegControls_Set_ReverseX(double Value);

    /*
    Time delay [s] for subsequent tap changes in a set. Control may reset before actually changing taps.
    */
    DSS_CAPI_V7_DLL void RegControls_Set_TapDelay(double Value);

    /*
    Tapped winding number
    */
    DSS_CAPI_V7_DLL void RegControls_Set_TapWinding(int32_t Value);

    /*
    Name of the transformer this regulator controls
    */
    DSS_CAPI_V7_DLL void RegControls_Set_Transformer(char* Value);

    /*
    First house voltage limit on PT secondary base.  Setting to 0 disables this function.
    */
    DSS_CAPI_V7_DLL void RegControls_Set_VoltageLimit(double Value);

    /*
    Winding number for PT and CT connections
    */
    DSS_CAPI_V7_DLL void RegControls_Set_Winding(int32_t Value);

    /*
    Integer number of the tap that the controlled transformer winding is currentliy on.
    */
    DSS_CAPI_V7_DLL void RegControls_Set_TapNumber(int32_t Value);

    /*
    Number of RegControl objects in Active Circuit
    */
    DSS_CAPI_V7_DLL int32_t RegControls_Get_Count(void);

    DSS_CAPI_V7_DLL void RegControls_Reset(void);

    /*
    Array of strings containing names of all Relay elements
    */
    DSS_CAPI_V7_DLL void Relays_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as Relays_Get_AllNames but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Relays_Get_AllNames_GR(void);

    /*
    Number of Relays in circuit
    */
    DSS_CAPI_V7_DLL int32_t Relays_Get_Count(void);

    /*
    Set First Relay active. If none, returns 0.
    */
    DSS_CAPI_V7_DLL int32_t Relays_Get_First(void);

    /*
    Get name of active relay.
    */
    DSS_CAPI_V7_DLL char* Relays_Get_Name(void);

    /*
    Advance to next Relay object. Returns 0 when no more relays.
    */
    DSS_CAPI_V7_DLL int32_t Relays_Get_Next(void);

    /*
    Set Relay active by name
    */
    DSS_CAPI_V7_DLL void Relays_Set_Name(char* Value);

    /*
    Full name of object this Relay is monitoring.
    */
    DSS_CAPI_V7_DLL char* Relays_Get_MonitoredObj(void);

    DSS_CAPI_V7_DLL void Relays_Set_MonitoredObj(char* Value);

    /*
    Number of terminal of monitored element that this Relay is monitoring.
    */
    DSS_CAPI_V7_DLL int32_t Relays_Get_MonitoredTerm(void);

    /*
    Full name of element that will be switched when relay trips.
    */
    DSS_CAPI_V7_DLL char* Relays_Get_SwitchedObj(void);

    DSS_CAPI_V7_DLL void Relays_Set_MonitoredTerm(int32_t Value);

    DSS_CAPI_V7_DLL void Relays_Set_SwitchedObj(char* Value);

    DSS_CAPI_V7_DLL int32_t Relays_Get_SwitchedTerm(void);

    /*
    Terminal number of the switched object that will be opened when the relay trips.
    */
    DSS_CAPI_V7_DLL void Relays_Set_SwitchedTerm(int32_t Value);

    /*
    Get/Set active Relay by index into the Relay list. 1..Count
    */
    DSS_CAPI_V7_DLL int32_t Relays_Get_idx(void);

    /*
    Get/Set Relay active by index into relay list. 1..Count
    */
    DSS_CAPI_V7_DLL void Relays_Set_idx(int32_t Value);

    /*
    Array of Sensor names.
    */
    DSS_CAPI_V7_DLL void Sensors_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as Sensors_Get_AllNames but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Sensors_Get_AllNames_GR(void);

    /*
    Number of Sensors in Active Circuit.
    */
    DSS_CAPI_V7_DLL int32_t Sensors_Get_Count(void);

    /*
    Array of doubles for the line current measurements; don't use with kWS and kVARS.
    */
    DSS_CAPI_V7_DLL void Sensors_Get_Currents(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Sensors_Get_Currents but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Sensors_Get_Currents_GR(void);

    /*
    Sets the first sensor active. Returns 0 if none.
    */
    DSS_CAPI_V7_DLL int32_t Sensors_Get_First(void);

    /*
    True if measured voltages are line-line. Currents are always line currents.
    */
    DSS_CAPI_V7_DLL uint16_t Sensors_Get_IsDelta(void);

    /*
    Array of doubles for Q measurements. Overwrites Currents with a new estimate using kWS.
    */
    DSS_CAPI_V7_DLL void Sensors_Get_kVARS(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Sensors_Get_kVARS but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Sensors_Get_kVARS_GR(void);

    /*
    Array of doubles for the LL or LN (depending on Delta connection) voltage measurements.
    */
    DSS_CAPI_V7_DLL void Sensors_Get_kVS(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Sensors_Get_kVS but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Sensors_Get_kVS_GR(void);

    /*
    Array of doubles for P measurements. Overwrites Currents with a new estimate using kVARS.
    */
    DSS_CAPI_V7_DLL void Sensors_Get_kWS(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Sensors_Get_kWS but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Sensors_Get_kWS_GR(void);

    /*
    Full Name of the measured element
    */
    DSS_CAPI_V7_DLL char* Sensors_Get_MeteredElement(void);

    /*
    Number of the measured terminal in the measured element.
    */
    DSS_CAPI_V7_DLL int32_t Sensors_Get_MeteredTerminal(void);

    /*
    Name of the active sensor.
    */
    DSS_CAPI_V7_DLL char* Sensors_Get_Name(void);

    /*
    Sets the next Sensor active. Returns 0 if no more.
    */
    DSS_CAPI_V7_DLL int32_t Sensors_Get_Next(void);

    /*
    Assumed percent error in the Sensor measurement. Default is 1.
    */
    DSS_CAPI_V7_DLL double Sensors_Get_PctError(void);

    /*
    True if voltage measurements are 1-3, 3-2, 2-1.
    */
    DSS_CAPI_V7_DLL uint16_t Sensors_Get_ReverseDelta(void);

    /*
    Weighting factor for this Sensor measurement with respect to other Sensors. Default is 1.
    */
    DSS_CAPI_V7_DLL double Sensors_Get_Weight(void);

    DSS_CAPI_V7_DLL void Sensors_Reset(void);

    DSS_CAPI_V7_DLL void Sensors_ResetAll(void);

    DSS_CAPI_V7_DLL void Sensors_Set_Currents(double* ValuePtr, int32_t ValueCount);

    DSS_CAPI_V7_DLL void Sensors_Set_IsDelta(uint16_t Value);

    DSS_CAPI_V7_DLL void Sensors_Set_kVARS(double* ValuePtr, int32_t ValueCount);

    DSS_CAPI_V7_DLL void Sensors_Set_kVS(double* ValuePtr, int32_t ValueCount);

    DSS_CAPI_V7_DLL void Sensors_Set_kWS(double* ValuePtr, int32_t ValueCount);

    DSS_CAPI_V7_DLL void Sensors_Set_MeteredElement(char* Value);

    DSS_CAPI_V7_DLL void Sensors_Set_MeteredTerminal(int32_t Value);

    /*
    Set the active Sensor by name.
    */
    DSS_CAPI_V7_DLL void Sensors_Set_Name(char* Value);

    DSS_CAPI_V7_DLL void Sensors_Set_PctError(double Value);

    DSS_CAPI_V7_DLL void Sensors_Set_ReverseDelta(uint16_t Value);

    DSS_CAPI_V7_DLL void Sensors_Set_Weight(double Value);

    /*
    Voltage base for the sensor measurements. LL for 2 and 3-phase sensors, LN for 1-phase sensors.
    */
    DSS_CAPI_V7_DLL double Sensors_Get_kVbase(void);

    DSS_CAPI_V7_DLL void Sensors_Set_kVbase(double Value);

    /*
    {True | False*} Designates whether to allow duplicate names of objects
    */
    DSS_CAPI_V7_DLL uint16_t Settings_Get_AllowDuplicates(void);

    /*
    List of Buses or (File=xxxx) syntax for the AutoAdd solution mode.
    */
    DSS_CAPI_V7_DLL char* Settings_Get_AutoBusList(void);

    /*
    {dssMultiphase * | dssPositiveSeq} IIndicate if the circuit model is positive sequence.
    */
    DSS_CAPI_V7_DLL int32_t Settings_Get_CktModel(void);

    /*
    Per Unit maximum voltage for Emergency conditions.
    */
    DSS_CAPI_V7_DLL double Settings_Get_EmergVmaxpu(void);

    /*
    Per Unit minimum voltage for Emergency conditions.
    */
    DSS_CAPI_V7_DLL double Settings_Get_EmergVminpu(void);

    /*
    Per Unit maximum voltage for Normal conditions.
    */
    DSS_CAPI_V7_DLL double Settings_Get_NormVmaxpu(void);

    /*
    Per Unit minimum voltage for Normal conditions.
    */
    DSS_CAPI_V7_DLL double Settings_Get_NormVminpu(void);

    /*
    {True | False*}  Locks Zones on energy meters to prevent rebuilding if a circuit change occurs.
    */
    DSS_CAPI_V7_DLL uint16_t Settings_Get_ZoneLock(void);

    /*
    Sets all load allocation factors for all loads defined by XFKVA property to this value.
    */
    DSS_CAPI_V7_DLL void Settings_Set_AllocationFactors(double Value);

    /*
    {True | False*} Designates whether to allow duplicate names of objects
    */
    DSS_CAPI_V7_DLL void Settings_Set_AllowDuplicates(uint16_t Value);

    /*
    List of Buses or (File=xxxx) syntax for the AutoAdd solution mode.
    */
    DSS_CAPI_V7_DLL void Settings_Set_AutoBusList(char* Value);

    /*
    {dssMultiphase * | dssPositiveSeq} IIndicate if the circuit model is positive sequence.
    */
    DSS_CAPI_V7_DLL void Settings_Set_CktModel(int32_t Value);

    /*
    Per Unit maximum voltage for Emergency conditions.
    */
    DSS_CAPI_V7_DLL void Settings_Set_EmergVmaxpu(double Value);

    /*
    Per Unit minimum voltage for Emergency conditions.
    */
    DSS_CAPI_V7_DLL void Settings_Set_EmergVminpu(double Value);

    /*
    Per Unit maximum voltage for Normal conditions.
    */
    DSS_CAPI_V7_DLL void Settings_Set_NormVmaxpu(double Value);

    /*
    Per Unit minimum voltage for Normal conditions.
    */
    DSS_CAPI_V7_DLL void Settings_Set_NormVminpu(double Value);

    /*
    {True | False*}  Locks Zones on energy meters to prevent rebuilding if a circuit change occurs.
    */
    DSS_CAPI_V7_DLL void Settings_Set_ZoneLock(uint16_t Value);

    /*
    Integer array defining which energy meter registers to use for computing losses
    */
    DSS_CAPI_V7_DLL void Settings_Get_LossRegs(int32_t** ResultPtr, int32_t* ResultCount);
    /*
    Same as Settings_Get_LossRegs but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Settings_Get_LossRegs_GR(void);

    /*
    Weighting factor applied to Loss register values.
    */
    DSS_CAPI_V7_DLL double Settings_Get_LossWeight(void);

    /*
    {True | False *} Gets value of trapezoidal integration flag in energy meters.
    */
    DSS_CAPI_V7_DLL uint16_t Settings_Get_Trapezoidal(void);

    /*
    Array of Integers defining energy meter registers to use for computing UE
    */
    DSS_CAPI_V7_DLL void Settings_Get_UEregs(int32_t** ResultPtr, int32_t* ResultCount);
    /*
    Same as Settings_Get_UEregs but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Settings_Get_UEregs_GR(void);

    /*
    Weighting factor applied to UE register values.
    */
    DSS_CAPI_V7_DLL double Settings_Get_UEweight(void);

    /*
    Integer array defining which energy meter registers to use for computing losses
    */
    DSS_CAPI_V7_DLL void Settings_Set_LossRegs(int32_t* ValuePtr, int32_t ValueCount);

    /*
    Weighting factor applied to Loss register values.
    */
    DSS_CAPI_V7_DLL void Settings_Set_LossWeight(double Value);

    /*
    {True | False *} Gets value of trapezoidal integration flag in energy meters.
    */
    DSS_CAPI_V7_DLL void Settings_Set_Trapezoidal(uint16_t Value);

    /*
    Array of Integers defining energy meter registers to use for computing UE
    */
    DSS_CAPI_V7_DLL void Settings_Set_UEregs(int32_t* ValuePtr, int32_t ValueCount);

    /*
    Weighting factor applied to UE register values.
    */
    DSS_CAPI_V7_DLL void Settings_Set_UEweight(double Value);

    /*
    {True | False*} Denotes whether to trace the control actions to a file.
    */
    DSS_CAPI_V7_DLL uint16_t Settings_Get_ControlTrace(void);

    /*
    Array of doubles defining the legal voltage bases in kV L-L
    */
    DSS_CAPI_V7_DLL void Settings_Get_VoltageBases(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as Settings_Get_VoltageBases but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Settings_Get_VoltageBases_GR(void);

    /*
    {True | False*} Denotes whether to trace the control actions to a file.
    */
    DSS_CAPI_V7_DLL void Settings_Set_ControlTrace(uint16_t Value);

    /*
    Array of doubles defining the legal voltage bases in kV L-L
    */
    DSS_CAPI_V7_DLL void Settings_Set_VoltageBases(double* ValuePtr, int32_t ValueCount);

    /*
    Name of LoadShape object that serves as the source of price signal data for yearly simulations, etc.
    */
    DSS_CAPI_V7_DLL char* Settings_Get_PriceCurve(void);

    /*
    Price Signal for the Circuit
    */
    DSS_CAPI_V7_DLL double Settings_Get_PriceSignal(void);

    /*
    Name of LoadShape object that serves as the source of price signal data for yearly simulations, etc.
    */
    DSS_CAPI_V7_DLL void Settings_Set_PriceCurve(char* Value);

    /*
    Price Signal for the Circuit
    */
    DSS_CAPI_V7_DLL void Settings_Set_PriceSignal(double Value);

    /*
    Set the Frequency for next solution
    */
    DSS_CAPI_V7_DLL double Solution_Get_Frequency(void);

    /*
    Set Hour for time series solutions.
    */
    DSS_CAPI_V7_DLL int32_t Solution_Get_Hour(void);

    /*
    Number of iterations taken for last solution. (Same as TotalIterations)
    */
    DSS_CAPI_V7_DLL int32_t Solution_Get_Iterations(void);

    /*
    Default load multiplier applied to all non-fixed loads
    */
    DSS_CAPI_V7_DLL double Solution_Get_LoadMult(void);

    /*
    Max allowable iterations.
    */
    DSS_CAPI_V7_DLL int32_t Solution_Get_MaxIterations(void);

    /*
    Set present solution mode (by a text code - see DSS Help)
    */
    DSS_CAPI_V7_DLL int32_t Solution_Get_Mode(void);

    /*
    Number of solutions to perform for Monte Carlo and time series simulations
    */
    DSS_CAPI_V7_DLL int32_t Solution_Get_Number(void);

    /*
    Randomization mode for random variables "Gaussian" or "Uniform"
    */
    DSS_CAPI_V7_DLL int32_t Solution_Get_Random(void);

    /*
    Seconds from top of the hour.
    */
    DSS_CAPI_V7_DLL double Solution_Get_Seconds(void);

    /*
    Time step size in sec
    */
    DSS_CAPI_V7_DLL double Solution_Get_StepSize(void);

    /*
    Solution convergence tolerance.
    */
    DSS_CAPI_V7_DLL double Solution_Get_Tolerance(void);

    /*
    Set year for planning studies
    */
    DSS_CAPI_V7_DLL int32_t Solution_Get_Year(void);

    /*
    Set the Frequency for next solution
    */
    DSS_CAPI_V7_DLL void Solution_Set_Frequency(double Value);

    /*
    Set Hour for time series solutions.
    */
    DSS_CAPI_V7_DLL void Solution_Set_Hour(int32_t Value);

    /*
    Default load multiplier applied to all non-fixed loads
    */
    DSS_CAPI_V7_DLL void Solution_Set_LoadMult(double Value);

    /*
    Max allowable iterations.
    */
    DSS_CAPI_V7_DLL void Solution_Set_MaxIterations(int32_t Value);

    /*
    Set present solution mode (by a text code - see DSS Help)
    */
    DSS_CAPI_V7_DLL void Solution_Set_Mode(int32_t Mode);

    /*
    Number of solutions to perform for Monte Carlo and time series simulations
    */
    DSS_CAPI_V7_DLL void Solution_Set_Number(int32_t Value);

    /*
    Randomization mode for random variables "Gaussian" or "Uniform"
    */
    DSS_CAPI_V7_DLL void Solution_Set_Random(int32_t Random);

    /*
    Seconds from top of the hour.
    */
    DSS_CAPI_V7_DLL void Solution_Set_Seconds(double Value);

    /*
    Time step size in sec
    */
    DSS_CAPI_V7_DLL void Solution_Set_StepSize(double Value);

    /*
    Solution convergence tolerance.
    */
    DSS_CAPI_V7_DLL void Solution_Set_Tolerance(double Value);

    /*
    Set year for planning studies
    */
    DSS_CAPI_V7_DLL void Solution_Set_Year(int32_t Value);

    DSS_CAPI_V7_DLL void Solution_Solve(void);

    /*
    ID (text) of the present solution mode
    */
    DSS_CAPI_V7_DLL char* Solution_Get_ModeID(void);

    /*
    Load Model: {dssPowerFlow (default) | dssAdmittance}
    */
    DSS_CAPI_V7_DLL int32_t Solution_Get_LoadModel(void);

    /*
    Load Model: {dssPowerFlow (default) | dssAdmittance}
    */
    DSS_CAPI_V7_DLL void Solution_Set_LoadModel(int32_t Value);

    /*
    Load-Duration Curve name for LD modes
    */
    DSS_CAPI_V7_DLL char* Solution_Get_LDCurve(void);

    /*
    Load-Duration Curve name for LD modes
    */
    DSS_CAPI_V7_DLL void Solution_Set_LDCurve(char* Value);

    /*
    Percent default  annual load growth rate
    */
    DSS_CAPI_V7_DLL double Solution_Get_pctGrowth(void);

    /*
    Percent default  annual load growth rate
    */
    DSS_CAPI_V7_DLL void Solution_Set_pctGrowth(double Value);

    /*
    Type of device to add in AutoAdd Mode: {dssGen (Default) | dssCap}
    */
    DSS_CAPI_V7_DLL int32_t Solution_Get_AddType(void);

    /*
    Type of device to add in AutoAdd Mode: {dssGen (Default) | dssCap}
    */
    DSS_CAPI_V7_DLL void Solution_Set_AddType(int32_t Value);

    /*
    Generator kW for AutoAdd mode
    */
    DSS_CAPI_V7_DLL double Solution_Get_GenkW(void);

    /*
    Generator kW for AutoAdd mode
    */
    DSS_CAPI_V7_DLL void Solution_Set_GenkW(double Value);

    /*
    PF for generators in AutoAdd mode
    */
    DSS_CAPI_V7_DLL double Solution_Get_GenPF(void);

    /*
    PF for generators in AutoAdd mode
    */
    DSS_CAPI_V7_DLL void Solution_Set_GenPF(double Value);

    /*
    Capacitor kvar for adding capacitors in AutoAdd mode
    */
    DSS_CAPI_V7_DLL double Solution_Get_Capkvar(void);

    /*
    Capacitor kvar for adding capacitors in AutoAdd mode
    */
    DSS_CAPI_V7_DLL void Solution_Set_Capkvar(double Value);

    /*
    Base Solution algorithm: {dssNormalSolve | dssNewtonSolve}
    */
    DSS_CAPI_V7_DLL int32_t Solution_Get_Algorithm(void);

    /*
    Base Solution algorithm: {dssNormalSolve | dssNewtonSolve}
    */
    DSS_CAPI_V7_DLL void Solution_Set_Algorithm(int32_t Value);

    /*
    {dssStatic* | dssEvent | dssTime}  Modes for control devices
    */
    DSS_CAPI_V7_DLL int32_t Solution_Get_ControlMode(void);

    /*
    {dssStatic* | dssEvent | dssTime}  Modes for control devices
    */
    DSS_CAPI_V7_DLL void Solution_Set_ControlMode(int32_t Value);

    /*
    Default Multiplier applied to generators (like LoadMult)
    */
    DSS_CAPI_V7_DLL double Solution_Get_GenMult(void);

    /*
    Default Multiplier applied to generators (like LoadMult)
    */
    DSS_CAPI_V7_DLL void Solution_Set_GenMult(double Value);

    /*
    Default daily load shape (defaults to "Default")
    */
    DSS_CAPI_V7_DLL char* Solution_Get_DefaultDaily(void);

    /*
    Default Yearly load shape (defaults to "Default")
    */
    DSS_CAPI_V7_DLL char* Solution_Get_DefaultYearly(void);

    /*
    Default daily load shape (defaults to "Default")
    */
    DSS_CAPI_V7_DLL void Solution_Set_DefaultDaily(char* Value);

    /*
    Default Yearly load shape (defaults to "Default")
    */
    DSS_CAPI_V7_DLL void Solution_Set_DefaultYearly(char* Value);

    /*
    Array of strings containing the Event Log
    */
    DSS_CAPI_V7_DLL void Solution_Get_EventLog(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as Solution_Get_EventLog but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Solution_Get_EventLog_GR(void);

    /*
    Hour as a double, including fractional part
    */
    DSS_CAPI_V7_DLL double Solution_Get_dblHour(void);

    /*
    Hour as a double, including fractional part
    */
    DSS_CAPI_V7_DLL void Solution_Set_dblHour(double Value);

    /*
    Set Stepsize in Hr
    */
    DSS_CAPI_V7_DLL void Solution_Set_StepsizeHr(double Value);

    /*
    Set Stepsize in minutes
    */
    DSS_CAPI_V7_DLL void Solution_Set_StepsizeMin(double Value);

    /*
    Value of the control iteration counter
    */
    DSS_CAPI_V7_DLL int32_t Solution_Get_ControlIterations(void);

    /*
    Maximum allowable control iterations
    */
    DSS_CAPI_V7_DLL int32_t Solution_Get_MaxControlIterations(void);

    DSS_CAPI_V7_DLL void Solution_Sample_DoControlActions(void);

    /*
    Value of the control iteration counter
    */
    DSS_CAPI_V7_DLL void Solution_Set_ControlIterations(int32_t Value);

    /*
    Maximum allowable control iterations
    */
    DSS_CAPI_V7_DLL void Solution_Set_MaxControlIterations(int32_t Value);

    DSS_CAPI_V7_DLL void Solution_CheckFaultStatus(void);

    DSS_CAPI_V7_DLL void Solution_SolveDirect(void);

    DSS_CAPI_V7_DLL void Solution_SolveNoControl(void);

    DSS_CAPI_V7_DLL void Solution_SolvePflow(void);

    DSS_CAPI_V7_DLL void Solution_SolvePlusControl(void);

    DSS_CAPI_V7_DLL void Solution_SolveSnap(void);

    DSS_CAPI_V7_DLL void Solution_CheckControls(void);

    DSS_CAPI_V7_DLL void Solution_InitSnap(void);

    /*
    Flag that indicates if elements of the System Y have been changed by recent activity.
    */
    DSS_CAPI_V7_DLL uint16_t Solution_Get_SystemYChanged(void);

    DSS_CAPI_V7_DLL void Solution_BuildYMatrix(int32_t BuildOption, int32_t AllocateVI);

    DSS_CAPI_V7_DLL void Solution_DoControlActions(void);

    DSS_CAPI_V7_DLL void Solution_SampleControlDevices(void);

    /*
    Flag to indicate whether the circuit solution converged
    */
    DSS_CAPI_V7_DLL uint16_t Solution_Get_Converged(void);

    /*
    Flag to indicate whether the circuit solution converged
    */
    DSS_CAPI_V7_DLL void Solution_Set_Converged(uint16_t Value);

    /*
    Total iterations including control iterations for most recent solution.
    */
    DSS_CAPI_V7_DLL int32_t Solution_Get_Totaliterations(void);

    /*
    Max number of iterations required to converge at any control iteration of the most recent solution.
    */
    DSS_CAPI_V7_DLL int32_t Solution_Get_MostIterationsDone(void);

    /*
    Flag indicating the control actions are done.
    */
    DSS_CAPI_V7_DLL uint16_t Solution_Get_ControlActionsDone(void);

    DSS_CAPI_V7_DLL void Solution_Set_ControlActionsDone(uint16_t Value);

    DSS_CAPI_V7_DLL void Solution_Cleanup(void);

    DSS_CAPI_V7_DLL void Solution_FinishTimeStep(void);

    /*
    Gets the time required to perform the latest solution (Read only)
    */
    DSS_CAPI_V7_DLL double Solution_Get_Process_Time(void);

    /*
    Gets the accumulated time of the simulation
    */
    DSS_CAPI_V7_DLL double Solution_Get_Total_Time(void);

    /*
    Sets the Accumulated time of the simulation
    */
    DSS_CAPI_V7_DLL void Solution_Set_Total_Time(double Value);

    /*
    Get the solution process time + sample time for time step
    */
    DSS_CAPI_V7_DLL double Solution_Get_Time_of_Step(void);

    /*
    Get/Set the Solution.IntervalHrs variable used for devices that integrate
    */
    DSS_CAPI_V7_DLL double Solution_Get_IntervalHrs(void);

    /*
    Get/Set the Solution.IntervalHrs variable for custom solution algorithms
    */
    DSS_CAPI_V7_DLL void Solution_Set_IntervalHrs(double Value);

    /*
    Minimum number of iterations required for a power flow solution.
    */
    DSS_CAPI_V7_DLL int32_t Solution_Get_MinIterations(void);

    /*
    Mininum number of iterations required for a power flow solution.
    */
    DSS_CAPI_V7_DLL void Solution_Set_MinIterations(int32_t Value);

    DSS_CAPI_V7_DLL void Solution_Get_IncMatrix(int32_t** ResultPtr, int32_t* ResultCount);

    /*
    Same as Solution_Get_IncMatrix but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Solution_Get_IncMatrix_GR(void);

    DSS_CAPI_V7_DLL void Solution_Get_Laplacian(int32_t** ResultPtr, int32_t* ResultCount);

    /*
    Same as Solution_Get_Laplacian but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Solution_Get_Laplacian_GR(void);

    DSS_CAPI_V7_DLL void Solution_Get_BusLevels(int32_t** ResultPtr, int32_t* ResultCount);
    /*
    Same as Solution_Get_BusLevels but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Solution_Get_BusLevels_GR(void);

    DSS_CAPI_V7_DLL void Solution_Get_IncMatrixRows(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as Solution_Get_IncMatrixRows but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Solution_Get_IncMatrixRows_GR(void);

    DSS_CAPI_V7_DLL void Solution_Get_IncMatrixCols(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as Solution_Get_IncMatrixCols but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Solution_Get_IncMatrixCols_GR(void);

    /*
    Open or Close the switch. No effect if switch is locked.  However, Reset removes any lock and then closes the switch (shelf state).
    */
    DSS_CAPI_V7_DLL int32_t SwtControls_Get_Action(void);

    /*
    Array of strings with all SwtControl names in the active circuit.
    */
    DSS_CAPI_V7_DLL void SwtControls_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as SwtControls_Get_AllNames but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void SwtControls_Get_AllNames_GR(void);

    /*
    Time delay [s] betwen arming and opening or closing the switch.  Control may reset before actually operating the switch.
    */
    DSS_CAPI_V7_DLL double SwtControls_Get_Delay(void);

    /*
    Sets the first SwtControl active. Returns 0 if no more.
    */
    DSS_CAPI_V7_DLL int32_t SwtControls_Get_First(void);

    /*
    The lock prevents both manual and automatic switch operation.
    */
    DSS_CAPI_V7_DLL uint16_t SwtControls_Get_IsLocked(void);

    /*
    Sets a SwtControl active by Name.
    */
    DSS_CAPI_V7_DLL char* SwtControls_Get_Name(void);

    /*
    Sets the next SwtControl active. Returns 0 if no more.
    */
    DSS_CAPI_V7_DLL int32_t SwtControls_Get_Next(void);

    /*
    Full name of the switched element.
    */
    DSS_CAPI_V7_DLL char* SwtControls_Get_SwitchedObj(void);

    /*
    Terminal number where the switch is located on the SwitchedObj
    */
    DSS_CAPI_V7_DLL int32_t SwtControls_Get_SwitchedTerm(void);

    /*
    Open or Close the switch. No effect if switch is locked.  However, Reset removes any lock and then closes the switch (shelf state).
    */
    DSS_CAPI_V7_DLL void SwtControls_Set_Action(int32_t Value);

    /*
    Time delay [s] betwen arming and opening or closing the switch.  Control may reset before actually operating the switch.
    */
    DSS_CAPI_V7_DLL void SwtControls_Set_Delay(double Value);

    /*
    The lock prevents both manual and automatic switch operation.
    */
    DSS_CAPI_V7_DLL void SwtControls_Set_IsLocked(uint16_t Value);

    /*
    Sets a SwtControl active by Name.
    */
    DSS_CAPI_V7_DLL void SwtControls_Set_Name(char* Value);

    /*
    Full name of the switched element.
    */
    DSS_CAPI_V7_DLL void SwtControls_Set_SwitchedObj(char* Value);

    /*
    Terminal number where the switch is located on the SwitchedObj
    */
    DSS_CAPI_V7_DLL void SwtControls_Set_SwitchedTerm(int32_t Value);

    DSS_CAPI_V7_DLL int32_t SwtControls_Get_Count(void);

    /*
    Get Normal state of switch
    */
    DSS_CAPI_V7_DLL int32_t SwtControls_Get_NormalState(void);

    /*
    set Normal state of switch  (see actioncodes) dssActionOpen or dssActionClose
    */
    DSS_CAPI_V7_DLL void SwtControls_Set_NormalState(int32_t Value);

    /*
    Force switch to specified state
    */
    DSS_CAPI_V7_DLL int32_t SwtControls_Get_State(void);

    /*
    Get Present state of switch
    */
    DSS_CAPI_V7_DLL void SwtControls_Set_State(int32_t Value);

    DSS_CAPI_V7_DLL void SwtControls_Reset(void);

    /*
    Input command string for the DSS.
    */
    DSS_CAPI_V7_DLL char* Text_Get_Command(void);

    /*
    Input command string for the DSS.
    */
    DSS_CAPI_V7_DLL void Text_Set_Command(char* Value);

    /*
    Result string for the last command.
    */
    DSS_CAPI_V7_DLL char* Text_Get_Result(void);

    /*
    Number of loops
    */
    DSS_CAPI_V7_DLL int32_t Topology_Get_NumLoops(void);

    /*
    Returns index of the active branch
    */
    DSS_CAPI_V7_DLL int32_t Topology_Get_ActiveBranch(void);

    /*
    Array of all isolated branch names.
    */
    DSS_CAPI_V7_DLL void Topology_Get_AllIsolatedBranches(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as Topology_Get_AllIsolatedBranches but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Topology_Get_AllIsolatedBranches_GR(void);

    /*
    Array of all looped element names, by pairs.
    */
    DSS_CAPI_V7_DLL void Topology_Get_AllLoopedPairs(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as Topology_Get_AllLoopedPairs but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Topology_Get_AllLoopedPairs_GR(void);

    /*
    MOve back toward the source, return index of new active branch, or 0 if no more.
    */
    DSS_CAPI_V7_DLL int32_t Topology_Get_BackwardBranch(void);

    /*
    Name of the active branch.
    */
    DSS_CAPI_V7_DLL char* Topology_Get_BranchName(void);

    /*
    Sets the first branch active, returns 0 if none.
    */
    DSS_CAPI_V7_DLL int32_t Topology_Get_First(void);

    /*
    Move forward in the tree, return index of new active branch or 0 if no more
    */
    DSS_CAPI_V7_DLL int32_t Topology_Get_ForwardBranch(void);

    /*
    Move to looped branch, return index or 0 if none.
    */
    DSS_CAPI_V7_DLL int32_t Topology_Get_LoopedBranch(void);

    /*
    Sets the next branch active, returns 0 if no more.
    */
    DSS_CAPI_V7_DLL int32_t Topology_Get_Next(void);

    /*
    Number of isolated branches (PD elements and capacitors).
    */
    DSS_CAPI_V7_DLL int32_t Topology_Get_NumIsolatedBranches(void);

    /*
    Move to directly parallel branch, return index or 0 if none.
    */
    DSS_CAPI_V7_DLL int32_t Topology_Get_ParallelBranch(void);

    DSS_CAPI_V7_DLL void Topology_Set_BranchName(char* Value);

    /*
    Array of all isolated load names.
    */
    DSS_CAPI_V7_DLL void Topology_Get_AllIsolatedLoads(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as Topology_Get_AllIsolatedLoads but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Topology_Get_AllIsolatedLoads_GR(void);

    /*
    First load at the active branch, return index or 0 if none.
    */
    DSS_CAPI_V7_DLL int32_t Topology_Get_FirstLoad(void);

    /*
    Next load at the active branch, return index or 0 if no more.
    */
    DSS_CAPI_V7_DLL int32_t Topology_Get_NextLoad(void);

    /*
    Number of isolated loads
    */
    DSS_CAPI_V7_DLL int32_t Topology_Get_NumIsolatedLoads(void);

    /*
    Topological depth of the active branch
    */
    DSS_CAPI_V7_DLL int32_t Topology_Get_ActiveLevel(void);

    DSS_CAPI_V7_DLL char* Topology_Get_BusName(void);

    /*
    Set the active branch to one containing this bus, return index or 0 if not found
    */
    DSS_CAPI_V7_DLL void Topology_Set_BusName(char* Value);

    /*
    Array of strings with all Transformer names in the active circuit.
    */
    DSS_CAPI_V7_DLL void Transformers_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as Transformers_Get_AllNames but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Transformers_Get_AllNames_GR(void);

    /*
    Sets the first Transformer active. Returns 0 if no more.
    */
    DSS_CAPI_V7_DLL int32_t Transformers_Get_First(void);

    /*
    Active Winding delta or wye connection?
    */
    DSS_CAPI_V7_DLL uint16_t Transformers_Get_IsDelta(void);

    /*
    Active Winding kV rating.  Phase-phase for 2 or 3 phases, actual winding kV for 1 phase transformer.
    */
    DSS_CAPI_V7_DLL double Transformers_Get_kV(void);

    /*
    Active Winding kVA rating. On winding 1, this also determines normal and emergency current ratings for all windings.
    */
    DSS_CAPI_V7_DLL double Transformers_Get_kVA(void);

    /*
    Active Winding maximum tap in per-unit.
    */
    DSS_CAPI_V7_DLL double Transformers_Get_MaxTap(void);

    /*
    Active Winding minimum tap in per-unit.
    */
    DSS_CAPI_V7_DLL double Transformers_Get_MinTap(void);

    /*
    Sets a Transformer active by Name.
    */
    DSS_CAPI_V7_DLL char* Transformers_Get_Name(void);

    /*
    Sets the next Transformer active. Returns 0 if no more.
    */
    DSS_CAPI_V7_DLL int32_t Transformers_Get_Next(void);

    /*
    Active Winding number of tap steps betwein MinTap and MaxTap.
    */
    DSS_CAPI_V7_DLL int32_t Transformers_Get_NumTaps(void);

    /*
    Number of windings on this transformer. Allocates memory; set or change this property first.
    */
    DSS_CAPI_V7_DLL int32_t Transformers_Get_NumWindings(void);

    /*
    Active Winding resistance in %
    */
    DSS_CAPI_V7_DLL double Transformers_Get_R(void);

    /*
    Active Winding neutral resistance [ohms] for wye connections. Set less than zero for ungrounded wye.
    */
    DSS_CAPI_V7_DLL double Transformers_Get_Rneut(void);

    /*
    Active Winding tap in per-unit.
    */
    DSS_CAPI_V7_DLL double Transformers_Get_Tap(void);

    /*
    Active Winding Number from 1..NumWindings. Update this before reading or setting a sequence of winding properties (R, Tap, kV, kVA, etc.)
    */
    DSS_CAPI_V7_DLL int32_t Transformers_Get_Wdg(void);

    /*
    Name of an XfrmCode that supplies electircal parameters for this Transformer.
    */
    DSS_CAPI_V7_DLL char* Transformers_Get_XfmrCode(void);

    /*
    Percent reactance between windings 1 and 2, on winding 1 kVA base. Use for 2-winding or 3-winding transformers.
    */
    DSS_CAPI_V7_DLL double Transformers_Get_Xhl(void);

    /*
    Percent reactance between windigns 1 and 3, on winding 1 kVA base.  Use for 3-winding transformers only.
    */
    DSS_CAPI_V7_DLL double Transformers_Get_Xht(void);

    /*
    Percent reactance between windings 2 and 3, on winding 1 kVA base. Use for 3-winding transformers only.
    */
    DSS_CAPI_V7_DLL double Transformers_Get_Xlt(void);

    /*
    Active Winding neutral reactance [ohms] for wye connections.
    */
    DSS_CAPI_V7_DLL double Transformers_Get_Xneut(void);

    /*
    Active Winding delta or wye connection?
    */
    DSS_CAPI_V7_DLL void Transformers_Set_IsDelta(uint16_t Value);

    /*
    Active Winding kV rating.  Phase-phase for 2 or 3 phases, actual winding kV for 1 phase transformer.
    */
    DSS_CAPI_V7_DLL void Transformers_Set_kV(double Value);

    /*
    Active Winding kVA rating. On winding 1, this also determines normal and emergency current ratings for all windings.
    */
    DSS_CAPI_V7_DLL void Transformers_Set_kVA(double Value);

    /*
    Active Winding maximum tap in per-unit.
    */
    DSS_CAPI_V7_DLL void Transformers_Set_MaxTap(double Value);

    /*
    Active Winding minimum tap in per-unit.
    */
    DSS_CAPI_V7_DLL void Transformers_Set_MinTap(double Value);

    /*
    Sets a Transformer active by Name.
    */
    DSS_CAPI_V7_DLL void Transformers_Set_Name(char* Value);

    /*
    Active Winding number of tap steps betwein MinTap and MaxTap.
    */
    DSS_CAPI_V7_DLL void Transformers_Set_NumTaps(int32_t Value);

    /*
    Number of windings on this transformer. Allocates memory; set or change this property first.
    */
    DSS_CAPI_V7_DLL void Transformers_Set_NumWindings(int32_t Value);

    /*
    Active Winding resistance in %
    */
    DSS_CAPI_V7_DLL void Transformers_Set_R(double Value);

    /*
    Active Winding neutral resistance [ohms] for wye connections. Set less than zero for ungrounded wye.
    */
    DSS_CAPI_V7_DLL void Transformers_Set_Rneut(double Value);

    /*
    Active Winding tap in per-unit.
    */
    DSS_CAPI_V7_DLL void Transformers_Set_Tap(double Value);

    /*
    Active Winding Number from 1..NumWindings. Update this before reading or setting a sequence of winding properties (R, Tap, kV, kVA, etc.)
    */
    DSS_CAPI_V7_DLL void Transformers_Set_Wdg(int32_t Value);

    /*
    Name of an XfrmCode that supplies electircal parameters for this Transformer.
    */
    DSS_CAPI_V7_DLL void Transformers_Set_XfmrCode(char* Value);

    /*
    Percent reactance between windings 1 and 2, on winding 1 kVA base. Use for 2-winding or 3-winding transformers.
    */
    DSS_CAPI_V7_DLL void Transformers_Set_Xhl(double Value);

    /*
    Percent reactance between windigns 1 and 3, on winding 1 kVA base.  Use for 3-winding transformers only.
    */
    DSS_CAPI_V7_DLL void Transformers_Set_Xht(double Value);

    /*
    Percent reactance between windings 2 and 3, on winding 1 kVA base. Use for 3-winding transformers only.
    */
    DSS_CAPI_V7_DLL void Transformers_Set_Xlt(double Value);

    /*
    Active Winding neutral reactance [ohms] for wye connections.
    */
    DSS_CAPI_V7_DLL void Transformers_Set_Xneut(double Value);

    DSS_CAPI_V7_DLL int32_t Transformers_Get_Count(void);

    /*
    Complex array of voltages for active winding
    */
    DSS_CAPI_V7_DLL void Transformers_Get_WdgVoltages(double** ResultPtr, int32_t* ResultCount);

    /*
    Same as Transformers_Get_WdgVoltages but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Transformers_Get_WdgVoltages_GR(void);

    /*
    All Winding currents (ph1, wdg1, wdg2,... ph2, wdg1, wdg2 ...)
    */
    DSS_CAPI_V7_DLL void Transformers_Get_WdgCurrents(double** ResultPtr, int32_t* ResultCount);

    /*
    Same as Transformers_Get_WdgCurrents but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Transformers_Get_WdgCurrents_GR(void);

    /*
    All winding currents in CSV string form like the WdgCurrents property
    */
    DSS_CAPI_V7_DLL char* Transformers_Get_strWdgCurrents(void);

    /*
    Transformer Core Type: 0=shell;1 = 1-phase; 3= 3-leg; 5= 5-leg
    */
    DSS_CAPI_V7_DLL int32_t Transformers_Get_CoreType(void);

    /*
    Transformer Core Type: 0=shell;1 = 1-phase; 3= 3-leg; 5= 5-leg
    */
    DSS_CAPI_V7_DLL void Transformers_Set_CoreType(int32_t Value);

    /*
    dc Resistance of active winding in ohms for GIC analysis
    */
    DSS_CAPI_V7_DLL double Transformers_Get_RdcOhms(void);

    /*
    dc Resistance of active winding in ohms for GIC analysis
    */
    DSS_CAPI_V7_DLL void Transformers_Set_RdcOhms(double Value);

    /*
    Names of all Vsource objects in the circuit
    */
    DSS_CAPI_V7_DLL void Vsources_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    /*
    Same as Vsources_Get_AllNames but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void Vsources_Get_AllNames_GR(void);

    /*
    Number of Vsource Object
    */
    DSS_CAPI_V7_DLL int32_t Vsources_Get_Count(void);

    /*
    Sets the first VSOURCE to be active; Returns 0 if none
    */
    DSS_CAPI_V7_DLL int32_t Vsources_Get_First(void);

    /*
    Sets the next VSOURCE object to be active; returns zero if no more
    */
    DSS_CAPI_V7_DLL int32_t Vsources_Get_Next(void);

    /*
    Get Active VSOURCE name
    */
    DSS_CAPI_V7_DLL char* Vsources_Get_Name(void);

    /*
    Set Active VSOURCE by Name
    */
    DSS_CAPI_V7_DLL void Vsources_Set_Name(char* Value);

    /*
    Source Voltage in kV
    */
    DSS_CAPI_V7_DLL double Vsources_Get_BasekV(void);

    /*
    Source pu voltage.
    */
    DSS_CAPI_V7_DLL double Vsources_Get_pu(void);

    /*
    Source voltage in kV
    */
    DSS_CAPI_V7_DLL void Vsources_Set_BasekV(double Value);

    /*
    Per-unit value of source voltage based on kV
    */
    DSS_CAPI_V7_DLL void Vsources_Set_pu(double Value);

    /*
    Phase angle of first phase in degrees
    */
    DSS_CAPI_V7_DLL double Vsources_Get_AngleDeg(void);

    /*
    Source Frequency in Hz
    */
    DSS_CAPI_V7_DLL double Vsources_Get_Frequency(void);

    /*
    Number of Phases
    */
    DSS_CAPI_V7_DLL int32_t Vsources_Get_Phases(void);

    /*
    phase angle in degrees
    */
    DSS_CAPI_V7_DLL void Vsources_Set_AngleDeg(double Value);

    /*
    Source frequency in Hz
    */
    DSS_CAPI_V7_DLL void Vsources_Set_Frequency(double Value);

    /*
    Number of phases
    */
    DSS_CAPI_V7_DLL void Vsources_Set_Phases(int32_t Value);

    /*
    Number of XYCurve Objects
    */
    DSS_CAPI_V7_DLL int32_t XYCurves_Get_Count(void);

    /*
    Sets first XYcurve object active; returns 0 if none.
    */
    DSS_CAPI_V7_DLL int32_t XYCurves_Get_First(void);

    /*
    Name of active XYCurve Object
    */
    DSS_CAPI_V7_DLL char* XYCurves_Get_Name(void);

    /*
    Advances to next XYCurve object; returns 0 if no more objects of this class
    */
    DSS_CAPI_V7_DLL int32_t XYCurves_Get_Next(void);

    /*
    Get Name of active XYCurve Object
    */
    DSS_CAPI_V7_DLL void XYCurves_Set_Name(char* Value);

    /*
    Get/Set Number of points in X-Y curve
    */
    DSS_CAPI_V7_DLL int32_t XYCurves_Get_Npts(void);

    /*
    Get/Set X values as a Array of doubles. Set Npts to max number expected if setting
    */
    DSS_CAPI_V7_DLL void XYCurves_Get_Xarray(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as XYCurves_Get_Xarray but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void XYCurves_Get_Xarray_GR(void);

    /*
    Get/Set Number of Points in X-Y curve
    */
    DSS_CAPI_V7_DLL void XYCurves_Set_Npts(int32_t Value);

    /*
    Get/Set X values as a Array of doubles. Set Npts to max number expected if setting
    */
    DSS_CAPI_V7_DLL void XYCurves_Set_Xarray(double* ValuePtr, int32_t ValueCount);

    /*
    Set X value or get interpolated value after setting Y
    */
    DSS_CAPI_V7_DLL double XYCurves_Get_x(void);

    /*
    Y value for present X or set this value then get corresponding X
    */
    DSS_CAPI_V7_DLL double XYCurves_Get_y(void);

    /*
    Get/Set Y values in curve; Set Npts to max number expected if setting
    */
    DSS_CAPI_V7_DLL void XYCurves_Get_Yarray(double** ResultPtr, int32_t* ResultCount);
    /*
    Same as XYCurves_Get_Yarray but using the global buffer interface for results
    */
    DSS_CAPI_V7_DLL void XYCurves_Get_Yarray_GR(void);

    DSS_CAPI_V7_DLL void XYCurves_Set_x(double Value);

    /*
    Set Y value or get interpolated Y value after setting X
    */
    DSS_CAPI_V7_DLL void XYCurves_Set_y(double Value);

    /*
    Get/Set Y values in curve; Set Npts to max number expected if setting
    */
    DSS_CAPI_V7_DLL void XYCurves_Set_Yarray(double* ValuePtr, int32_t ValueCount);

    /*
    Factor to scale X values from original curve
    */
    DSS_CAPI_V7_DLL double XYCurves_Get_Xscale(void);

    /*
    Amount to shift X value from original curve
    */
    DSS_CAPI_V7_DLL double XYCurves_Get_Xshift(void);

    /*
    Factor to scale Y values from original curve
    */
    DSS_CAPI_V7_DLL double XYCurves_Get_Yscale(void);

    /*
    amount to shift Y valiue from original curve
    */
    DSS_CAPI_V7_DLL double XYCurves_Get_Yshift(void);

    /*
    Factor to scale X values from original curve
    */
    DSS_CAPI_V7_DLL void XYCurves_Set_Xscale(double Value);

    DSS_CAPI_V7_DLL void XYCurves_Set_Xshift(double Value);

    /*
    Amount to scale Y values from original curve. Represents a curve shift.
    */
    DSS_CAPI_V7_DLL void XYCurves_Set_Yscale(double Value);

    DSS_CAPI_V7_DLL void XYCurves_Set_Yshift(double Value);

    DSS_CAPI_V7_DLL void YMatrix_GetCompressedYMatrix(uint16_t factor, uint32_t *nBus, uint32_t *nNz, int32_t **ColPtr, int32_t **RowIdxPtr, double **cValsPtr);
    DSS_CAPI_V7_DLL void YMatrix_ZeroInjCurr(void);
    DSS_CAPI_V7_DLL void YMatrix_GetSourceInjCurrents(void);
    DSS_CAPI_V7_DLL void YMatrix_GetPCInjCurr(void);
    DSS_CAPI_V7_DLL void YMatrix_BuildYMatrixD(int32_t BuildOps, int32_t AllocateVI);
    DSS_CAPI_V7_DLL void YMatrix_AddInAuxCurrents(int32_t SType);
    DSS_CAPI_V7_DLL void YMatrix_getIpointer(double **IvectorPtr);
    DSS_CAPI_V7_DLL void YMatrix_getVpointer(double **VvectorPtr);
    DSS_CAPI_V7_DLL int32_t YMatrix_SolveSystem(double **NodeVPtr);
    DSS_CAPI_V7_DLL void YMatrix_Set_SystemYChanged(uint16_t arg);
    DSS_CAPI_V7_DLL uint16_t YMatrix_Get_SystemYChanged(void);
    DSS_CAPI_V7_DLL void YMatrix_Set_UseAuxCurrents(uint16_t arg);
    DSS_CAPI_V7_DLL uint16_t YMatrix_Get_UseAuxCurrents(void);


    enum MonitorModes {
        MonitorModes_VI = 0x00000000,
        MonitorModes_Power = 0x00000001,
        MonitorModes_Sequence = 0x00000010,
        MonitorModes_Magnitude = 0x00000020,
        MonitorModes_PosOnly = 0x00000040,
        MonitorModes_Taps = 0x00000002,
        MonitorModes_States = 0x00000003
    };

    enum SolveModes {
        SolveModes_SnapShot = 0x00000000,
        SolveModes_DutyCycle = 0x00000006,
        SolveModes_Direct = 0x00000007,
        SolveModes_Daily = 0x00000001,
        SolveModes_Monte1 = 0x00000003,
        SolveModes_Monte2 = 0x0000000A,
        SolveModes_Monte3 = 0x0000000B,
        SolveModes_FaultStudy = 0x00000009,
        SolveModes_Yearly = 0x00000002,
        SolveModes_MonteFault = 0x00000008,
        SolveModes_PeakDay = 0x00000005,
        SolveModes_LD1 = 0x00000004,
        SolveModes_LD2 = 0x0000000C,
        SolveModes_AutoAdd = 0x0000000D,
        SolveModes_Harmonic = 0x0000000F,
        SolveModes_Dynamic = 0x0000000E
    };

    enum Options {
        Options_PowerFlow = 0x00000001,
        Options_Admittance = 0x00000002,
        Options_NormalSolve = 0x00000000,
        Options_NewtonSolve = 0x00000001,
        Options_Static = 0x00000000,
        Options_Event = 0x00000001,
        Options_Time = 0x00000002,
        Options_Multiphase = 0x00000000,
        Options_PositiveSeq = 0x00000001,
        Options_Gaussian = 0x00000001,
        Options_Uniform = 0x00000002,
        Options_LogNormal = 0x00000003,
        Options_AddGen = 0x00000001,
        Options_AddCap = 0x00000002,
        Options_ControlOFF = 0xFFFFFFFF
    };

    enum CapControlModes {
        CapControlModes_Voltage = 0x00000001,
        CapControlModes_KVAR = 0x00000002,
        CapControlModes_Current = 0x00000000,
        CapControlModes_PF = 0x00000004,
        CapControlModes_Time = 0x00000003
    };

    enum ActionCodes {
        ActionCodes_none = 0x00000000,
        ActionCodes_Open = 0x00000001,
        ActionCodes_Close = 0x00000002,
        ActionCodes_Reset = 0x00000003,
        ActionCodes_Lock = 0x00000004,
        ActionCodes_Unlock = 0x00000005,
        ActionCodes_TapUp = 0x00000006,
        ActionCodes_TapDown = 0x00000007
    };

    enum LoadStatus {
        LoadStatus_Variable = 0x00000000,
        LoadStatus_Fixed = 0x00000001,
        LoadStatus_Exempt = 0x00000002
    };

    enum LoadModels {
        LoadModels_ConstPQ = 0x00000001,
        LoadModels_ConstZ = 0x00000002,
        LoadModels_Motor = 0x00000003,
        LoadModels_CVR = 0x00000004,
        LoadModels_ConstI = 0x00000005,
        LoadModels_ConstPFixedQ = 0x00000006,
        LoadModels_ConstPFixedX = 0x00000007,
        LoadModels_ZIPV = 0x00000008
    };

    enum LineUnits {
        LineUnits_none = 0x00000000,
        LineUnits_Miles = 0x00000001,
        LineUnits_kFt = 0x00000002,
        LineUnits_km = 0x00000003,
        LineUnits_meter = 0x00000004,
        LineUnits_ft = 0x00000005,
        LineUnits_inch = 0x00000006,
        LineUnits_cm = 0x00000007,
        LineUnits_mm = 0x00000008,
        LineUnits_Maxnum = 0x00000009
    };

    // Experimental API extensions
    DSS_CAPI_V7_DLL int32_t CNData_Get_Count(void);
    DSS_CAPI_V7_DLL int32_t CNData_Get_First(void);
    DSS_CAPI_V7_DLL int32_t CNData_Get_Next(void);
    DSS_CAPI_V7_DLL char *CNData_Get_Name(void);
    DSS_CAPI_V7_DLL void CNData_Set_Name(char *Value);
    DSS_CAPI_V7_DLL void CNData_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    DSS_CAPI_V7_DLL void CNData_Get_AllNames_GR(void);
    DSS_CAPI_V7_DLL double CNData_Get_Rdc(void);
    DSS_CAPI_V7_DLL void CNData_Set_Rdc(double Value);
    DSS_CAPI_V7_DLL double CNData_Get_Rac(void);
    DSS_CAPI_V7_DLL void CNData_Set_Rac(double Value);
    DSS_CAPI_V7_DLL double CNData_Get_GMRac(void);
    DSS_CAPI_V7_DLL void CNData_Set_GMRac(double Value);
    DSS_CAPI_V7_DLL int32_t CNData_Get_GMRUnits(void);
    DSS_CAPI_V7_DLL void CNData_Set_GMRUnits(int32_t Value);
    DSS_CAPI_V7_DLL double CNData_Get_Radius(void);
    DSS_CAPI_V7_DLL void CNData_Set_Radius(double Value);
    DSS_CAPI_V7_DLL int32_t CNData_Get_RadiusUnits(void);
    DSS_CAPI_V7_DLL void CNData_Set_RadiusUnits(int32_t Value);
    DSS_CAPI_V7_DLL int32_t CNData_Get_ResistanceUnits(void);
    DSS_CAPI_V7_DLL void CNData_Set_ResistanceUnits(int32_t Value);
    DSS_CAPI_V7_DLL double CNData_Get_Diameter(void);
    DSS_CAPI_V7_DLL void CNData_Set_Diameter(double Value);
    DSS_CAPI_V7_DLL double CNData_Get_NormAmps(void);
    DSS_CAPI_V7_DLL void CNData_Set_NormAmps(double Value);
    DSS_CAPI_V7_DLL double CNData_Get_EmergAmps(void);
    DSS_CAPI_V7_DLL void CNData_Set_EmergAmps(double Value);
    DSS_CAPI_V7_DLL double CNData_Get_EpsR(void);
    DSS_CAPI_V7_DLL void CNData_Set_EpsR(double Value);
    DSS_CAPI_V7_DLL double CNData_Get_InsLayer(void);
    DSS_CAPI_V7_DLL void CNData_Set_InsLayer(double Value);
    DSS_CAPI_V7_DLL double CNData_Get_DiaIns(void);
    DSS_CAPI_V7_DLL void CNData_Set_DiaIns(double Value);
    DSS_CAPI_V7_DLL double CNData_Get_DiaCable(void);
    DSS_CAPI_V7_DLL void CNData_Set_DiaCable(double Value);
    DSS_CAPI_V7_DLL int32_t CNData_Get_k(void);
    DSS_CAPI_V7_DLL void CNData_Set_k(int32_t Value);
    DSS_CAPI_V7_DLL double CNData_Get_DiaStrand(void);
    DSS_CAPI_V7_DLL void CNData_Set_DiaStrand(double Value);
    DSS_CAPI_V7_DLL double CNData_Get_GmrStrand(void);
    DSS_CAPI_V7_DLL void CNData_Set_GmrStrand(double Value);
    DSS_CAPI_V7_DLL double CNData_Get_RStrand(void);
    DSS_CAPI_V7_DLL void CNData_Set_RStrand(double Value);

    DSS_CAPI_V7_DLL int32_t LineGeometries_Get_Count(void);
    DSS_CAPI_V7_DLL int32_t LineGeometries_Get_First(void);
    DSS_CAPI_V7_DLL int32_t LineGeometries_Get_Next(void);
    DSS_CAPI_V7_DLL char* LineGeometries_Get_Name(void);
    DSS_CAPI_V7_DLL void LineGeometries_Set_Name(char* Value);
    DSS_CAPI_V7_DLL int32_t LineGeometries_Get_Nconds(void);
    DSS_CAPI_V7_DLL void LineGeometries_Set_Nconds(int32_t Value);
    DSS_CAPI_V7_DLL int32_t LineGeometries_Get_Phases(void);
    DSS_CAPI_V7_DLL void LineGeometries_Set_Phases(int32_t Value);
    DSS_CAPI_V7_DLL void LineGeometries_Get_Cmatrix(double** ResultPtr, int32_t* ResultCount, double Frequency, double Length, int32_t Units);
    DSS_CAPI_V7_DLL void LineGeometries_Get_Cmatrix_GR(double Frequency, double Length, int32_t Units);
    DSS_CAPI_V7_DLL void LineGeometries_Get_Rmatrix(double** ResultPtr, int32_t* ResultCount, double Frequency, double Length, int32_t Units);
    DSS_CAPI_V7_DLL void LineGeometries_Get_Rmatrix_GR(double Frequency, double Length, int32_t Units);
    DSS_CAPI_V7_DLL void LineGeometries_Get_Xmatrix(double** ResultPtr, int32_t* ResultCount, double Frequency, double Length, int32_t Units);
    DSS_CAPI_V7_DLL void LineGeometries_Get_Xmatrix_GR(double Frequency, double Length, int32_t Units);
    DSS_CAPI_V7_DLL void LineGeometries_Get_Zmatrix(double** ResultPtr, int32_t* ResultCount, double Frequency, double Length, int32_t Units);
    DSS_CAPI_V7_DLL void LineGeometries_Get_Zmatrix_GR(double Frequency, double Length, int32_t Units);
    DSS_CAPI_V7_DLL void LineGeometries_Get_Units(int32_t** ResultPtr, int32_t* ResultCount);
    DSS_CAPI_V7_DLL void LineGeometries_Get_Units_GR(void);
    DSS_CAPI_V7_DLL void LineGeometries_Set_Units(int32_t *ValuePtr, int32_t ValueCount);
    DSS_CAPI_V7_DLL void LineGeometries_Get_Xcoords(double** ResultPtr, int32_t* ResultCount);
    DSS_CAPI_V7_DLL void LineGeometries_Get_Xcoords_GR(void);
    DSS_CAPI_V7_DLL void LineGeometries_Set_Xcoords(double* ValuePtr, int32_t ValueCount);
    DSS_CAPI_V7_DLL void LineGeometries_Get_Ycoords(double** ResultPtr, int32_t* ResultCount);
    DSS_CAPI_V7_DLL void LineGeometries_Get_Ycoords_GR(void);
    DSS_CAPI_V7_DLL void LineGeometries_Set_Ycoords(double* ValuePtr, int32_t ValueCount);
    DSS_CAPI_V7_DLL void LineGeometries_Get_Conductors(char*** ResultPtr, int32_t* ResultCount);
    DSS_CAPI_V7_DLL void LineGeometries_Get_Conductors_GR(void);
    DSS_CAPI_V7_DLL uint16_t LineGeometries_Get_Reduce(void);
    DSS_CAPI_V7_DLL void LineGeometries_Set_Reduce(uint16_t Value);
    DSS_CAPI_V7_DLL double LineGeometries_Get_RhoEarth(void);
    DSS_CAPI_V7_DLL void LineGeometries_Set_RhoEarth(double Value);
    DSS_CAPI_V7_DLL double LineGeometries_Get_NormAmps(void);
    DSS_CAPI_V7_DLL void LineGeometries_Set_NormAmps(double Value);
    DSS_CAPI_V7_DLL double LineGeometries_Get_EmergAmps(void);
    DSS_CAPI_V7_DLL void LineGeometries_Set_EmergAmps(double Value);
    DSS_CAPI_V7_DLL void LineGeometries_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    DSS_CAPI_V7_DLL void LineGeometries_Get_AllNames_GR(void);

    DSS_CAPI_V7_DLL int32_t LineSpacings_Get_Count(void);
    DSS_CAPI_V7_DLL int32_t LineSpacings_Get_First(void);
    DSS_CAPI_V7_DLL int32_t LineSpacings_Get_Next(void);
    DSS_CAPI_V7_DLL char* LineSpacings_Get_Name(void);
    DSS_CAPI_V7_DLL void LineSpacings_Set_Name(char* Value);
    DSS_CAPI_V7_DLL int32_t LineSpacings_Get_Nconds(void);
    DSS_CAPI_V7_DLL void LineSpacings_Set_Nconds(int32_t Value);
    DSS_CAPI_V7_DLL int32_t LineSpacings_Get_Phases(void);
    DSS_CAPI_V7_DLL void LineSpacings_Set_Phases(int32_t Value);
    DSS_CAPI_V7_DLL int32_t LineSpacings_Get_Units(void);
    DSS_CAPI_V7_DLL void LineSpacings_Set_Units(int32_t Value);
    DSS_CAPI_V7_DLL void LineSpacings_Get_Xcoords(double** ResultPtr, int32_t* ResultCount);
    DSS_CAPI_V7_DLL void LineSpacings_Get_Xcoords_GR(void);
    DSS_CAPI_V7_DLL void LineSpacings_Set_Xcoords(double* ValuePtr, int32_t ValueCount);
    DSS_CAPI_V7_DLL void LineSpacings_Get_Ycoords(double** ResultPtr, int32_t* ResultCount);
    DSS_CAPI_V7_DLL void LineSpacings_Get_Ycoords_GR(void);
    DSS_CAPI_V7_DLL void LineSpacings_Set_Ycoords(double* ValuePtr, int32_t ValueCount);
    DSS_CAPI_V7_DLL void LineSpacings_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    DSS_CAPI_V7_DLL void LineSpacings_Get_AllNames_GR(void);

    DSS_CAPI_V7_DLL int32_t Loads_Get_Phases(void);
    DSS_CAPI_V7_DLL void Loads_Set_Phases(int32_t Integer);

    DSS_CAPI_V7_DLL void Reactors_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    DSS_CAPI_V7_DLL void Reactors_Get_AllNames_GR(void);
    DSS_CAPI_V7_DLL char* Reactors_Get_Name(void);
    DSS_CAPI_V7_DLL void Reactors_Set_Name(char *Value);
    DSS_CAPI_V7_DLL int32_t Reactors_Get_First(void);
    DSS_CAPI_V7_DLL int32_t Reactors_Get_Next(void);
    DSS_CAPI_V7_DLL int32_t Reactors_Get_Count(void);
    DSS_CAPI_V7_DLL double Reactors_Get_kV(void);
    DSS_CAPI_V7_DLL void Reactors_Set_kV(double Value);
    DSS_CAPI_V7_DLL double Reactors_Get_kvar(void);
    DSS_CAPI_V7_DLL void Reactors_Set_kvar(double Value);
    DSS_CAPI_V7_DLL int32_t Reactors_Get_Phases(void);
    DSS_CAPI_V7_DLL void Reactors_Set_Phases(int32_t Integer);
    DSS_CAPI_V7_DLL uint16_t Reactors_Get_IsDelta(void);
    DSS_CAPI_V7_DLL void Reactors_Set_IsDelta(uint16_t Value);
    DSS_CAPI_V7_DLL uint16_t Reactors_Get_Parallel(void);
    DSS_CAPI_V7_DLL void Reactors_Set_Parallel(uint16_t Value);
    DSS_CAPI_V7_DLL double Reactors_Get_LmH(void);
    DSS_CAPI_V7_DLL void Reactors_Set_LmH(double Value);
    DSS_CAPI_V7_DLL char* Reactors_Get_Bus1(void);
    DSS_CAPI_V7_DLL char* Reactors_Get_Bus2(void);
    DSS_CAPI_V7_DLL void Reactors_Set_Bus1(char* Value);
    DSS_CAPI_V7_DLL void Reactors_Set_Bus2(char* Value);
    DSS_CAPI_V7_DLL double Reactors_Get_R(void);
    DSS_CAPI_V7_DLL void Reactors_Set_R(double Value);
    DSS_CAPI_V7_DLL double Reactors_Get_X(void);
    DSS_CAPI_V7_DLL void Reactors_Set_X(double Value);
    DSS_CAPI_V7_DLL double Reactors_Get_Rp(void);
    DSS_CAPI_V7_DLL void Reactors_Set_Rp(double Value);
    DSS_CAPI_V7_DLL char* Reactors_Get_RCurve(void);
    DSS_CAPI_V7_DLL void Reactors_Set_RCurve(char* Value);
    DSS_CAPI_V7_DLL char* Reactors_Get_LCurve(void);
    DSS_CAPI_V7_DLL void Reactors_Set_LCurve(char* Value);
    DSS_CAPI_V7_DLL void Reactors_Get_Rmatrix(double** ResultPtr, int32_t* ResultCount);
    DSS_CAPI_V7_DLL void Reactors_Get_Rmatrix_GR(void);
    DSS_CAPI_V7_DLL void Reactors_Set_Rmatrix(double* ValuePtr, int32_t ValueCount);
    DSS_CAPI_V7_DLL void Reactors_Get_Xmatrix(double** ResultPtr, int32_t* ResultCount);
    DSS_CAPI_V7_DLL void Reactors_Get_Xmatrix_GR(void);
    DSS_CAPI_V7_DLL void Reactors_Set_Xmatrix(double* ValuePtr, int32_t ValueCount);
    DSS_CAPI_V7_DLL void Reactors_Get_Z(double** ResultPtr, int32_t* ResultCount);
    DSS_CAPI_V7_DLL void Reactors_Get_Z_GR(void);
    DSS_CAPI_V7_DLL void Reactors_Set_Z(double* ValuePtr, int32_t ValueCount);
    DSS_CAPI_V7_DLL void Reactors_Get_Z1(double** ResultPtr, int32_t* ResultCount);
    DSS_CAPI_V7_DLL void Reactors_Get_Z1_GR(void);
    DSS_CAPI_V7_DLL void Reactors_Set_Z1(double* ValuePtr, int32_t ValueCount);
    DSS_CAPI_V7_DLL void Reactors_Get_Z2(double** ResultPtr, int32_t* ResultCount);
    DSS_CAPI_V7_DLL void Reactors_Get_Z2_GR(void);
    DSS_CAPI_V7_DLL void Reactors_Set_Z2(double* ValuePtr, int32_t ValueCount);
    DSS_CAPI_V7_DLL void Reactors_Get_Z0(double** ResultPtr, int32_t* ResultCount);
    DSS_CAPI_V7_DLL void Reactors_Get_Z0_GR(void);
    DSS_CAPI_V7_DLL void Reactors_Set_Z0(double* ValuePtr, int32_t ValueCount);
    DSS_CAPI_V7_DLL int32_t Reactors_Get_SpecType(void);

    DSS_CAPI_V7_DLL int32_t TSData_Get_Count(void);
    DSS_CAPI_V7_DLL int32_t TSData_Get_First(void);
    DSS_CAPI_V7_DLL int32_t TSData_Get_Next(void);
    DSS_CAPI_V7_DLL char *TSData_Get_Name(void);
    DSS_CAPI_V7_DLL void TSData_Set_Name(char *Value);
    DSS_CAPI_V7_DLL void TSData_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    DSS_CAPI_V7_DLL void TSData_Get_AllNames_GR(void);
    DSS_CAPI_V7_DLL double TSData_Get_Rdc(void);
    DSS_CAPI_V7_DLL void TSData_Set_Rdc(double Value);
    DSS_CAPI_V7_DLL double TSData_Get_Rac(void);
    DSS_CAPI_V7_DLL void TSData_Set_Rac(double Value);
    DSS_CAPI_V7_DLL double TSData_Get_GMRac(void);
    DSS_CAPI_V7_DLL void TSData_Set_GMRac(double Value);
    DSS_CAPI_V7_DLL int32_t TSData_Get_GMRUnits(void);
    DSS_CAPI_V7_DLL void TSData_Set_GMRUnits(int32_t Value);
    DSS_CAPI_V7_DLL double TSData_Get_Radius(void);
    DSS_CAPI_V7_DLL void TSData_Set_Radius(double Value);
    DSS_CAPI_V7_DLL int32_t TSData_Get_RadiusUnits(void);
    DSS_CAPI_V7_DLL void TSData_Set_RadiusUnits(int32_t Value);
    DSS_CAPI_V7_DLL int32_t TSData_Get_ResistanceUnits(void);
    DSS_CAPI_V7_DLL void TSData_Set_ResistanceUnits(int32_t Value);
    DSS_CAPI_V7_DLL double TSData_Get_Diameter(void);
    DSS_CAPI_V7_DLL void TSData_Set_Diameter(double Value);
    DSS_CAPI_V7_DLL double TSData_Get_NormAmps(void);
    DSS_CAPI_V7_DLL void TSData_Set_NormAmps(double Value);
    DSS_CAPI_V7_DLL double TSData_Get_EmergAmps(void);
    DSS_CAPI_V7_DLL void TSData_Set_EmergAmps(double Value);
    DSS_CAPI_V7_DLL double TSData_Get_EpsR(void);
    DSS_CAPI_V7_DLL void TSData_Set_EpsR(double Value);
    DSS_CAPI_V7_DLL double TSData_Get_InsLayer(void);
    DSS_CAPI_V7_DLL void TSData_Set_InsLayer(double Value);
    DSS_CAPI_V7_DLL double TSData_Get_DiaIns(void);
    DSS_CAPI_V7_DLL void TSData_Set_DiaIns(double Value);
    DSS_CAPI_V7_DLL double TSData_Get_DiaCable(void);
    DSS_CAPI_V7_DLL void TSData_Set_DiaCable(double Value);
    DSS_CAPI_V7_DLL double TSData_Get_DiaShield(void);
    DSS_CAPI_V7_DLL void TSData_Set_DiaShield(double Value);
    DSS_CAPI_V7_DLL double TSData_Get_TapeLayer(void);
    DSS_CAPI_V7_DLL void TSData_Set_TapeLayer(double Value);
    DSS_CAPI_V7_DLL double TSData_Get_TapeLap(void);
    DSS_CAPI_V7_DLL void TSData_Set_TapeLap(double Value);

    DSS_CAPI_V7_DLL int32_t WireData_Get_Count(void);
    DSS_CAPI_V7_DLL int32_t WireData_Get_First(void);
    DSS_CAPI_V7_DLL int32_t WireData_Get_Next(void);
    DSS_CAPI_V7_DLL char* WireData_Get_Name(void);
    DSS_CAPI_V7_DLL void WireData_Set_Name(char* Value);
    DSS_CAPI_V7_DLL void WireData_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    DSS_CAPI_V7_DLL void WireData_Get_AllNames_GR(void);
    DSS_CAPI_V7_DLL double WireData_Get_Rdc(void);
    DSS_CAPI_V7_DLL void WireData_Set_Rdc(double Value);
    DSS_CAPI_V7_DLL double WireData_Get_Rac(void);
    DSS_CAPI_V7_DLL void WireData_Set_Rac(double Value);
    DSS_CAPI_V7_DLL double WireData_Get_GMRac(void);
    DSS_CAPI_V7_DLL void WireData_Set_GMRac(double Value);
    DSS_CAPI_V7_DLL int32_t WireData_Get_GMRUnits(void);
    DSS_CAPI_V7_DLL void WireData_Set_GMRUnits(int32_t Value);
    DSS_CAPI_V7_DLL double WireData_Get_Radius(void);
    DSS_CAPI_V7_DLL void WireData_Set_Radius(double Value);
    DSS_CAPI_V7_DLL int32_t WireData_Get_RadiusUnits(void);
    DSS_CAPI_V7_DLL void WireData_Set_RadiusUnits(int32_t Value);
    DSS_CAPI_V7_DLL int32_t WireData_Get_ResistanceUnits(void);
    DSS_CAPI_V7_DLL void WireData_Set_ResistanceUnits(int32_t Value);
    DSS_CAPI_V7_DLL double WireData_Get_Diameter(void);
    DSS_CAPI_V7_DLL void WireData_Set_Diameter(double Value);
    DSS_CAPI_V7_DLL double WireData_Get_NormAmps(void);
    DSS_CAPI_V7_DLL void WireData_Set_NormAmps(double Value);
    DSS_CAPI_V7_DLL double WireData_Get_EmergAmps(void);
    DSS_CAPI_V7_DLL void WireData_Set_EmergAmps(double Value);

    /*
    Set the next bus as active. Returns -1 if no more buses, 0 otherwise.
    */
    DSS_CAPI_V7_DLL int32_t Bus_Get_Next(void);

    /* 
    Gets/sets the DSS script error-handling behavior. If a warning or error 
    occurs and early abortion is enabled (default), the processing of the 
    script is always halted. Otherwise, the processing of the script continues
    until a major error occurs or it finishes.
    */
    DSS_CAPI_V7_DLL uint16_t Error_Get_EarlyAbort(void);
    DSS_CAPI_V7_DLL void Error_Set_EarlyAbort(uint16_t Value);

#ifdef __cplusplus
} // extern "C"
#endif
#endif
