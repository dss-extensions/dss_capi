#ifndef DSSPM_CAPI_DLL_H
#define DSSPM_CAPI_DLL_H
#define DSSPM_CAPI_VERSION "0.9.4"
#ifndef DSSPM_CAPI_DLL
//#define DSSPM_CAPI_DLL __declspec(dllimport)
#define DSSPM_CAPI_DLL
#endif
#ifdef __cplusplus
extern "C" {
#else
#endif
    DSSPM_CAPI_DLL void DSS_ResetStringBuffer(void);
    DSSPM_CAPI_DLL void DSS_Dispose_PByte(int8_t** p);
    DSSPM_CAPI_DLL void DSS_Dispose_PDouble(double** p);
    DSSPM_CAPI_DLL void DSS_Dispose_PInteger(int32_t** p);
    DSSPM_CAPI_DLL void DSS_Dispose_PPAnsiChar(char ***p, int32_t cnt);

    
    /*
    Array of strings consisting of all element names in the active class.
    */
    DSSPM_CAPI_DLL void ActiveClass_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Sets first element in the active class to be the active DSS object. If object is a CktElement, ActiveCktELment also points to this element. Returns 0 if none.
    */
    DSSPM_CAPI_DLL int32_t ActiveClass_Get_First(void);
    
    /*
    Sets next element in active class to be the active DSS object. If object is a CktElement, ActiveCktElement also points to this element.  Returns 0 if no more.
    */
    DSSPM_CAPI_DLL int32_t ActiveClass_Get_Next(void);
    
    /*
    Name of the Active Element of the Active Class
    */
    DSSPM_CAPI_DLL char* ActiveClass_Get_Name(void);
    
    DSSPM_CAPI_DLL void ActiveClass_Set_Name(char* Value);
    
    /*
    Number of elements in this class. Same as Count property.
    */
    DSSPM_CAPI_DLL int32_t ActiveClass_Get_NumElements(void);
    
    /*
    Returns name of active class.
    */
    DSSPM_CAPI_DLL char* ActiveClass_Get_ActiveClassName(void);
    
    /*
    Number of elements in Active Class. Same as NumElements Property.
    */
    DSSPM_CAPI_DLL int32_t ActiveClass_Get_Count(void);
    
    /*
    Name of Bus
    */
    DSSPM_CAPI_DLL char* Bus_Get_Name(void);
    
    /*
    Number of Nodes this bus.
    */
    DSSPM_CAPI_DLL int32_t Bus_Get_NumNodes(void);
    
    /*
    Double Array of sequence voltages at this bus.
    */
    DSSPM_CAPI_DLL void Bus_Get_SeqVoltages(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Complex array of voltages at this bus.
    */
    DSSPM_CAPI_DLL void Bus_Get_Voltages(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Integer Array of Node Numbers defined at the bus in same order as the voltages.
    */
    DSSPM_CAPI_DLL void Bus_Get_Nodes(int32_t** ResultPtr, int32_t* ResultCount);
    
    /*
    Short circuit currents at bus; Complex Array.
    */
    DSSPM_CAPI_DLL void Bus_Get_Isc(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Open circuit voltage; Complex array.
    */
    DSSPM_CAPI_DLL void Bus_Get_Voc(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Base voltage at bus in kV
    */
    DSSPM_CAPI_DLL double Bus_Get_kVBase(void);
    
    /*
    Complex Array of pu voltages at the bus.
    */
    DSSPM_CAPI_DLL void Bus_Get_puVoltages(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Complex Zero-Sequence short circuit impedance at bus.
    */
    DSSPM_CAPI_DLL void Bus_Get_Zsc0(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Complex Positive-Sequence short circuit impedance at bus..
    */
    DSSPM_CAPI_DLL void Bus_Get_Zsc1(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Complex array of Zsc matrix at bus. Column by column.
    */
    DSSPM_CAPI_DLL void Bus_Get_ZscMatrix(double** ResultPtr, int32_t* ResultCount);
    
    DSSPM_CAPI_DLL uint16_t Bus_ZscRefresh(void);
    
    /*
    Complex array of Ysc matrix at bus. Column by column.
    */
    DSSPM_CAPI_DLL void Bus_Get_YscMatrix(double** ResultPtr, int32_t* ResultCount);
    
    /*
    False=0 else True. Indicates whether a coordinate has been defined for this bus
    */
    DSSPM_CAPI_DLL uint16_t Bus_Get_Coorddefined(void);
    
    /*
    X Coordinate for bus (double)
    */
    DSSPM_CAPI_DLL double Bus_Get_x(void);
    
    /*
    X Coordinate for bus (double)
    */
    DSSPM_CAPI_DLL void Bus_Set_x(double Value);
    
    /*
    Y coordinate for bus(double)
    */
    DSSPM_CAPI_DLL double Bus_Get_y(void);
    
    /*
    Y coordinate for bus(double)
    */
    DSSPM_CAPI_DLL void Bus_Set_y(double Value);
    
    /*
    Distance from energymeter (if non-zero)
    */
    DSSPM_CAPI_DLL double Bus_Get_Distance(void);
    
    DSSPM_CAPI_DLL int32_t Bus_GetUniqueNodeNumber(int32_t StartNumber);
    
    /*
    Complex Double array of Sequence Voltages (0, 1, 2) at this Bus.
    */
    DSSPM_CAPI_DLL void Bus_Get_CplxSeqVoltages(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Average interruption duration, hr.
    */
    DSSPM_CAPI_DLL double Bus_Get_Int_Duration(void);
    
    /*
    Accumulated failure rate downstream from this bus; faults per year
    */
    DSSPM_CAPI_DLL double Bus_Get_Lambda(void);
    
    /*
    Accumulated customer outage durations
    */
    DSSPM_CAPI_DLL double Bus_Get_Cust_Duration(void);
    
    /*
    Annual number of customer-interruptions from this bus
    */
    DSSPM_CAPI_DLL double Bus_Get_Cust_Interrupts(void);
    
    /*
    Total numbers of customers served downline from this bus
    */
    DSSPM_CAPI_DLL int32_t Bus_Get_N_Customers(void);
    
    /*
    Number of interruptions this bus per year
    */
    DSSPM_CAPI_DLL double Bus_Get_N_interrupts(void);
    
    /*
    Returns Complex array of pu L-L voltages for 2- and 3-phase buses. Returns -1.0 for 1-phase bus. If more than 3 phases, returns only 3 phases.
    */
    DSSPM_CAPI_DLL void Bus_Get_puVLL(double** ResultPtr, int32_t* ResultCount);
    
    /*
    For 2- and 3-phase buses, returns array of complex numbers represetin L-L voltages in volts. Returns -1.0 for 1-phase bus. If more than 3 phases, returns only first 3.
    */
    DSSPM_CAPI_DLL void Bus_Get_VLL(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Array of doubles containig voltage magnitude, angle pairs in per unit
    */
    DSSPM_CAPI_DLL void Bus_Get_puVmagAngle(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Variant Array of doubles containing voltages in Magnitude (VLN), angle (deg) 
    */
    DSSPM_CAPI_DLL void Bus_Get_VMagAngle(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Total length of line downline from this bus, in miles. For recloser siting algorithm.
    */
    DSSPM_CAPI_DLL double Bus_Get_TotalMiles(void);
    
    /*
    Integer ID of the feeder section in which this bus is located.
    */
    DSSPM_CAPI_DLL int32_t Bus_Get_SectionID(void);
    
    /*
    Array of strings with all Capacitor names in the circuit.
    */
    DSSPM_CAPI_DLL void Capacitors_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Sets the first Capacitor active. Returns 0 if no more.
    */
    DSSPM_CAPI_DLL int32_t Capacitors_Get_First(void);
    
    /*
    Delta connection or wye?\x01) for distributing and switching the total bank kVAR.
    */
    DSSPM_CAPI_DLL uint16_t Capacitors_Get_IsDelta(void);
    
    /*
    Bank kV rating. Use LL for 2 or 3 phases, or actual can rating for 1 phase.
    */
    DSSPM_CAPI_DLL double Capacitors_Get_kV(void);
    
    /*
    Total bank KVAR, distributed equally among phases and steps.
    */
    DSSPM_CAPI_DLL double Capacitors_Get_kvar(void);
    
    /*
    Sets the acitve Capacitor by Name.
    */
    DSSPM_CAPI_DLL char* Capacitors_Get_Name(void);
    
    /*
    Sets the next Capacitor active. Returns 0 if no more.
    */
    DSSPM_CAPI_DLL int32_t Capacitors_Get_Next(void);
    
    /*
    Number of steps (default 1) for distributing and switching the total bank kVAR.
    */
    DSSPM_CAPI_DLL int32_t Capacitors_Get_NumSteps(void);
    
    /*
    Delta connection or wye?\x01) for distributing and switching the total bank kVAR.
    */
    DSSPM_CAPI_DLL void Capacitors_Set_IsDelta(uint16_t Value);
    
    /*
    Bank kV rating. Use LL for 2 or 3 phases, or actual can rating for 1 phase.
    */
    DSSPM_CAPI_DLL void Capacitors_Set_kV(double Value);
    
    /*
    Total bank KVAR, distributed equally among phases and steps.
    */
    DSSPM_CAPI_DLL void Capacitors_Set_kvar(double Value);
    
    /*
    Sets the acitve Capacitor by Name.
    */
    DSSPM_CAPI_DLL void Capacitors_Set_Name(char* Value);
    
    /*
    Number of steps (default 1) for distributing and switching the total bank kVAR.
    */
    DSSPM_CAPI_DLL void Capacitors_Set_NumSteps(int32_t Value);
    
    /*
    Number of Capacitor objects in active circuit.
    */
    DSSPM_CAPI_DLL int32_t Capacitors_Get_Count(void);
    
    DSSPM_CAPI_DLL uint16_t Capacitors_AddStep(void);
    
    DSSPM_CAPI_DLL uint16_t Capacitors_SubtractStep(void);
    
    /*
    Number of Steps available in cap bank to be switched ON.
    */
    DSSPM_CAPI_DLL int32_t Capacitors_Get_AvailableSteps(void);
    
    /*
    A array of  integer [0..numsteps-1] indicating state of each step. If value is -1 an error has occurred.
    */
    DSSPM_CAPI_DLL void Capacitors_Get_States(int32_t** ResultPtr, int32_t* ResultCount);
    
    /*
    Array of integer [0 ..numSteps-1] indicating the state of each step
    */
    DSSPM_CAPI_DLL void Capacitors_Set_States(int32_t* ValuePtr, int32_t ValueCount);
    
    DSSPM_CAPI_DLL void Capacitors_Open(void);
    
    DSSPM_CAPI_DLL void Capacitors_Close(void);
    
    /*
    Array of strings with all CapControl names.
    */
    DSSPM_CAPI_DLL void CapControls_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Name of the Capacitor that is controlled.
    */
    DSSPM_CAPI_DLL char* CapControls_Get_Capacitor(void);
    
    /*
    Transducer ratio from pirmary current to control current.
    */
    DSSPM_CAPI_DLL double CapControls_Get_CTratio(void);
    
    DSSPM_CAPI_DLL double CapControls_Get_DeadTime(void);
    
    /*
    Time delay [s] to switch on after arming.  Control may reset before actually switching.
    */
    DSSPM_CAPI_DLL double CapControls_Get_Delay(void);
    
    /*
    Time delay [s] before swithcing off a step. Control may reset before actually switching.
    */
    DSSPM_CAPI_DLL double CapControls_Get_DelayOff(void);
    
    /*
    Sets the first CapControl as active. Return 0 if none.
    */
    DSSPM_CAPI_DLL int32_t CapControls_Get_First(void);
    
    /*
    Type of automatic controller.
    */
    DSSPM_CAPI_DLL int32_t CapControls_Get_Mode(void);
    
    /*
    Full name of the element that PT and CT are connected to.
    */
    DSSPM_CAPI_DLL char* CapControls_Get_MonitoredObj(void);
    
    /*
    Terminal number on the element that PT and CT are connected to.
    */
    DSSPM_CAPI_DLL int32_t CapControls_Get_MonitoredTerm(void);
    
    /*
    Sets a CapControl active by name.
    */
    DSSPM_CAPI_DLL char* CapControls_Get_Name(void);
    
    /*
    Gets the next CapControl in the circut. Returns 0 if none.
    */
    DSSPM_CAPI_DLL int32_t CapControls_Get_Next(void);
    
    /*
    Threshold to switch off a step. See Mode for units.
    */
    DSSPM_CAPI_DLL double CapControls_Get_OFFSetting(void);
    
    /*
    Threshold to arm or switch on a step.  See Mode for units.
    */
    DSSPM_CAPI_DLL double CapControls_Get_ONSetting(void);
    
    /*
    Transducer ratio from primary feeder to control voltage.
    */
    DSSPM_CAPI_DLL double CapControls_Get_PTratio(void);
    
    /*
    Enables Vmin and Vmax to override the control Mode
    */
    DSSPM_CAPI_DLL uint16_t CapControls_Get_UseVoltOverride(void);
    
    /*
    With VoltOverride, swtich off whenever PT voltage exceeds this level.
    */
    DSSPM_CAPI_DLL double CapControls_Get_Vmax(void);
    
    /*
    With VoltOverride, switch ON whenever PT voltage drops below this level.
    */
    DSSPM_CAPI_DLL double CapControls_Get_Vmin(void);
    
    /*
    Name of the Capacitor that is controlled.
    */
    DSSPM_CAPI_DLL void CapControls_Set_Capacitor(char* Value);
    
    /*
    Transducer ratio from pirmary current to control current.
    */
    DSSPM_CAPI_DLL void CapControls_Set_CTratio(double Value);
    
    DSSPM_CAPI_DLL void CapControls_Set_DeadTime(double Value);
    
    /*
    Time delay [s] to switch on after arming.  Control may reset before actually switching.
    */
    DSSPM_CAPI_DLL void CapControls_Set_Delay(double Value);
    
    /*
    Time delay [s] before swithcing off a step. Control may reset before actually switching.
    */
    DSSPM_CAPI_DLL void CapControls_Set_DelayOff(double Value);
    
    /*
    Type of automatic controller.
    */
    DSSPM_CAPI_DLL void CapControls_Set_Mode(int32_t Value);
    
    /*
    Full name of the element that PT and CT are connected to.
    */
    DSSPM_CAPI_DLL void CapControls_Set_MonitoredObj(char* Value);
    
    /*
    Terminal number on the element that PT and CT are connected to.
    */
    DSSPM_CAPI_DLL void CapControls_Set_MonitoredTerm(int32_t Value);
    
    /*
    Sets a CapControl active by name.
    */
    DSSPM_CAPI_DLL void CapControls_Set_Name(char* Value);
    
    /*
    Threshold to switch off a step. See Mode for units.
    */
    DSSPM_CAPI_DLL void CapControls_Set_OFFSetting(double Value);
    
    /*
    Threshold to arm or switch on a step.  See Mode for units.
    */
    DSSPM_CAPI_DLL void CapControls_Set_ONSetting(double Value);
    
    /*
    Transducer ratio from primary feeder to control voltage.
    */
    DSSPM_CAPI_DLL void CapControls_Set_PTratio(double Value);
    
    /*
    Enables Vmin and Vmax to override the control Mode
    */
    DSSPM_CAPI_DLL void CapControls_Set_UseVoltOverride(uint16_t Value);
    
    /*
    With VoltOverride, swtich off whenever PT voltage exceeds this level.
    */
    DSSPM_CAPI_DLL void CapControls_Set_Vmax(double Value);
    
    /*
    With VoltOverride, switch ON whenever PT voltage drops below this level.
    */
    DSSPM_CAPI_DLL void CapControls_Set_Vmin(double Value);
    
    /*
    Number of CapControls in Active Circuit
    */
    DSSPM_CAPI_DLL int32_t CapControls_Get_Count(void);
    
    DSSPM_CAPI_DLL void CapControls_Reset(void);
    
    /*
    Name of the active circuit.
    */
    DSSPM_CAPI_DLL char* Circuit_Get_Name(void);
    
    /*
    Total number of Buses in the circuit.
    */
    DSSPM_CAPI_DLL int32_t Circuit_Get_NumBuses(void);
    
    /*
    Number of CktElements in the circuit.
    */
    DSSPM_CAPI_DLL int32_t Circuit_Get_NumCktElements(void);
    
    /*
    Total number of nodes in the circuit.
    */
    DSSPM_CAPI_DLL int32_t Circuit_Get_NumNodes(void);
    
    /*
    Complex total line losses in the circuit
    */
    DSSPM_CAPI_DLL void Circuit_Get_LineLosses(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Total losses in active circuit, complex number (two-element array of double).
    */
    DSSPM_CAPI_DLL void Circuit_Get_Losses(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Array of magnitudes (doubles) of voltages at all buses
    */
    DSSPM_CAPI_DLL void Circuit_Get_AllBusVmag(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Complex array of all bus, node voltages from most recent solution
    */
    DSSPM_CAPI_DLL void Circuit_Get_AllBusVolts(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Array of strings containing Full Name of all elements.
    */
    DSSPM_CAPI_DLL void Circuit_Get_AllElementNames(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Complex losses in all transformers designated to substations.
    */
    DSSPM_CAPI_DLL void Circuit_Get_SubstationLosses(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Total power, watts delivered to the circuit
    */
    DSSPM_CAPI_DLL void Circuit_Get_TotalPower(double** ResultPtr, int32_t* ResultCount);
    
    DSSPM_CAPI_DLL void Circuit_Disable(char* Name);
    
    DSSPM_CAPI_DLL void Circuit_Enable(char* Name);
    
    DSSPM_CAPI_DLL int32_t Circuit_FirstPCElement(void);
    
    DSSPM_CAPI_DLL int32_t Circuit_FirstPDElement(void);
    
    DSSPM_CAPI_DLL int32_t Circuit_NextPCElement(void);
    
    DSSPM_CAPI_DLL int32_t Circuit_NextPDElement(void);
    
    /*
    Array of strings containing names of all buses in circuit (see AllNodeNames).
    */
    DSSPM_CAPI_DLL void Circuit_Get_AllBusNames(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Array of total losses (complex) in each circuit element
    */
    DSSPM_CAPI_DLL void Circuit_Get_AllElementLosses(double** ResultPtr, int32_t* ResultCount);
    
    DSSPM_CAPI_DLL void Circuit_Sample(void);
    
    DSSPM_CAPI_DLL void Circuit_SaveSample(void);
    
    DSSPM_CAPI_DLL int32_t Circuit_SetActiveElement(char* FullName);
    
    DSSPM_CAPI_DLL double Circuit_Capacity(double Start, double Increment);
    
    /*
    Double Array of all bus voltages (each node) magnitudes in Per unit
    */
    DSSPM_CAPI_DLL void Circuit_Get_AllBusVmagPu(double** ResultPtr, int32_t* ResultCount);
    
    DSSPM_CAPI_DLL int32_t Circuit_SetActiveBus(char* BusName);
    
    DSSPM_CAPI_DLL int32_t Circuit_SetActiveBusi(int32_t BusIndex);
    
    /*
    Array of strings containing full name of each node in system in same order as returned by AllBusVolts, etc.
    */
    DSSPM_CAPI_DLL void Circuit_Get_AllNodeNames(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    System Y matrix (after a solution has been performed)
    */
    DSSPM_CAPI_DLL void Circuit_Get_SystemY(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Returns distance from each bus to parent EnergyMeter. Corresponds to sequence in AllBusNames.
    */
    DSSPM_CAPI_DLL void Circuit_Get_AllBusDistances(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Returns an array of distances from parent EnergyMeter for each Node. Corresponds to AllBusVMag sequence.
    */
    DSSPM_CAPI_DLL void Circuit_Get_AllNodeDistances(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Returns an array of doubles representing the distances to parent EnergyMeter. Sequence of array corresponds to other node ByPhase properties.
    */
    DSSPM_CAPI_DLL void Circuit_Get_AllNodeDistancesByPhase(double** ResultPtr, int32_t* ResultCount, int32_t Phase);
    
    /*
    Returns Array of doubles represent voltage magnitudes for nodes on the specified phase.
    */
    DSSPM_CAPI_DLL void Circuit_Get_AllNodeVmagByPhase(double** ResultPtr, int32_t* ResultCount, int32_t Phase);
    
    /*
    Returns array of per unit voltage magnitudes for each node by phase
    */
    DSSPM_CAPI_DLL void Circuit_Get_AllNodeVmagPUByPhase(double** ResultPtr, int32_t* ResultCount, int32_t Phase);
    
    /*
    Return array of strings of the node names for the By Phase criteria. Sequence corresponds to other ByPhase properties.
    */
    DSSPM_CAPI_DLL void Circuit_Get_AllNodeNamesByPhase(char*** ResultPtr, int32_t* ResultCount, int32_t Phase);
    
    DSSPM_CAPI_DLL int32_t Circuit_SetActiveClass(char* ClassName);
    
    DSSPM_CAPI_DLL int32_t Circuit_FirstElement(void);
    
    DSSPM_CAPI_DLL int32_t Circuit_NextElement(void);
    
    DSSPM_CAPI_DLL void Circuit_UpdateStorage(void);
    
    /*
    Sets Parent PD element, if any, to be the active circuit element and returns index>0; Returns 0 if it fails or not applicable.
    */
    DSSPM_CAPI_DLL int32_t Circuit_Get_ParentPDElement(void);
    
    DSSPM_CAPI_DLL void Circuit_EndOfTimeStepUpdate(void);
    
    /*
    Array of strings containing the names of the nodes in the same order as the Y matrix
    */
    DSSPM_CAPI_DLL void Circuit_Get_YNodeOrder(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Array of doubles containing complex injection currents for the present solution. Is is the "I" vector of I=YV
    */
    DSSPM_CAPI_DLL void Circuit_Get_YCurrents(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Complex array of actual node voltages in same order as SystemY matrix.
    */
    DSSPM_CAPI_DLL void Circuit_Get_YNodeVarray(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Array of strings. Get  Bus definitions to which each terminal is connected. 0-based array.
    */
    DSSPM_CAPI_DLL void CktElement_Get_BusNames(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Full Name of Active Circuit Element
    */
    DSSPM_CAPI_DLL char* CktElement_Get_Name(void);
    
    /*
    Number of Conductors per Terminal
    */
    DSSPM_CAPI_DLL int32_t CktElement_Get_NumConductors(void);
    
    /*
    Number of Phases
    */
    DSSPM_CAPI_DLL int32_t CktElement_Get_NumPhases(void);
    
    /*
    Number of Terminals this Circuit Element
    */
    DSSPM_CAPI_DLL int32_t CktElement_Get_NumTerminals(void);
    
    /*
    Array of strings. Set Bus definitions for each terminal is connected.
    */
    DSSPM_CAPI_DLL void CktElement_Set_BusNames(char** ValuePtr, int32_t ValueCount);
    
    /*
    Complex array of currents into each conductor of each terminal
    */
    DSSPM_CAPI_DLL void CktElement_Get_Currents(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Complex array of voltages at terminals
    */
    DSSPM_CAPI_DLL void CktElement_Get_Voltages(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Emergency Ampere Rating for PD elements
    */
    DSSPM_CAPI_DLL double CktElement_Get_EmergAmps(void);
    
    /*
    Boolean indicating that element is currently in the circuit.
    */
    DSSPM_CAPI_DLL uint16_t CktElement_Get_Enabled(void);
    
    /*
    Total losses in the element: two-element complex array
    */
    DSSPM_CAPI_DLL void CktElement_Get_Losses(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Normal ampere rating for PD Elements
    */
    DSSPM_CAPI_DLL double CktElement_Get_NormalAmps(void);
    
    /*
    Complex array of losses by phase
    */
    DSSPM_CAPI_DLL void CktElement_Get_PhaseLosses(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Complex array of powers into each conductor of each terminal
    */
    DSSPM_CAPI_DLL void CktElement_Get_Powers(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Double array of symmetrical component currents into each 3-phase terminal
    */
    DSSPM_CAPI_DLL void CktElement_Get_SeqCurrents(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Double array of sequence powers into each 3-phase teminal
    */
    DSSPM_CAPI_DLL void CktElement_Get_SeqPowers(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Double array of symmetrical component voltages at each 3-phase terminal
    */
    DSSPM_CAPI_DLL void CktElement_Get_SeqVoltages(double** ResultPtr, int32_t* ResultCount);
    
    DSSPM_CAPI_DLL void CktElement_Close(int32_t Term, int32_t Phs);
    
    DSSPM_CAPI_DLL void CktElement_Open(int32_t Term, int32_t Phs);
    
    /*
    Emergency Ampere Rating
    */
    DSSPM_CAPI_DLL void CktElement_Set_EmergAmps(double Value);
    
    /*
    Boolean indicating that element is currently in the circuit.
    */
    DSSPM_CAPI_DLL void CktElement_Set_Enabled(uint16_t Value);
    
    /*
    Normal ampere rating
    */
    DSSPM_CAPI_DLL void CktElement_Set_NormalAmps(double Value);
    
    DSSPM_CAPI_DLL uint16_t CktElement_IsOpen(int32_t Term, int32_t Phs);
    
    /*
    Array containing all property names of the active device.
    */
    DSSPM_CAPI_DLL void CktElement_Get_AllPropertyNames(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Number of Properties this Circuit Element.
    */
    DSSPM_CAPI_DLL int32_t CktElement_Get_NumProperties(void);
    
    /*
    Residual currents for each terminal: (mag, angle)
    */
    DSSPM_CAPI_DLL void CktElement_Get_Residuals(double** ResultPtr, int32_t* ResultCount);
    
    /*
    YPrim matrix, column order, complex numbers (paired)
    */
    DSSPM_CAPI_DLL void CktElement_Get_Yprim(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Display name of the object (not necessarily unique)
    */
    DSSPM_CAPI_DLL char* CktElement_Get_DisplayName(void);
    
    /*
    globally unique identifier for this object
    */
    DSSPM_CAPI_DLL char* CktElement_Get_GUID(void);
    
    /*
    Pointer to this object
    */
    DSSPM_CAPI_DLL int32_t CktElement_Get_Handle(void);
    
    /*
    Display name of the object (not necessarily unique)
    */
    DSSPM_CAPI_DLL void CktElement_Set_DisplayName(char* Value);
    
    /*
    Full name of the i-th controller attached to this element. Ex: str = Controller(2).  See NumControls to determine valid index range
    */
    DSSPM_CAPI_DLL char* CktElement_Get_Controller(int32_t idx);
    
    /*
    Name of the Energy Meter this element is assigned to.
    */
    DSSPM_CAPI_DLL char* CktElement_Get_EnergyMeter(void);
    
    /*
    This element has a CapControl or RegControl attached.
    */
    DSSPM_CAPI_DLL uint16_t CktElement_Get_HasVoltControl(void);
    
    /*
    This element has a SwtControl attached.
    */
    DSSPM_CAPI_DLL uint16_t CktElement_Get_HasSwitchControl(void);
    
    /*
    Complex double array of Sequence Voltage for all terminals of active circuit element.
    */
    DSSPM_CAPI_DLL void CktElement_Get_CplxSeqVoltages(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Complex double array of Sequence Currents for all conductors of all terminals of active circuit element.
    */
    DSSPM_CAPI_DLL void CktElement_Get_CplxSeqCurrents(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Array of strings listing all the published variable names, if a PCElement. Otherwise, null string.
    */
    DSSPM_CAPI_DLL void CktElement_Get_AllVariableNames(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Array of doubles. Values of state variables of active element if PC element.
    */
    DSSPM_CAPI_DLL void CktElement_Get_AllVariableValues(double** ResultPtr, int32_t* ResultCount);
    
    /*
    For PCElement, get the value of a variable by name. If Code>0 Then no variable by this name or not a PCelement.
    */
    DSSPM_CAPI_DLL double CktElement_Get_Variable(char* MyVarName, int32_t Code);
    
    /*
    For PCElement, get the value of a variable by integer index.
    */
    DSSPM_CAPI_DLL double CktElement_Get_Variablei(int32_t Idx, int32_t Code);
    
    /*
    Array of integer containing the node numbers (representing phases, for example) for each conductor of each terminal. 
    */
    DSSPM_CAPI_DLL void CktElement_Get_NodeOrder(int32_t** ResultPtr, int32_t* ResultCount);
    
    /*
    True if a recloser, relay, or fuse controlling this ckt element. OCP = Overcurrent Protection 
    */
    DSSPM_CAPI_DLL uint16_t CktElement_Get_HasOCPDevice(void);
    
    /*
    Number of controls connected to this device. Use to determine valid range for index into Controller array.
    */
    DSSPM_CAPI_DLL int32_t CktElement_Get_NumControls(void);
    
    /*
    Index into Controller list of OCP Device controlling this CktElement
    */
    DSSPM_CAPI_DLL int32_t CktElement_Get_OCPDevIndex(void);
    
    /*
    0=None; 1=Fuse; 2=Recloser; 3=Relay;  Type of OCP controller device
    */
    DSSPM_CAPI_DLL int32_t CktElement_Get_OCPDevType(void);
    
    /*
    Currents in magnitude, angle format as a array of doubles.
    */
    DSSPM_CAPI_DLL void CktElement_Get_CurrentsMagAng(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Voltages at each conductor in magnitude, angle form as array of doubles.
    */
    DSSPM_CAPI_DLL void CktElement_Get_VoltagesMagAng(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Convert real and imaginary doubles to Array of doubles
    */
    DSSPM_CAPI_DLL void CmathLib_Get_cmplx(double** ResultPtr, int32_t* ResultCount, double RealPart, double ImagPart);
    
    /*
    Return abs value of complex number given in real and imag doubles
    */
    DSSPM_CAPI_DLL double CmathLib_Get_cabs(double realpart, double imagpart);
    
    /*
    Returns the angle, in degrees, of a complex number specified as two doubles: Realpart and imagpart.
    */
    DSSPM_CAPI_DLL double CmathLib_Get_cdang(double RealPart, double ImagPart);
    
    /*
    Convert complex number to magnitude and angle, degrees. Returns array of two doubles.
    */
    DSSPM_CAPI_DLL void CmathLib_Get_ctopolardeg(double** ResultPtr, int32_t* ResultCount, double RealPart, double ImagPart);
    
    /*
    Convert magnitude, angle in degrees to a complex number. Returns Array of two doubles.
    */
    DSSPM_CAPI_DLL void CmathLib_Get_pdegtocomplex(double** ResultPtr, int32_t* ResultCount, double magnitude, double angle);
    
    /*
    Multiply two complex numbers: (a1, b1) * (a2, b2). Returns result as a array of two doubles.
    */
    DSSPM_CAPI_DLL void CmathLib_Get_cmul(double** ResultPtr, int32_t* ResultCount, double a1, double b1, double a2, double b2);
    
    /*
    Divide two complex number: (a1, b1)/(a2, b2). Returns array of two doubles representing complex result.
    */
    DSSPM_CAPI_DLL void CmathLib_Get_cdiv(double** ResultPtr, int32_t* ResultCount, double a1, double b1, double a2, double b2);
    
    DSSPM_CAPI_DLL void CtrlQueue_ClearQueue(void);
    
    DSSPM_CAPI_DLL void CtrlQueue_Delete(int32_t ActionHandle);
    
    /*
    Code for the active action. Long integer code to tell the control device what to do
    */
    DSSPM_CAPI_DLL int32_t CtrlQueue_Get_ActionCode(void);
    
    /*
    Handle (User defined) to device that must act on the pending action.
    */
    DSSPM_CAPI_DLL int32_t CtrlQueue_Get_DeviceHandle(void);
    
    /*
    Number of Actions on the current actionlist (that have been popped off the control queue by CheckControlActions)
    */
    DSSPM_CAPI_DLL int32_t CtrlQueue_Get_NumActions(void);
    
    DSSPM_CAPI_DLL void CtrlQueue_Show(void);
    
    DSSPM_CAPI_DLL void CtrlQueue_ClearActions(void);
    
    /*
    Pops next action off the action list and makes it the active action. Returns zero if none.
    */
    DSSPM_CAPI_DLL int32_t CtrlQueue_Get_PopAction(void);
    
    /*
    Set the active action by index
    */
    DSSPM_CAPI_DLL void CtrlQueue_Set_Action(int32_t Param1);
    
    /*
    Number of items on the OpenDSS control Queue
    */
    DSSPM_CAPI_DLL int32_t CtrlQueue_Get_QueueSize(void);
    
    DSSPM_CAPI_DLL void CtrlQueue_DoAllQueue(void);
    
    /*
    Array of strings containing the entire queue in CSV format
    */
    DSSPM_CAPI_DLL void CtrlQueue_Get_Queue(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Number of Circuits currently defined
    */
    DSSPM_CAPI_DLL int32_t DSS_Get_NumCircuits(void);
    
    DSSPM_CAPI_DLL void DSS_ClearAll(void);
    
    /*
    Get version string for the DSS.
    */
    DSSPM_CAPI_DLL char* DSS_Get_Version(void);
    
    DSSPM_CAPI_DLL uint16_t DSS_Start(int32_t code);
    
    /*
    List of DSS intrinsic classes (names of the classes)
    */
    DSSPM_CAPI_DLL void DSS_Get_Classes(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    List of user-defined classes
    */
    DSSPM_CAPI_DLL void DSS_Get_UserClasses(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Number of DSS intrinsic classes
    */
    DSSPM_CAPI_DLL int32_t DSS_Get_NumClasses(void);
    
    /*
    Number of user-defined classes
    */
    DSSPM_CAPI_DLL int32_t DSS_Get_NumUserClasses(void);
    
    /*
    DSS Data File Path.  Default path for reports, etc. from DSS
    */
    DSSPM_CAPI_DLL char* DSS_Get_DataPath(void);
    
    /*
    DSS Data File Path.  Default path for reports, etc. from DSS
    */
    DSSPM_CAPI_DLL void DSS_Set_DataPath(char* Value);
    
    DSSPM_CAPI_DLL void DSS_Reset(void);
    
    /*
    Returns the path name for the default text editor.
    */
    DSSPM_CAPI_DLL char* DSS_Get_DefaultEditor(void);
    
    DSSPM_CAPI_DLL int32_t DSS_SetActiveClass(char* ClassName);
    
    /*
    Array of strings containing the names of all properties for the active DSS object.
    */
    DSSPM_CAPI_DLL void DSSElement_Get_AllPropertyNames(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Full Name of Active DSS Object (general element or circuit element).
    */
    DSSPM_CAPI_DLL char* DSSElement_Get_Name(void);
    
    /*
    Number of Properties for the active DSS object.
    */
    DSSPM_CAPI_DLL int32_t DSSElement_Get_NumProperties(void);
    
    DSSPM_CAPI_DLL void DSSimComs_BusVoltagepu(double** ResultPtr, int32_t* ResultCount, size_t Index);
    
    DSSPM_CAPI_DLL void DSSimComs_BusVoltage(double** ResultPtr, int32_t* ResultCount, size_t Index);
    
    DSSPM_CAPI_DLL void DSSProgress_Close(void);
    
    /*
    Caption to appear on the bottom of the DSS Progress form.
    */
    DSSPM_CAPI_DLL void DSSProgress_Set_Caption(char* Value);
    
    /*
    Percent progress to indicate [0..100]
    */
    DSSPM_CAPI_DLL void DSSProgress_Set_PctProgress(int32_t Value);
    
    DSSPM_CAPI_DLL void DSSProgress_Show(void);
    
    /*
    Description of the property.
    */
    DSSPM_CAPI_DLL char* DSSProperty_Get_Description(void);
    
    /*
    Name of Property
    */
    DSSPM_CAPI_DLL char* DSSProperty_Get_Name(void);
    
    DSSPM_CAPI_DLL char* DSSProperty_Get_Val(void);
    
    DSSPM_CAPI_DLL void DSSProperty_Set_Val(char* Value);
    DSSPM_CAPI_DLL void DSSProperty_Set_Name(char* Value);
    DSSPM_CAPI_DLL void DSSProperty_Set_Index(int32_t Value);
    
    /*
    Get i-th command
    */
    DSSPM_CAPI_DLL char* DSS_Executive_Get_Command(int32_t i);
    
    /*
    Number of DSS Executive Commands
    */
    DSSPM_CAPI_DLL int32_t DSS_Executive_Get_NumCommands(void);
    
    /*
    Number of DSS Executive Options
    */
    DSSPM_CAPI_DLL int32_t DSS_Executive_Get_NumOptions(void);
    
    /*
    Get i-th option
    */
    DSSPM_CAPI_DLL char* DSS_Executive_Get_Option(int32_t i);
    
    /*
    Get help string for i-th command
    */
    DSSPM_CAPI_DLL char* DSS_Executive_Get_CommandHelp(int32_t i);
    
    /*
    Get help string for i-th option
    */
    DSSPM_CAPI_DLL char* DSS_Executive_Get_OptionHelp(int32_t i);
    
    /*
    Get present value of i-th option
    */
    DSSPM_CAPI_DLL char* DSS_Executive_Get_OptionValue(int32_t i);
    
    /*
    Description of error for last operation
    */
    DSSPM_CAPI_DLL char* Error_Get_Description(void);
    
    /*
    Error Number
    */
    DSSPM_CAPI_DLL int32_t Error_Get_Number(void);
    
    /*
    Array of strings containing names of all Fuses in the circuit
    */
    DSSPM_CAPI_DLL void Fuses_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Number of Fuse elements in the circuit
    */
    DSSPM_CAPI_DLL int32_t Fuses_Get_Count(void);
    
    /*
    Set the first Fuse to be the active fuse. Returns 0 if none.
    */
    DSSPM_CAPI_DLL int32_t Fuses_Get_First(void);
    
    /*
    Get the name of the active Fuse element
    */
    DSSPM_CAPI_DLL char* Fuses_Get_Name(void);
    
    /*
    Advance the active Fuse element pointer to the next fuse. Returns 0 if no more fuses.
    */
    DSSPM_CAPI_DLL int32_t Fuses_Get_Next(void);
    
    /*
    Set the active Fuse element by name.
    */
    DSSPM_CAPI_DLL void Fuses_Set_Name(char* Value);
    
    /*
    Full name of the circuit element to which the fuse is connected.
    */
    DSSPM_CAPI_DLL char* Fuses_Get_MonitoredObj(void);
    
    /*
    Terminal number to which the fuse is connected.
    */
    DSSPM_CAPI_DLL int32_t Fuses_Get_MonitoredTerm(void);
    
    /*
    Full name of the circuit element switch that the fuse controls. Defaults to the MonitoredObj.
    */
    DSSPM_CAPI_DLL char* Fuses_Get_SwitchedObj(void);
    
    /*
    Full name of the circuit element to which the fuse is connected.
    */
    DSSPM_CAPI_DLL void Fuses_Set_MonitoredObj(char* Value);
    
    /*
    Number of the terminal to which the fuse is connected
    */
    DSSPM_CAPI_DLL void Fuses_Set_MonitoredTerm(int32_t Value);
    
    /*
    Full name of the circuit element switch that the fuse controls. Defaults to MonitoredObj.
    */
    DSSPM_CAPI_DLL void Fuses_Set_SwitchedObj(char* Value);
    
    /*
    Number of the terminal containing the switch controlled by the fuse.
    */
    DSSPM_CAPI_DLL int32_t Fuses_Get_SwitchedTerm(void);
    
    /*
    Number of the terminal of the controlled element containing the switch controlled by the fuse.
    */
    DSSPM_CAPI_DLL void Fuses_Set_SwitchedTerm(int32_t Value);
    
    /*
    Name of the TCCcurve object that determines fuse blowing.
    */
    DSSPM_CAPI_DLL char* Fuses_Get_TCCcurve(void);
    
    /*
    Name of the TCCcurve object that determines fuse blowing.
    */
    DSSPM_CAPI_DLL void Fuses_Set_TCCcurve(char* Value);
    
    /*
    Multiplier or actual amps for the TCCcurve object. Defaults to 1.0.  Multipliy current values of TCC curve by this to get actual amps.
    */
    DSSPM_CAPI_DLL double Fuses_Get_RatedCurrent(void);
    
    /*
    Multiplier or actual fuse amps for the TCC curve. Defaults to 1.0. Has to correspond to the Current axis of TCCcurve object.
    */
    DSSPM_CAPI_DLL void Fuses_Set_RatedCurrent(double Value);
    
    /*
    A fixed delay time in seconds added to the fuse blowing time determined by the TCC curve. Default is 0.
    */
    DSSPM_CAPI_DLL double Fuses_Get_Delay(void);
    
    DSSPM_CAPI_DLL void Fuses_Open(void);
    
    DSSPM_CAPI_DLL void Fuses_Close(void);
    
    /*
    Fixed delay time in seconds added to the fuse blowing time to represent fuse clear or other delay.
    */
    DSSPM_CAPI_DLL void Fuses_Set_Delay(double Value);
    
    /*
    Get/set active fuse by index into the list of fuses. 1 based: 1..count
    */
    DSSPM_CAPI_DLL int32_t Fuses_Get_idx(void);
    
    /*
    Set Fuse active by index into the list of fuses. 1..count
    */
    DSSPM_CAPI_DLL void Fuses_Set_idx(int32_t Value);
    
    /*
    Number of phases, this fuse. 
    */
    DSSPM_CAPI_DLL int32_t Fuses_Get_NumPhases(void);
    
    /*
    Array of names of all Generator objects.
    */
    DSSPM_CAPI_DLL void Generators_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Sets first Generator to be active.  Returns 0 if none.
    */
    DSSPM_CAPI_DLL int32_t Generators_Get_First(void);
    
    /*
    Sets a generator active by name.
    */
    DSSPM_CAPI_DLL char* Generators_Get_Name(void);
    
    /*
    Sets next Generator to be active.  Returns 0 if no more.
    */
    DSSPM_CAPI_DLL int32_t Generators_Get_Next(void);
    
    /*
    Array of Names of all generator energy meter registers
    */
    DSSPM_CAPI_DLL void Generators_Get_RegisterNames(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Array of valus in generator energy meter registers.
    */
    DSSPM_CAPI_DLL void Generators_Get_RegisterValues(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Indicates whether the generator is forced ON regardles of other dispatch criteria.
    */
    DSSPM_CAPI_DLL uint16_t Generators_Get_ForcedON(void);
    
    /*
    Indicates whether the generator is forced ON regardles of other dispatch criteria.
    */
    DSSPM_CAPI_DLL void Generators_Set_ForcedON(uint16_t Value);
    
    /*
    Sets a generator active by name.
    */
    DSSPM_CAPI_DLL void Generators_Set_Name(char* Value);
    
    /*
    Voltage base for the active generator, kV
    */
    DSSPM_CAPI_DLL double Generators_Get_kV(void);
    
    /*
    kvar output for the active generator. Updates power factor based on present kW value.
    */
    DSSPM_CAPI_DLL double Generators_Get_kvar(void);
    
    /*
    kW output for the active generator. kvar is updated for current power factor.
    */
    DSSPM_CAPI_DLL double Generators_Get_kW(void);
    
    /*
    Power factor (pos. = producing vars). Updates kvar based on present kW value.
    */
    DSSPM_CAPI_DLL double Generators_Get_PF(void);
    
    /*
    Number of phases
    */
    DSSPM_CAPI_DLL int32_t Generators_Get_Phases(void);
    
    /*
    Voltage base for the active generator, kV
    */
    DSSPM_CAPI_DLL void Generators_Set_kV(double Value);
    
    /*
    kvar output for the active generator. Updates power factor based on present kW.
    */
    DSSPM_CAPI_DLL void Generators_Set_kvar(double Value);
    
    /*
    kW output for the active generator. kvar is updated for current power factor
    */
    DSSPM_CAPI_DLL void Generators_Set_kW(double Value);
    
    /*
    Power factor (pos. = producing vars). Updates kvar based on present kW value.
    */
    DSSPM_CAPI_DLL void Generators_Set_PF(double Value);
    
    /*
    Number of phases
    */
    DSSPM_CAPI_DLL void Generators_Set_Phases(int32_t Value);
    
    /*
    Number of Generator Objects in Active Circuit
    */
    DSSPM_CAPI_DLL int32_t Generators_Get_Count(void);
    
    /*
    Get/Set active Generator by index into generators list.  1..Count
    */
    DSSPM_CAPI_DLL int32_t Generators_Get_idx(void);
    
    /*
    Get/Set active Generator by index into generators list. 1..Count
    */
    DSSPM_CAPI_DLL void Generators_Set_idx(int32_t Value);
    
    /*
    Generator Model
    */
    DSSPM_CAPI_DLL int32_t Generators_Get_Model(void);
    
    /*
    Generator Model
    */
    DSSPM_CAPI_DLL void Generators_Set_Model(int32_t Value);
    
    /*
    kVA rating of the generator
    */
    DSSPM_CAPI_DLL double Generators_Get_kVArated(void);
    
    /*
    KVA Rating of the generator
    */
    DSSPM_CAPI_DLL void Generators_Set_kVArated(double Value);
    
    /*
    vmaxpu for Generator model
    */
    DSSPM_CAPI_DLL double Generators_Get_Vmaxpu(void);
    
    /*
    Vminpu for Generator model
    */
    DSSPM_CAPI_DLL double Generators_Get_Vminpu(void);
    
    /*
    Vmaxpu for generator model
    */
    DSSPM_CAPI_DLL void Generators_Set_Vmaxpu(double Value);
    
    /*
    Vminpu for Generator model
    */
    DSSPM_CAPI_DLL void Generators_Set_Vminpu(double Value);
    
    /*
    Array of strings containing names of all ISOURCE elements.
    */
    DSSPM_CAPI_DLL void ISources_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Count: Number of ISOURCE elements.
    */
    DSSPM_CAPI_DLL int32_t ISources_Get_Count(void);
    
    /*
    Set the First ISOURCE to be active; returns Zero if none.
    */
    DSSPM_CAPI_DLL int32_t ISources_Get_First(void);
    
    /*
    Sets the next ISOURCE element to be the active one. Returns Zero if no more.
    */
    DSSPM_CAPI_DLL int32_t ISources_Get_Next(void);
    
    /*
    Get name of active ISOURCE
    */
    DSSPM_CAPI_DLL char* ISources_Get_Name(void);
    
    /*
    Set Active ISOURCE by name
    */
    DSSPM_CAPI_DLL void ISources_Set_Name(char* Value);
    
    /*
    Get the magnitude of the ISOURCE in amps
    */
    DSSPM_CAPI_DLL double ISources_Get_Amps(void);
    
    /*
    Set the magnitude of the ISOURCE, amps
    */
    DSSPM_CAPI_DLL void ISources_Set_Amps(double Value);
    
    /*
    Phase angle for ISOURCE, degrees
    */
    DSSPM_CAPI_DLL double ISources_Get_AngleDeg(void);
    
    /*
    The present frequency of the ISOURCE, Hz
    */
    DSSPM_CAPI_DLL double ISources_Get_Frequency(void);
    
    /*
    Phase angle for ISOURCE, degrees
    */
    DSSPM_CAPI_DLL void ISources_Set_AngleDeg(double Value);
    
    /*
    Set the present frequency for the ISOURCE
    */
    DSSPM_CAPI_DLL void ISources_Set_Frequency(double Value);
    
    DSSPM_CAPI_DLL int32_t LineCodes_Get_Count(void);
    
    DSSPM_CAPI_DLL int32_t LineCodes_Get_First(void);
    
    DSSPM_CAPI_DLL int32_t LineCodes_Get_Next(void);
    
    DSSPM_CAPI_DLL char* LineCodes_Get_Name(void);
    
    DSSPM_CAPI_DLL void LineCodes_Set_Name(char* Value);
    
    DSSPM_CAPI_DLL uint16_t LineCodes_Get_IsZ1Z0(void);
    
    DSSPM_CAPI_DLL int32_t LineCodes_Get_Units(void);
    
    DSSPM_CAPI_DLL void LineCodes_Set_Units(int32_t Value);
    
    DSSPM_CAPI_DLL int32_t LineCodes_Get_Phases(void);
    
    DSSPM_CAPI_DLL void LineCodes_Set_Phases(int32_t Value);
    
    DSSPM_CAPI_DLL double LineCodes_Get_R1(void);
    
    DSSPM_CAPI_DLL void LineCodes_Set_R1(double Value);
    
    DSSPM_CAPI_DLL double LineCodes_Get_X1(void);
    
    DSSPM_CAPI_DLL void LineCodes_Set_X1(double Value);
    
    DSSPM_CAPI_DLL double LineCodes_Get_R0(void);
    
    DSSPM_CAPI_DLL double LineCodes_Get_X0(void);
    
    DSSPM_CAPI_DLL void LineCodes_Set_R0(double Value);
    
    DSSPM_CAPI_DLL void LineCodes_Set_X0(double Value);
    
    DSSPM_CAPI_DLL double LineCodes_Get_C0(void);
    
    DSSPM_CAPI_DLL double LineCodes_Get_C1(void);
    
    DSSPM_CAPI_DLL void LineCodes_Set_C0(double Value);
    
    DSSPM_CAPI_DLL void LineCodes_Set_C1(double Value);
    
    DSSPM_CAPI_DLL void LineCodes_Get_Cmatrix(double** ResultPtr, int32_t* ResultCount);
    
    DSSPM_CAPI_DLL void LineCodes_Get_Rmatrix(double** ResultPtr, int32_t* ResultCount);
    
    DSSPM_CAPI_DLL void LineCodes_Get_Xmatrix(double** ResultPtr, int32_t* ResultCount);
    
    DSSPM_CAPI_DLL void LineCodes_Set_Cmatrix(double* ValuePtr, int32_t ValueCount);
    
    DSSPM_CAPI_DLL void LineCodes_Set_Rmatrix(double* ValuePtr, int32_t ValueCount);
    
    DSSPM_CAPI_DLL void LineCodes_Set_Xmatrix(double* ValuePtr, int32_t ValueCount);
    
    DSSPM_CAPI_DLL double LineCodes_Get_NormAmps(void);
    
    DSSPM_CAPI_DLL void LineCodes_Set_NormAmps(double Value);
    
    DSSPM_CAPI_DLL double LineCodes_Get_EmergAmps(void);
    
    DSSPM_CAPI_DLL void LineCodes_Set_EmergAmps(double Value);
    
    DSSPM_CAPI_DLL void LineCodes_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Names of all Line Objects
    */
    DSSPM_CAPI_DLL void Lines_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Name of bus for terminal 1.
    */
    DSSPM_CAPI_DLL char* Lines_Get_Bus1(void);
    
    /*
    Name of bus for terminal 2.
    */
    DSSPM_CAPI_DLL char* Lines_Get_Bus2(void);
    
    /*
    Invoking this property sets the first element active.  Returns 0 if no lines.  Otherwise, index of the line element.
    */
    DSSPM_CAPI_DLL int32_t Lines_Get_First(void);
    
    /*
    Length of line section in units compatible with the LineCode definition.
    */
    DSSPM_CAPI_DLL double Lines_Get_Length(void);
    
    /*
    Name of LineCode object that defines the impedances.
    */
    DSSPM_CAPI_DLL char* Lines_Get_LineCode(void);
    
    /*
    Specify the name of the Line element to set it active.
    */
    DSSPM_CAPI_DLL char* Lines_Get_Name(void);
    
    /*
    Invoking this property advances to the next Line element active.  Returns 0 if no more lines.  Otherwise, index of the line element.
    */
    DSSPM_CAPI_DLL int32_t Lines_Get_Next(void);
    
    /*
    Number of Phases, this Line element.
    */
    DSSPM_CAPI_DLL int32_t Lines_Get_Phases(void);
    
    /*
    Positive Sequence resistance, ohms per unit length.
    */
    DSSPM_CAPI_DLL double Lines_Get_R1(void);
    
    /*
    Positive Sequence reactance, ohms per unit length.
    */
    DSSPM_CAPI_DLL double Lines_Get_X1(void);
    
    DSSPM_CAPI_DLL int32_t Lines_New(char* Name);
    
    /*
    Name of bus for terminal 1.
    */
    DSSPM_CAPI_DLL void Lines_Set_Bus1(char* Value);
    
    /*
    Name of bus for terminal 2.
    */
    DSSPM_CAPI_DLL void Lines_Set_Bus2(char* Value);
    
    /*
    Length of line section in units compatible with the LineCode definition.
    */
    DSSPM_CAPI_DLL void Lines_Set_Length(double Value);
    
    /*
    Name of LineCode object that defines the impedances.
    */
    DSSPM_CAPI_DLL void Lines_Set_LineCode(char* Value);
    
    /*
    Specify the name of the Line element to set it active.
    */
    DSSPM_CAPI_DLL void Lines_Set_Name(char* Value);
    
    /*
    Number of Phases, this Line element.
    */
    DSSPM_CAPI_DLL void Lines_Set_Phases(int32_t Value);
    
    /*
    Positive Sequence resistance, ohms per unit length.
    */
    DSSPM_CAPI_DLL void Lines_Set_R1(double Value);
    
    /*
    Positive Sequence reactance, ohms per unit length.
    */
    DSSPM_CAPI_DLL void Lines_Set_X1(double Value);
    
    /*
    Zero Sequence capacitance, nanofarads per unit length.
    */
    DSSPM_CAPI_DLL double Lines_Get_C0(void);
    
    /*
    Positive Sequence capacitance, nanofarads per unit length.
    */
    DSSPM_CAPI_DLL double Lines_Get_C1(void);
    
    DSSPM_CAPI_DLL void Lines_Get_Cmatrix(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Zero Sequence resistance, ohms per unit length.
    */
    DSSPM_CAPI_DLL double Lines_Get_R0(void);
    
    /*
    Resistance matrix (full), ohms per unit length. Array of doubles.
    */
    DSSPM_CAPI_DLL void Lines_Get_Rmatrix(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Zero Sequence reactance ohms per unit length.
    */
    DSSPM_CAPI_DLL double Lines_Get_X0(void);
    
    DSSPM_CAPI_DLL void Lines_Get_Xmatrix(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Zero Sequence capacitance, nanofarads per unit length.
    */
    DSSPM_CAPI_DLL void Lines_Set_C0(double Value);
    
    /*
    Positive Sequence capacitance, nanofarads per unit length.
    */
    DSSPM_CAPI_DLL void Lines_Set_C1(double Value);
    
    DSSPM_CAPI_DLL void Lines_Set_Cmatrix(double* ValuePtr, int32_t ValueCount);
    
    /*
    Zero Sequence resistance, ohms per unit length.
    */
    DSSPM_CAPI_DLL void Lines_Set_R0(double Value);
    
    /*
    Resistance matrix (full), ohms per unit length. Array of doubles.
    */
    DSSPM_CAPI_DLL void Lines_Set_Rmatrix(double* ValuePtr, int32_t ValueCount);
    
    /*
    Zero Sequence reactance ohms per unit length.
    */
    DSSPM_CAPI_DLL void Lines_Set_X0(double Value);
    
    DSSPM_CAPI_DLL void Lines_Set_Xmatrix(double* ValuePtr, int32_t ValueCount);
    
    /*
    Emergency (maximum) ampere rating of Line.
    */
    DSSPM_CAPI_DLL double Lines_Get_EmergAmps(void);
    
    /*
    Normal ampere rating of Line.
    */
    DSSPM_CAPI_DLL double Lines_Get_NormAmps(void);
    
    /*
    Emergency (maximum) ampere rating of Line.
    */
    DSSPM_CAPI_DLL void Lines_Set_EmergAmps(double Value);
    
    /*
    Normal ampere rating of Line.
    */
    DSSPM_CAPI_DLL void Lines_Set_NormAmps(double Value);
    
    /*
    Line geometry code
    */
    DSSPM_CAPI_DLL char* Lines_Get_Geometry(void);
    
    /*
    Line geometry code
    */
    DSSPM_CAPI_DLL void Lines_Set_Geometry(char* Value);
    
    /*
    Earth return resistance value used to compute line impedances at power frequency
    */
    DSSPM_CAPI_DLL double Lines_Get_Rg(void);
    
    /*
    Earth Resistivity, m-ohms
    */
    DSSPM_CAPI_DLL double Lines_Get_Rho(void);
    
    /*
    Earth return reactance value used to compute line impedances at power frequency
    */
    DSSPM_CAPI_DLL double Lines_Get_Xg(void);
    
    /*
    Earth return resistance value used to compute line impedances at power frequency
    */
    DSSPM_CAPI_DLL void Lines_Set_Rg(double Value);
    
    /*
    Earth Resistivity, m-ohms
    */
    DSSPM_CAPI_DLL void Lines_Set_Rho(double Value);
    
    /*
    Earth return reactance value used to compute line impedances at power frequency
    */
    DSSPM_CAPI_DLL void Lines_Set_Xg(double Value);
    
    /*
    Yprimitive: Does Nothing at present on Put; Dangerous
    */
    DSSPM_CAPI_DLL void Lines_Get_Yprim(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Yprimitive: Does Nothing at present on Put; Dangerous
    */
    DSSPM_CAPI_DLL void Lines_Set_Yprim(double* ValuePtr, int32_t ValueCount);
    
    /*
    Number of customers on this line section.
    */
    DSSPM_CAPI_DLL int32_t Lines_Get_NumCust(void);
    
    /*
    Total Number of customers served from this line section.
    */
    DSSPM_CAPI_DLL int32_t Lines_Get_TotalCust(void);
    
    /*
    Sets Parent of the active Line to be the active line. Returns 0 if no parent or action fails.
    */
    DSSPM_CAPI_DLL int32_t Lines_Get_Parent(void);
    
    /*
    Number of Line objects in Active Circuit.
    */
    DSSPM_CAPI_DLL int32_t Lines_Get_Count(void);
    
    /*
    Line spacing code
    */
    DSSPM_CAPI_DLL char* Lines_Get_Spacing(void);
    
    /*
    Line spacing code
    */
    DSSPM_CAPI_DLL void Lines_Set_Spacing(char* Value);
    
    DSSPM_CAPI_DLL int32_t Lines_Get_Units(void);
    
    DSSPM_CAPI_DLL void Lines_Set_Units(int32_t Value);
    
    /*
    Array of strings containing all Load names
    */
    DSSPM_CAPI_DLL void Loads_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Set first Load element to be active; returns 0 if none.
    */
    DSSPM_CAPI_DLL int32_t Loads_Get_First(void);
    
    DSSPM_CAPI_DLL int32_t Loads_Get_idx(void);
    
    /*
    Set active load by name.
    */
    DSSPM_CAPI_DLL char* Loads_Get_Name(void);
    
    /*
    Sets next Load element to be active; returns 0 of none else index of active load.
    */
    DSSPM_CAPI_DLL int32_t Loads_Get_Next(void);
    
    DSSPM_CAPI_DLL void Loads_Set_idx(int32_t Value);
    
    /*
    Set active load by name.
    */
    DSSPM_CAPI_DLL void Loads_Set_Name(char* Value);
    
    /*
    Set kV rating for active Load. For 2 or more phases set Line-Line kV. Else actual kV across terminals.
    */
    DSSPM_CAPI_DLL double Loads_Get_kV(void);
    
    /*
    Set kvar for active Load. Updates PF based in present kW.
    */
    DSSPM_CAPI_DLL double Loads_Get_kvar(void);
    
    /*
    Set kW for active Load. Updates kvar based on present PF.
    */
    DSSPM_CAPI_DLL double Loads_Get_kW(void);
    
    /*
    Set Power Factor for Active Load. Specify leading PF as negative. Updates kvar based on kW value
    */
    DSSPM_CAPI_DLL double Loads_Get_PF(void);
    
    /*
    Set kV rating for active Load. For 2 or more phases set Line-Line kV. Else actual kV across terminals.
    */
    DSSPM_CAPI_DLL void Loads_Set_kV(double Value);
    
    /*
    Set kvar for active Load. Updates PF based on present kW.
    */
    DSSPM_CAPI_DLL void Loads_Set_kvar(double Value);
    
    /*
    Set kW for active Load. Updates kvar based on present PF.
    */
    DSSPM_CAPI_DLL void Loads_Set_kW(double Value);
    
    /*
    Set Power Factor for Active Load. Specify leading PF as negative. Updates kvar based on present value of kW.
    */
    DSSPM_CAPI_DLL void Loads_Set_PF(double Value);
    
    /*
    Number of Load objects in active circuit.
    */
    DSSPM_CAPI_DLL int32_t Loads_Get_Count(void);
    
    /*
    Factor for allocating loads by connected xfkva
    */
    DSSPM_CAPI_DLL double Loads_Get_AllocationFactor(void);
    
    /*
    Factor relates average to peak kw.  Used for allocation with kwh and kwhdays/
    */
    DSSPM_CAPI_DLL double Loads_Get_Cfactor(void);
    
    DSSPM_CAPI_DLL int32_t Loads_Get_Class_(void);
    
    /*
    Name of a loadshape with both Mult and Qmult, for CVR factors as a function of time.
    */
    DSSPM_CAPI_DLL char* Loads_Get_CVRcurve(void);
    
    /*
    Percent reduction in Q for percent reduction in V. Must be used with dssLoadModelCVR.
    */
    DSSPM_CAPI_DLL double Loads_Get_CVRvars(void);
    
    /*
    Percent reduction in P for percent reduction in V. Must be used with dssLoadModelCVR.
    */
    DSSPM_CAPI_DLL double Loads_Get_CVRwatts(void);
    
    /*
    Name of the loadshape for a daily load profile.
    */
    DSSPM_CAPI_DLL char* Loads_Get_daily(void);
    
    /*
    Name of the loadshape for a duty cycle simulation.
    */
    DSSPM_CAPI_DLL char* Loads_Get_duty(void);
    
    /*
    Name of the growthshape curve for yearly load growth factors.
    */
    DSSPM_CAPI_DLL char* Loads_Get_Growth(void);
    
    /*
    Delta loads are connected line-to-line.
    */
    DSSPM_CAPI_DLL uint16_t Loads_Get_IsDelta(void);
    
    /*
    Base load kva. Also defined kw and kvar or pf input, or load allocation by kwh or xfkva.
    */
    DSSPM_CAPI_DLL double Loads_Get_kva(void);
    
    /*
    kwh billed for this period. Can be used with Cfactor for load allocation.
    */
    DSSPM_CAPI_DLL double Loads_Get_kwh(void);
    
    /*
    Length of kwh billing period for average demand calculation. Default 30.
    */
    DSSPM_CAPI_DLL double Loads_Get_kwhdays(void);
    
    /*
    The Load Model defines variation of P and Q with voltage.
    */
    DSSPM_CAPI_DLL int32_t Loads_Get_Model(void);
    
    /*
    Number of customers in this load, defaults to one.
    */
    DSSPM_CAPI_DLL int32_t Loads_Get_NumCust(void);
    
    /*
    Average percent of nominal load in Monte Carlo studies; only if no loadshape defined for this load.
    */
    DSSPM_CAPI_DLL double Loads_Get_PctMean(void);
    
    /*
    Percent standard deviation for Monte Carlo load studies; if there is no loadshape assigned to this load.
    */
    DSSPM_CAPI_DLL double Loads_Get_PctStdDev(void);
    
    /*
    Neutral resistance for wye-connected loads.
    */
    DSSPM_CAPI_DLL double Loads_Get_Rneut(void);
    
    /*
    Name of harmonic current spectrrum shape.
    */
    DSSPM_CAPI_DLL char* Loads_Get_Spectrum(void);
    
    /*
    Response to load multipliers: Fixed (growth only), Exempt (no LD curve), Variable (all).
    */
    DSSPM_CAPI_DLL int32_t Loads_Get_Status(void);
    
    /*
    Maximum per-unit voltage to use the load model. Above this, constant Z applies.
    */
    DSSPM_CAPI_DLL double Loads_Get_Vmaxpu(void);
    
    /*
    Minimum voltage for unserved energy (UE) evaluation.
    */
    DSSPM_CAPI_DLL double Loads_Get_Vminemerg(void);
    
    /*
    Minimum voltage for energy exceeding normal (EEN) evaluations.
    */
    DSSPM_CAPI_DLL double Loads_Get_Vminnorm(void);
    
    /*
    Minimum voltage to apply the load model. Below this, constant Z is used.
    */
    DSSPM_CAPI_DLL double Loads_Get_Vminpu(void);
    
    /*
    Rated service transformer kVA for load allocation, using AllocationFactor. Affects kW, kvar, and pf.
    */
    DSSPM_CAPI_DLL double Loads_Get_xfkVA(void);
    
    /*
    Neutral reactance for wye-connected loads.
    */
    DSSPM_CAPI_DLL double Loads_Get_Xneut(void);
    
    /*
    Name of yearly duration loadshape
    */
    DSSPM_CAPI_DLL char* Loads_Get_Yearly(void);
    
    DSSPM_CAPI_DLL void Loads_Set_AllocationFactor(double Value);
    
    DSSPM_CAPI_DLL void Loads_Set_Cfactor(double Value);
    
    DSSPM_CAPI_DLL void Loads_Set_Class_(int32_t Value);
    
    DSSPM_CAPI_DLL void Loads_Set_CVRcurve(char* Value);
    
    DSSPM_CAPI_DLL void Loads_Set_CVRvars(double Value);
    
    DSSPM_CAPI_DLL void Loads_Set_CVRwatts(double Value);
    
    DSSPM_CAPI_DLL void Loads_Set_daily(char* Value);
    
    DSSPM_CAPI_DLL void Loads_Set_duty(char* Value);
    
    DSSPM_CAPI_DLL void Loads_Set_Growth(char* Value);
    
    DSSPM_CAPI_DLL void Loads_Set_IsDelta(uint16_t Value);
    
    DSSPM_CAPI_DLL void Loads_Set_kva(double Value);
    
    DSSPM_CAPI_DLL void Loads_Set_kwh(double Value);
    
    DSSPM_CAPI_DLL void Loads_Set_kwhdays(double Value);
    
    DSSPM_CAPI_DLL void Loads_Set_Model(int32_t Value);
    
    DSSPM_CAPI_DLL void Loads_Set_NumCust(int32_t Value);
    
    DSSPM_CAPI_DLL void Loads_Set_PctMean(double Value);
    
    DSSPM_CAPI_DLL void Loads_Set_PctStdDev(double Value);
    
    DSSPM_CAPI_DLL void Loads_Set_Rneut(double Value);
    
    DSSPM_CAPI_DLL void Loads_Set_Spectrum(char* Value);
    
    DSSPM_CAPI_DLL void Loads_Set_Status(int32_t Value);
    
    DSSPM_CAPI_DLL void Loads_Set_Vmaxpu(double Value);
    
    DSSPM_CAPI_DLL void Loads_Set_Vminemerg(double Value);
    
    DSSPM_CAPI_DLL void Loads_Set_Vminnorm(double Value);
    
    DSSPM_CAPI_DLL void Loads_Set_Vminpu(double Value);
    
    DSSPM_CAPI_DLL void Loads_Set_xfkVA(double Value);
    
    DSSPM_CAPI_DLL void Loads_Set_Xneut(double Value);
    
    DSSPM_CAPI_DLL void Loads_Set_Yearly(char* Value);
    
    /*
    Array of 7  doubles with values for ZIPV property of the LOAD object
    */
    DSSPM_CAPI_DLL void Loads_Get_ZIPV(double** ResultPtr, int32_t* ResultCount);
    
    DSSPM_CAPI_DLL void Loads_Set_ZIPV(double* ValuePtr, int32_t ValueCount);
    
    DSSPM_CAPI_DLL double Loads_Get_pctSeriesRL(void);
    
    /*
    Percent of Load that is modeled as series R-L for harmonics studies
    */
    DSSPM_CAPI_DLL void Loads_Set_pctSeriesRL(double Value);
    
    /*
    Relative Weighting factor for the active LOAD
    */
    DSSPM_CAPI_DLL double Loads_Get_RelWeight(void);
    
    /*
    Get the Name of the active Loadshape
    */
    DSSPM_CAPI_DLL char* LoadShapes_Get_Name(void);
    
    /*
    Set the active Loadshape by name
    */
    DSSPM_CAPI_DLL void LoadShapes_Set_Name(char* Value);
    
    /*
    Number of Loadshape objects currently defined in Loadshape collection
    */
    DSSPM_CAPI_DLL int32_t LoadShapes_Get_Count(void);
    
    /*
    Set the first loadshape active and return integer index of the loadshape. Returns 0 if none.
    */
    DSSPM_CAPI_DLL int32_t LoadShapes_Get_First(void);
    
    /*
    Advance active Loadshape to the next on in the collection. Returns 0 if no more loadshapes.
    */
    DSSPM_CAPI_DLL int32_t LoadShapes_Get_Next(void);
    
    /*
    Array of strings containing names of all Loadshape objects currently defined.
    */
    DSSPM_CAPI_DLL void LoadShapes_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Get Number of points in active Loadshape.
    */
    DSSPM_CAPI_DLL int32_t LoadShapes_Get_Npts(void);
    
    /*
    Array of Doubles for the P multiplier in the Loadshape.
    */
    DSSPM_CAPI_DLL void LoadShapes_Get_Pmult(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Array of doubles containing the Q multipliers.
    */
    DSSPM_CAPI_DLL void LoadShapes_Get_Qmult(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Set number of points to allocate for active Loadshape.
    */
    DSSPM_CAPI_DLL void LoadShapes_Set_Npts(int32_t Value);
    
    /*
    Array of doubles containing the P array for the Loadshape.
    */
    DSSPM_CAPI_DLL void LoadShapes_Set_Pmult(double* ValuePtr, int32_t ValueCount);
    
    /*
    Array of doubles containing the Q multipliers.
    */
    DSSPM_CAPI_DLL void LoadShapes_Set_Qmult(double* ValuePtr, int32_t ValueCount);
    
    DSSPM_CAPI_DLL void LoadShapes_Normalize(void);
    
    /*
    Time array in hours correscponding to P and Q multipliers when the Interval=0.
    */
    DSSPM_CAPI_DLL void LoadShapes_Get_TimeArray(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Time array in hours correscponding to P and Q multipliers when the Interval=0.
    */
    DSSPM_CAPI_DLL void LoadShapes_Set_TimeArray(double* ValuePtr, int32_t ValueCount);
    
    /*
    Fixed interval time value, hours
    */
    DSSPM_CAPI_DLL double LoadShapes_Get_HrInterval(void);
    
    /*
    Fixed Interval time value, in minutes
    */
    DSSPM_CAPI_DLL double LoadShapes_Get_MinInterval(void);
    
    DSSPM_CAPI_DLL double LoadShapes_Get_sInterval(void);
    
    /*
    Fixed interval time value, hours.
    */
    DSSPM_CAPI_DLL void LoadShapes_Set_HrInterval(double Value);
    
    /*
    Fixed Interval time value, in minutes
    */
    DSSPM_CAPI_DLL void LoadShapes_Set_MinInterval(double Value);
    
    /*
    Fixed interval data time interval, seconds
    */
    DSSPM_CAPI_DLL void LoadShapes_Set_Sinterval(double Value);
    
    DSSPM_CAPI_DLL double LoadShapes_Get_PBase(void);
    
    /*
    Base for normalizing Q curve. If left at zero, the peak value is used.
    */
    DSSPM_CAPI_DLL double LoadShapes_Get_Qbase(void);
    
    DSSPM_CAPI_DLL void LoadShapes_Set_PBase(double Value);
    
    /*
    Base for normalizing Q curve. If left at zero, the peak value is used.
    */
    DSSPM_CAPI_DLL void LoadShapes_Set_Qbase(double Value);
    
    /*
    T/F flag to let Loads know to use the actual value in the curve rather than use the value as a multiplier.
    */
    DSSPM_CAPI_DLL uint16_t LoadShapes_Get_UseActual(void);
    
    /*
    T/F flag to let Loads know to use the actual value in the curve rather than use the value as a multiplier.
    */
    DSSPM_CAPI_DLL void LoadShapes_Set_UseActual(uint16_t Value);
    
    /*
    Array of all energy Meter names
    */
    DSSPM_CAPI_DLL void Meters_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Set the first energy Meter active. Returns 0 if none.
    */
    DSSPM_CAPI_DLL int32_t Meters_Get_First(void);
    
    /*
    Get/Set the active meter  name.
    */
    DSSPM_CAPI_DLL char* Meters_Get_Name(void);
    
    /*
    Sets the next energy Meter active.  Returns 0 if no more.
    */
    DSSPM_CAPI_DLL int32_t Meters_Get_Next(void);
    
    /*
    Array of strings containing the names of the registers.
    */
    DSSPM_CAPI_DLL void Meters_Get_RegisterNames(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Array of all the values contained in the Meter registers for the active Meter.
    */
    DSSPM_CAPI_DLL void Meters_Get_RegisterValues(double** ResultPtr, int32_t* ResultCount);
    
    DSSPM_CAPI_DLL void Meters_Reset(void);
    
    DSSPM_CAPI_DLL void Meters_ResetAll(void);
    
    DSSPM_CAPI_DLL void Meters_Sample(void);
    
    DSSPM_CAPI_DLL void Meters_Save(void);
    
    /*
    Set a meter to be active by name.
    */
    DSSPM_CAPI_DLL void Meters_Set_Name(char* Value);
    
    /*
    Totals of all registers of all meters
    */
    DSSPM_CAPI_DLL void Meters_Get_Totals(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Array of doubles to set values of Peak Current property
    */
    DSSPM_CAPI_DLL void Meters_Get_Peakcurrent(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Array of doubles to set values of Peak Current property
    */
    DSSPM_CAPI_DLL void Meters_Set_Peakcurrent(double* ValuePtr, int32_t ValueCount);
    
    /*
    Set the magnitude of the real part of the Calculated Current (normally determined by solution) for the Meter to force some behavior on Load Allocation
    */
    DSSPM_CAPI_DLL void Meters_Get_CalcCurrent(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Set the magnitude of the real part of the Calculated Current (normally determined by solution) for the Meter to force some behavior on Load Allocation
    */
    DSSPM_CAPI_DLL void Meters_Set_CalcCurrent(double* ValuePtr, int32_t ValueCount);
    
    /*
    Array of doubles: set the phase allocation factors for the active meter.
    */
    DSSPM_CAPI_DLL void Meters_Get_AllocFactors(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Array of doubles: set the phase allocation factors for the active meter.
    */
    DSSPM_CAPI_DLL void Meters_Set_AllocFactors(double* ValuePtr, int32_t ValueCount);
    
    /*
    Set Name of metered element
    */
    DSSPM_CAPI_DLL char* Meters_Get_MeteredElement(void);
    
    /*
    set Number of Metered Terminal
    */
    DSSPM_CAPI_DLL int32_t Meters_Get_MeteredTerminal(void);
    
    /*
    Set Name of metered element
    */
    DSSPM_CAPI_DLL void Meters_Set_MeteredElement(char* Value);
    
    /*
    set Number of Metered Terminal
    */
    DSSPM_CAPI_DLL void Meters_Set_MeteredTerminal(int32_t Value);
    
    /*
    Global Flag in the DSS to indicate if Demand Interval (DI) files have been properly opened.
    */
    DSSPM_CAPI_DLL uint16_t Meters_Get_DIFilesAreOpen(void);
    
    DSSPM_CAPI_DLL void Meters_CloseAllDIFiles(void);
    
    DSSPM_CAPI_DLL void Meters_OpenAllDIFiles(void);
    
    DSSPM_CAPI_DLL void Meters_SampleAll(void);
    
    DSSPM_CAPI_DLL void Meters_SaveAll(void);
    
    /*
    Array of names of all zone end elements.
    */
    DSSPM_CAPI_DLL void Meters_Get_AllEndElements(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Number of zone end elements in the active meter zone.
    */
    DSSPM_CAPI_DLL int32_t Meters_Get_CountEndElements(void);
    
    /*
    Number of Energy Meters in the Active Circuit
    */
    DSSPM_CAPI_DLL int32_t Meters_Get_Count(void);
    
    /*
    Wide string list of all branches in zone of the active energymeter object.
    */
    DSSPM_CAPI_DLL void Meters_Get_AllBranchesInZone(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Number of branches in Active energymeter zone. (Same as sequencelist size)
    */
    DSSPM_CAPI_DLL int32_t Meters_Get_CountBranches(void);
    
    /*
    Returns SAIFI for this meter's Zone. Execute Reliability Calc method first.
    */
    DSSPM_CAPI_DLL double Meters_Get_SAIFI(void);
    
    /*
    Get/set Index into Meter's SequenceList that contains branch pointers in lexical order. Earlier index guaranteed to be upline from later index. Sets PDelement active.
    */
    DSSPM_CAPI_DLL int32_t Meters_Get_SequenceIndex(void);
    
    /*
    Get/set Index into Meter's SequenceList that contains branch pointers in lexical order. Earlier index guaranteed to be upline from later index. Sets PDelement active.
    */
    DSSPM_CAPI_DLL void Meters_Set_SequenceIndex(int32_t Value);
    
    /*
    SAIFI based on kW rather than number of customers. Get after reliability calcs.
    */
    DSSPM_CAPI_DLL double Meters_Get_SAIFIKW(void);
    
    DSSPM_CAPI_DLL void Meters_DoReliabilityCalc(uint16_t AssumeRestoration);
    
    /*
    Size of Sequence List
    */
    DSSPM_CAPI_DLL int32_t Meters_Get_SeqListSize(void);
    
    /*
    Total Number of customers in this zone (downline from the EnergyMeter)
    */
    DSSPM_CAPI_DLL int32_t Meters_Get_TotalCustomers(void);
    
    /*
    SAIDI for this meter's zone. Execute DoReliabilityCalc first.
    */
    DSSPM_CAPI_DLL double Meters_Get_SAIDI(void);
    
    /*
    Total customer interruptions for this Meter zone based on reliability calcs.
    */
    DSSPM_CAPI_DLL double Meters_Get_CustInterrupts(void);
    
    /*
    Number of feeder sections in this meter's zone
    */
    DSSPM_CAPI_DLL int32_t Meters_Get_NumSections(void);
    
    DSSPM_CAPI_DLL void Meters_SetActiveSection(int32_t SectIdx);
    
    /*
    Average Repair time in this section of the meter zone
    */
    DSSPM_CAPI_DLL double Meters_Get_AvgRepairTime(void);
    
    /*
    Sum of Fault Rate time Repair Hrs in this section of the meter zone
    */
    DSSPM_CAPI_DLL double Meters_Get_FaultRateXRepairHrs(void);
    
    /*
    Number of branches (lines) in this section
    */
    DSSPM_CAPI_DLL int32_t Meters_Get_NumSectionBranches(void);
    
    /*
    Number of Customers in the active section.
    */
    DSSPM_CAPI_DLL int32_t Meters_Get_NumSectionCustomers(void);
    
    /*
    Type of OCP device. 1=Fuse; 2=Recloser; 3=Relay
    */
    DSSPM_CAPI_DLL int32_t Meters_Get_OCPDeviceType(void);
    
    /*
    Sum of the branch fault rates in this section of the meter's zone
    */
    DSSPM_CAPI_DLL double Meters_Get_SumBranchFltRates(void);
    
    /*
    SequenceIndex of the branch at the head of this section
    */
    DSSPM_CAPI_DLL int32_t Meters_Get_SectSeqIdx(void);
    
    /*
    Total Customers downline from this section
    */
    DSSPM_CAPI_DLL int32_t Meters_Get_SectTotalCust(void);
    
    /*
    Array of all Monitor Names
    */
    DSSPM_CAPI_DLL void Monitors_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Name of CSV file associated with active Monitor.
    */
    DSSPM_CAPI_DLL char* Monitors_Get_FileName(void);
    
    /*
    Sets the first Monitor active.  Returns 0 if no monitors.
    */
    DSSPM_CAPI_DLL int32_t Monitors_Get_First(void);
    
    /*
    Set Monitor mode (bitmask integer - see DSS Help)
    */
    DSSPM_CAPI_DLL int32_t Monitors_Get_Mode(void);
    
    /*
    Sets the active Monitor object by name
    */
    DSSPM_CAPI_DLL char* Monitors_Get_Name(void);
    
    /*
    Sets next monitor active.  Returns 0 if no more.
    */
    DSSPM_CAPI_DLL int32_t Monitors_Get_Next(void);
    
    DSSPM_CAPI_DLL void Monitors_Reset(void);
    
    DSSPM_CAPI_DLL void Monitors_ResetAll(void);
    
    DSSPM_CAPI_DLL void Monitors_Sample(void);
    
    DSSPM_CAPI_DLL void Monitors_Save(void);
    
    /*
    Set Monitor mode (bitmask integer - see DSS Help)
    */
    DSSPM_CAPI_DLL void Monitors_Set_Mode(int32_t Value);
    
    DSSPM_CAPI_DLL void Monitors_Show(void);
    
    /*
    Sets the active Monitor object by name
    */
    DSSPM_CAPI_DLL void Monitors_Set_Name(char* Value);
    
    /*
    Byte Array containing monitor stream values. Make sure a "save" is done first (standard solution modes do this automatically)
    */
    DSSPM_CAPI_DLL void Monitors_Get_ByteStream(int8_t** ResultPtr, int32_t* ResultCount);
    
    /*
    Number of Samples in Monitor at Present
    */
    DSSPM_CAPI_DLL int32_t Monitors_Get_SampleCount(void);
    
    DSSPM_CAPI_DLL void Monitors_SampleAll(void);
    
    DSSPM_CAPI_DLL void Monitors_SaveAll(void);
    
    /*
    Number of Monitors
    */
    DSSPM_CAPI_DLL int32_t Monitors_Get_Count(void);
    
    DSSPM_CAPI_DLL void Monitors_Process(void);
    
    DSSPM_CAPI_DLL void Monitors_ProcessAll(void);
    
    /*
    Array of doubles for the specified channel  (usage: MyArray = DSSMonitor.Channel(i)) A Save or SaveAll  should be executed first. Done automatically by most standard solution modes.
    */
    DSSPM_CAPI_DLL void Monitors_Get_Channel(double** ResultPtr, int32_t* ResultCount, int32_t Index);
    
    /*
    Array of doubles containing frequency values for harmonics mode solutions; Empty for time mode solutions (use dblHour)
    */
    DSSPM_CAPI_DLL void Monitors_Get_dblFreq(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Array of doubles containgin time value in hours for time-sampled monitor values; Empty if frequency-sampled values for harmonics solution  (see dblFreq)
    */
    DSSPM_CAPI_DLL void Monitors_Get_dblHour(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Monitor File Version (integer)
    */
    DSSPM_CAPI_DLL int32_t Monitors_Get_FileVersion(void);
    
    /*
    Header string;  Array of strings containing Channel names
    */
    DSSPM_CAPI_DLL void Monitors_Get_Header(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Number of Channels in the active Monitor
    */
    DSSPM_CAPI_DLL int32_t Monitors_Get_NumChannels(void);
    
    /*
    Size of each record in ByteStream (Integer). Same as NumChannels.
    */
    DSSPM_CAPI_DLL int32_t Monitors_Get_RecordSize(void);
    
    /*
    Full object name of element being monitored.
    */
    DSSPM_CAPI_DLL char* Monitors_Get_Element(void);
    
    /*
    Full object name of element being monitored.
    */
    DSSPM_CAPI_DLL void Monitors_Set_Element(char* Value);
    
    /*
    Terminal number of element being monitored
    */
    DSSPM_CAPI_DLL int32_t Monitors_Get_Terminal(void);
    
    /*
    Terminal number of element being monitored.
    */
    DSSPM_CAPI_DLL void Monitors_Set_Terminal(int32_t Value);
    
    /*
    Delivers the number of CPUs on the current PC
    */
    DSSPM_CAPI_DLL int32_t Parallel_Get_NumCPUs(void);
    
    /*
    Delivers the number of Cores of the local PC
    */
    DSSPM_CAPI_DLL int32_t Parallel_Get_NumCores(void);
    
    /*
    Gets the ID of the Active Actor
    */
    DSSPM_CAPI_DLL int32_t Parallel_Get_ActiveActor(void);
    
    /*
    Sets the Active Actor
    */
    DSSPM_CAPI_DLL void Parallel_Set_ActiveActor(int32_t Value);
    
    DSSPM_CAPI_DLL void Parallel_CreateActor(void);
    
    /*
    Gets the CPU of the Active Actor
    */
    DSSPM_CAPI_DLL int32_t Parallel_Get_ActorCPU(void);
    
    /*
    Sets the CPU for the Active Actor
    */
    DSSPM_CAPI_DLL void Parallel_Set_ActorCPU(int32_t Value);
    
    /*
    Gets the number of Actors created
    */
    DSSPM_CAPI_DLL int32_t Parallel_Get_NumOfActors(void);
    
    DSSPM_CAPI_DLL void Parallel_Wait(void);
    
    /*
    Gets the progress of all existing actors in pct
    */
    DSSPM_CAPI_DLL void Parallel_Get_ActorProgress(int32_t** ResultPtr, int32_t* ResultCount);
    
    /*
    Gets the status of each actor
    */
    DSSPM_CAPI_DLL void Parallel_Get_ActorStatus(int32_t** ResultPtr, int32_t* ResultCount);
    
    /*
    Sets ON/OFF (1/0) Parallel features of the Engine
    */
    DSSPM_CAPI_DLL int32_t Parallel_Get_ActiveParallel(void);
    
    /*
    Delivers if the Parallel features of the Engine are Active
    */
    DSSPM_CAPI_DLL void Parallel_Set_ActiveParallel(int32_t Value);
    
    /*
    Reads the values of the ConcatenateReports option (1=enabled, 0=disabled)
    */
    DSSPM_CAPI_DLL int32_t Parallel_Get_ConcatenateReports(void);
    
    /*
    Enable/Disable (1/0) the ConcatenateReports option for extracting monitors data
    */
    DSSPM_CAPI_DLL void Parallel_Set_ConcatenateReports(int32_t Value);
    
    /*
    String to be parsed. Loading this string resets the Parser to the beginning of the line. Then parse off the tokens in sequence.
    */
    DSSPM_CAPI_DLL char* Parser_Get_CmdString(void);
    
    /*
    String to be parsed. Loading this string resets the Parser to the beginning of the line. Then parse off the tokens in sequence.
    */
    DSSPM_CAPI_DLL void Parser_Set_CmdString(char* Value);
    
    /*
    Get next token and return tag name (before = sign) if any. See AutoIncrement.
    */
    DSSPM_CAPI_DLL char* Parser_Get_NextParam(void);
    
    /*
    Default is FALSE. If TRUE parser automatically advances to next token after DblValue, IntValue, or StrValue. Simpler when you don't need to check for parameter names.
    */
    DSSPM_CAPI_DLL uint16_t Parser_Get_AutoIncrement(void);
    
    /*
    Default is FALSE. If TRUE parser automatically advances to next token after DblValue, IntValue, or StrValue. Simpler when you don't need to check for parameter names.
    */
    DSSPM_CAPI_DLL void Parser_Set_AutoIncrement(uint16_t Value);
    
    /*
    Return next parameter as a double.
    */
    DSSPM_CAPI_DLL double Parser_Get_DblValue(void);
    
    /*
    Return next parameter as a long integer.
    */
    DSSPM_CAPI_DLL int32_t Parser_Get_IntValue(void);
    
    /*
    Return next parameter as a string
    */
    DSSPM_CAPI_DLL char* Parser_Get_StrValue(void);
    
    /*
    Get the characters used for White space in the command string.  Default is blank and Tab.
    */
    DSSPM_CAPI_DLL char* Parser_Get_WhiteSpace(void);
    
    /*
    Set the characters used for White space in the command string.  Default is blank and Tab.
    */
    DSSPM_CAPI_DLL void Parser_Set_WhiteSpace(char* Value);
    
    /*
    Get String containing the the characters for Quoting in OpenDSS scripts. Matching pairs defined in EndQuote. Default is "'([{.
    */
    DSSPM_CAPI_DLL char* Parser_Get_BeginQuote(void);
    
    /*
    String containing characters, in order, that match the beginning quote characters in BeginQuote. Default is "')]}
    */
    DSSPM_CAPI_DLL char* Parser_Get_EndQuote(void);
    
    /*
    Set String containing the the characters for Quoting in OpenDSS scripts. Matching pairs defined in EndQuote. Default is "'([{.
    */
    DSSPM_CAPI_DLL void Parser_Set_BeginQuote(char* Value);
    
    /*
    String containing characters, in order, that match the beginning quote characters in BeginQuote. Default is "')]}
    */
    DSSPM_CAPI_DLL void Parser_Set_EndQuote(char* Value);
    
    /*
    String defining hard delimiters used to separate token on the command string. Default is , and =. The = separates token name from token value. These override whitesspace to separate tokens.
    */
    DSSPM_CAPI_DLL char* Parser_Get_Delimiters(void);
    
    /*
    String defining hard delimiters used to separate token on the command string. Default is , and =. The = separates token name from token value. These override whitesspace to separate tokens.
    */
    DSSPM_CAPI_DLL void Parser_Set_Delimiters(char* Value);
    
    DSSPM_CAPI_DLL void Parser_ResetDelimiters(void);
    
    /*
    Returns token as array of doubles. For parsing quoted array syntax.
    */
    DSSPM_CAPI_DLL void Parser_Get_Vector(double** ResultPtr, int32_t* ResultCount, int32_t ExpectedSize);
    
    /*
    Use this property to parse a Matrix token in OpenDSS format.  Returns square matrix of order specified. Order same as default Fortran order: column by column.
    */
    DSSPM_CAPI_DLL void Parser_Get_Matrix(double** ResultPtr, int32_t* ResultCount, int32_t ExpectedOrder);
    
    /*
    Use this property to parse a matrix token specified in lower triangle form. Symmetry is forced.
    */
    DSSPM_CAPI_DLL void Parser_Get_SymMatrix(double** ResultPtr, int32_t* ResultCount, int32_t ExpectedOrder);
    
    /*
    Number of PD elements (including disabled elements)
    */
    DSSPM_CAPI_DLL int32_t PDElements_Get_Count(void);
    
    /*
    Get/Set Number of failures per year. For LINE elements: Number of failures per unit length per year. 
    */
    DSSPM_CAPI_DLL double PDElements_Get_FaultRate(void);
    
    /*
    Set the first enabled PD element to be the active element.  Returns 0 if none found.
    */
    DSSPM_CAPI_DLL int32_t PDElements_Get_First(void);
    
    /*
    Variant boolean indicating of PD element should be treated as a shunt element rather than a series element. Applies to Capacitor and Reactor elements in particular.
    */
    DSSPM_CAPI_DLL uint16_t PDElements_Get_IsShunt(void);
    
    /*
    Advance to the next PD element in the circuit. Enabled elements only. Returns 0 when no more elements.
    */
    DSSPM_CAPI_DLL int32_t PDElements_Get_Next(void);
    
    /*
    Get/Set percent of faults that are permanent (require repair). Otherwise, fault is assumed to be transient/temporary.
    */
    DSSPM_CAPI_DLL double PDElements_Get_pctPermanent(void);
    
    DSSPM_CAPI_DLL void PDElements_Set_FaultRate(double Value);
    
    DSSPM_CAPI_DLL void PDElements_Set_pctPermanent(double Value);
    
    /*
    Get/Set name of active PD Element. Returns null string if active element is not PDElement type.
    */
    DSSPM_CAPI_DLL char* PDElements_Get_Name(void);
    
    DSSPM_CAPI_DLL void PDElements_Set_Name(char* Value);
    
    /*
    accummulated failure rate for this branch on downline
    */
    DSSPM_CAPI_DLL double PDElements_Get_AccumulatedL(void);
    
    /*
    Failure rate for this branch. Faults per year including length of line.
    */
    DSSPM_CAPI_DLL double PDElements_Get_Lambda(void);
    
    /*
    Number of customers, this branch
    */
    DSSPM_CAPI_DLL int32_t PDElements_Get_Numcustomers(void);
    
    /*
    Sets the parent PD element to be the active circuit element.  Returns 0 if no more elements upline.
    */
    DSSPM_CAPI_DLL int32_t PDElements_Get_ParentPDElement(void);
    
    /*
    Average repair time for this element in hours
    */
    DSSPM_CAPI_DLL double PDElements_Get_RepairTime(void);
    
    /*
    Total number of customers from this branch to the end of the zone
    */
    DSSPM_CAPI_DLL int32_t PDElements_Get_Totalcustomers(void);
    
    /*
    Number of the terminal of active PD element that is on the "from" side. This is set after the meter zone is determined.
    */
    DSSPM_CAPI_DLL int32_t PDElements_Get_FromTerminal(void);
    
    /*
    Total miles of line from this element to the end of the zone. For recloser siting algorithm.
    */
    DSSPM_CAPI_DLL double PDElements_Get_TotalMiles(void);
    
    /*
    Integer ID of the feeder section that this PDElement branch is part of
    */
    DSSPM_CAPI_DLL int32_t PDElements_Get_SectionID(void);
    
    /*
    Average repair time for this element in hours
    */
    DSSPM_CAPI_DLL void PDElements_Set_RepairTime(double Value);
    
    /*
    Vairant array of strings with all PVSystem names
    */
    DSSPM_CAPI_DLL void PVSystems_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Variant Array of PVSYSTEM energy meter register names
    */
    DSSPM_CAPI_DLL void PVSystems_Get_RegisterNames(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Array of doubles containing values in PVSystem registers.
    */
    DSSPM_CAPI_DLL void PVSystems_Get_RegisterValues(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Set first PVSystem active; returns 0 if none.
    */
    DSSPM_CAPI_DLL int32_t PVSystems_Get_First(void);
    
    /*
    Sets next PVSystem active; returns 0 if no more.
    */
    DSSPM_CAPI_DLL int32_t PVSystems_Get_Next(void);
    
    /*
    Number of PVSystems
    */
    DSSPM_CAPI_DLL int32_t PVSystems_Get_Count(void);
    
    /*
    Get/set active PVSystem by index;  1..Count
    */
    DSSPM_CAPI_DLL int32_t PVSystems_Get_idx(void);
    
    /*
    Get/Set Active PVSystem by index:  1.. Count
    */
    DSSPM_CAPI_DLL void PVSystems_Set_idx(int32_t Value);
    
    /*
    Get the name of the active PVSystem
    */
    DSSPM_CAPI_DLL char* PVSystems_Get_Name(void);
    
    /*
    Set the name of the active PVSystem
    */
    DSSPM_CAPI_DLL void PVSystems_Set_Name(char* Value);
    
    /*
    Get the present value of the Irradiance property in W/sq-m
    */
    DSSPM_CAPI_DLL double PVSystems_Get_Irradiance(void);
    
    /*
    Set the present Irradiance value in W/sq-m
    */
    DSSPM_CAPI_DLL void PVSystems_Set_Irradiance(double Value);
    
    /*
    Get kvar value
    */
    DSSPM_CAPI_DLL double PVSystems_Get_kvar(void);
    
    /*
    Get Rated kVA of the PVSystem
    */
    DSSPM_CAPI_DLL double PVSystems_Get_kVArated(void);
    
    /*
    get kW output
    */
    DSSPM_CAPI_DLL double PVSystems_Get_kW(void);
    
    /*
    Get Power factor 
    */
    DSSPM_CAPI_DLL double PVSystems_Get_PF(void);
    
    /*
    Array of strings with names of all Reclosers in Active Circuit
    */
    DSSPM_CAPI_DLL void Reclosers_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Number of Reclosers in active circuit.
    */
    DSSPM_CAPI_DLL int32_t Reclosers_Get_Count(void);
    
    /*
    Set First Recloser to be Active Ckt Element. Returns 0 if none.
    */
    DSSPM_CAPI_DLL int32_t Reclosers_Get_First(void);
    
    /*
    Get Name of active Recloser or set the active Recloser by name.
    */
    DSSPM_CAPI_DLL char* Reclosers_Get_Name(void);
    
    /*
    Iterate to the next recloser in the circuit. Returns zero if no more.
    */
    DSSPM_CAPI_DLL int32_t Reclosers_Get_Next(void);
    
    DSSPM_CAPI_DLL void Reclosers_Set_Name(char* Value);
    
    /*
    Terminal number of Monitored object for the Recloser 
    */
    DSSPM_CAPI_DLL int32_t Reclosers_Get_MonitoredTerm(void);
    
    DSSPM_CAPI_DLL void Reclosers_Set_MonitoredTerm(int32_t Value);
    
    /*
    Full name of the circuit element that is being switched by the Recloser.
    */
    DSSPM_CAPI_DLL char* Reclosers_Get_SwitchedObj(void);
    
    DSSPM_CAPI_DLL void Reclosers_Set_SwitchedObj(char* Value);
    
    /*
    Full name of object this Recloser is monitoring.
    */
    DSSPM_CAPI_DLL char* Reclosers_Get_MonitoredObj(void);
    
    /*
    Terminal number of the controlled device being switched by the Recloser
    */
    DSSPM_CAPI_DLL int32_t Reclosers_Get_SwitchedTerm(void);
    
    /*
    Set monitored object by full name.
    */
    DSSPM_CAPI_DLL void Reclosers_Set_MonitoredObj(char* Value);
    
    DSSPM_CAPI_DLL void Reclosers_Set_SwitchedTerm(int32_t Value);
    
    /*
    Number of fast shots
    */
    DSSPM_CAPI_DLL int32_t Reclosers_Get_NumFast(void);
    
    /*
    Variant Array of Doubles: reclose intervals, s, between shots.
    */
    DSSPM_CAPI_DLL void Reclosers_Get_RecloseIntervals(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Number of shots to lockout (fast + delayed)
    */
    DSSPM_CAPI_DLL int32_t Reclosers_Get_Shots(void);
    
    DSSPM_CAPI_DLL void Reclosers_Set_NumFast(int32_t Value);
    
    DSSPM_CAPI_DLL void Reclosers_Set_Shots(int32_t Value);
    
    /*
    Phase trip curve multiplier or actual amps
    */
    DSSPM_CAPI_DLL double Reclosers_Get_PhaseTrip(void);
    
    /*
    Phase Trip multiplier or actual amps
    */
    DSSPM_CAPI_DLL void Reclosers_Set_PhaseTrip(double Value);
    
    /*
    Ground (3I0) instantaneous trip setting - curve multipler or actual amps.
    */
    DSSPM_CAPI_DLL double Reclosers_Get_GroundInst(void);
    
    /*
    Ground (3I0) trip multiplier or actual amps
    */
    DSSPM_CAPI_DLL double Reclosers_Get_GroundTrip(void);
    
    /*
    Phase instantaneous curve multipler or actual amps
    */
    DSSPM_CAPI_DLL double Reclosers_Get_PhaseInst(void);
    
    /*
    Ground (3I0) trip instantaneous multiplier or actual amps
    */
    DSSPM_CAPI_DLL void Reclosers_Set_GroundInst(double Value);
    
    DSSPM_CAPI_DLL void Reclosers_Set_GroundTrip(double Value);
    
    DSSPM_CAPI_DLL void Reclosers_Set_PhaseInst(double Value);
    
    DSSPM_CAPI_DLL void Reclosers_Close(void);
    
    DSSPM_CAPI_DLL void Reclosers_Open(void);
    
    /*
    Get/Set the active Recloser by index into the recloser list.  1..Count
    */
    DSSPM_CAPI_DLL int32_t Reclosers_Get_idx(void);
    
    /*
    Get/Set the Active Recloser by index into the recloser list. 1..Count
    */
    DSSPM_CAPI_DLL void Reclosers_Set_idx(int32_t Value);
    
    /*
    Array of strings containing all RegControl names
    */
    DSSPM_CAPI_DLL void RegControls_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    CT primary ampere rating (secondary is 0.2 amperes)
    */
    DSSPM_CAPI_DLL double RegControls_Get_CTPrimary(void);
    
    /*
    Time delay [s] after arming before the first tap change. Control may reset before actually changing taps.
    */
    DSSPM_CAPI_DLL double RegControls_Get_Delay(void);
    
    /*
    Sets the first RegControl active. Returns 0 if none.
    */
    DSSPM_CAPI_DLL int32_t RegControls_Get_First(void);
    
    /*
    Regulation bandwidth in forward direciton, centered on Vreg
    */
    DSSPM_CAPI_DLL double RegControls_Get_ForwardBand(void);
    
    /*
    LDC R setting in Volts
    */
    DSSPM_CAPI_DLL double RegControls_Get_ForwardR(void);
    
    /*
    Target voltage in the forward direction, on PT secondary base.
    */
    DSSPM_CAPI_DLL double RegControls_Get_ForwardVreg(void);
    
    /*
    LDC X setting in Volts
    */
    DSSPM_CAPI_DLL double RegControls_Get_ForwardX(void);
    
    /*
    Time delay is inversely adjsuted, proportinal to the amount of voltage outside the regulating band.
    */
    DSSPM_CAPI_DLL uint16_t RegControls_Get_IsInverseTime(void);
    
    /*
    Regulator can use different settings in the reverse direction.  Usually not applicable to substation transformers.
    */
    DSSPM_CAPI_DLL uint16_t RegControls_Get_IsReversible(void);
    
    /*
    Maximum tap change per iteration in STATIC solution mode. 1 is more realistic, 16 is the default for a faster soluiton.
    */
    DSSPM_CAPI_DLL int32_t RegControls_Get_MaxTapChange(void);
    
    /*
    Name of a remote regulated bus, in lieu of LDC settings
    */
    DSSPM_CAPI_DLL char* RegControls_Get_MonitoredBus(void);
    
    /*
    Get/set Active RegControl  name
    */
    DSSPM_CAPI_DLL char* RegControls_Get_Name(void);
    
    /*
    Sets the next RegControl active. Returns 0 if none.
    */
    DSSPM_CAPI_DLL int32_t RegControls_Get_Next(void);
    
    /*
    PT ratio for voltage control settings
    */
    DSSPM_CAPI_DLL double RegControls_Get_PTratio(void);
    
    /*
    Bandwidth in reverse direction, centered on reverse Vreg.
    */
    DSSPM_CAPI_DLL double RegControls_Get_ReverseBand(void);
    
    /*
    Reverse LDC R setting in Volts.
    */
    DSSPM_CAPI_DLL double RegControls_Get_ReverseR(void);
    
    /*
    Target voltage in the revese direction, on PT secondary base.
    */
    DSSPM_CAPI_DLL double RegControls_Get_ReverseVreg(void);
    
    /*
    Reverse LDC X setting in volts.
    */
    DSSPM_CAPI_DLL double RegControls_Get_ReverseX(void);
    
    /*
    Time delay [s] for subsequent tap changes in a set. Control may reset before actually changing taps.
    */
    DSSPM_CAPI_DLL double RegControls_Get_TapDelay(void);
    
    /*
    Tapped winding number
    */
    DSSPM_CAPI_DLL int32_t RegControls_Get_TapWinding(void);
    
    /*
    Name of the transformer this regulator controls
    */
    DSSPM_CAPI_DLL char* RegControls_Get_Transformer(void);
    
    /*
    First house voltage limit on PT secondary base.  Setting to 0 disables this function.
    */
    DSSPM_CAPI_DLL double RegControls_Get_VoltageLimit(void);
    
    /*
    Winding number for PT and CT connections
    */
    DSSPM_CAPI_DLL int32_t RegControls_Get_Winding(void);
    
    DSSPM_CAPI_DLL int32_t RegControls_Get_TapNumber(void);
    
    /*
    CT primary ampere rating (secondary is 0.2 amperes)
    */
    DSSPM_CAPI_DLL void RegControls_Set_CTPrimary(double Value);
    
    /*
    Time delay [s] after arming before the first tap change. Control may reset before actually changing taps.
    */
    DSSPM_CAPI_DLL void RegControls_Set_Delay(double Value);
    
    /*
    Regulation bandwidth in forward direciton, centered on Vreg
    */
    DSSPM_CAPI_DLL void RegControls_Set_ForwardBand(double Value);
    
    /*
    LDC R setting in Volts
    */
    DSSPM_CAPI_DLL void RegControls_Set_ForwardR(double Value);
    
    /*
    Target voltage in the forward direction, on PT secondary base.
    */
    DSSPM_CAPI_DLL void RegControls_Set_ForwardVreg(double Value);
    
    /*
    LDC X setting in Volts
    */
    DSSPM_CAPI_DLL void RegControls_Set_ForwardX(double Value);
    
    /*
    Time delay is inversely adjsuted, proportinal to the amount of voltage outside the regulating band.
    */
    DSSPM_CAPI_DLL void RegControls_Set_IsInverseTime(uint16_t Value);
    
    /*
    Regulator can use different settings in the reverse direction.  Usually not applicable to substation transformers.
    */
    DSSPM_CAPI_DLL void RegControls_Set_IsReversible(uint16_t Value);
    
    /*
    Maximum tap change per iteration in STATIC solution mode. 1 is more realistic, 16 is the default for a faster soluiton.
    */
    DSSPM_CAPI_DLL void RegControls_Set_MaxTapChange(int32_t Value);
    
    /*
    Name of a remote regulated bus, in lieu of LDC settings
    */
    DSSPM_CAPI_DLL void RegControls_Set_MonitoredBus(char* Value);
    
    /*
    Sets a RegControl active by name
    */
    DSSPM_CAPI_DLL void RegControls_Set_Name(char* Value);
    
    /*
    PT ratio for voltage control settings
    */
    DSSPM_CAPI_DLL void RegControls_Set_PTratio(double Value);
    
    /*
    Bandwidth in reverse direction, centered on reverse Vreg.
    */
    DSSPM_CAPI_DLL void RegControls_Set_ReverseBand(double Value);
    
    /*
    Reverse LDC R setting in Volts.
    */
    DSSPM_CAPI_DLL void RegControls_Set_ReverseR(double Value);
    
    /*
    Target voltage in the revese direction, on PT secondary base.
    */
    DSSPM_CAPI_DLL void RegControls_Set_ReverseVreg(double Value);
    
    /*
    Reverse LDC X setting in volts.
    */
    DSSPM_CAPI_DLL void RegControls_Set_ReverseX(double Value);
    
    /*
    Time delay [s] for subsequent tap changes in a set. Control may reset before actually changing taps.
    */
    DSSPM_CAPI_DLL void RegControls_Set_TapDelay(double Value);
    
    /*
    Tapped winding number
    */
    DSSPM_CAPI_DLL void RegControls_Set_TapWinding(int32_t Value);
    
    /*
    Name of the transformer this regulator controls
    */
    DSSPM_CAPI_DLL void RegControls_Set_Transformer(char* Value);
    
    /*
    First house voltage limit on PT secondary base.  Setting to 0 disables this function.
    */
    DSSPM_CAPI_DLL void RegControls_Set_VoltageLimit(double Value);
    
    /*
    Winding number for PT and CT connections
    */
    DSSPM_CAPI_DLL void RegControls_Set_Winding(int32_t Value);
    
    /*
    Integer number of the tap that the controlled transformer winding is currentliy on.
    */
    DSSPM_CAPI_DLL void RegControls_Set_TapNumber(int32_t Value);
    
    /*
    Number of RegControl objects in Active Circuit
    */
    DSSPM_CAPI_DLL int32_t RegControls_Get_Count(void);
    
    DSSPM_CAPI_DLL void RegControls_Reset(void);
    
    /*
    Array of strings containing names of all Relay elements
    */
    DSSPM_CAPI_DLL void Relays_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Number of Relays in circuit
    */
    DSSPM_CAPI_DLL int32_t Relays_Get_Count(void);
    
    /*
    Set First Relay active. If none, returns 0.
    */
    DSSPM_CAPI_DLL int32_t Relays_Get_First(void);
    
    /*
    Get name of active relay.
    */
    DSSPM_CAPI_DLL char* Relays_Get_Name(void);
    
    /*
    Advance to next Relay object. Returns 0 when no more relays.
    */
    DSSPM_CAPI_DLL int32_t Relays_Get_Next(void);
    
    /*
    Set Relay active by name
    */
    DSSPM_CAPI_DLL void Relays_Set_Name(char* Value);
    
    /*
    Full name of object this Relay is monitoring.
    */
    DSSPM_CAPI_DLL char* Relays_Get_MonitoredObj(void);
    
    DSSPM_CAPI_DLL void Relays_Set_MonitoredObj(char* Value);
    
    /*
    Number of terminal of monitored element that this Relay is monitoring.
    */
    DSSPM_CAPI_DLL int32_t Relays_Get_MonitoredTerm(void);
    
    /*
    Full name of element that will be switched when relay trips.
    */
    DSSPM_CAPI_DLL char* Relays_Get_SwitchedObj(void);
    
    DSSPM_CAPI_DLL void Relays_Set_MonitoredTerm(int32_t Value);
    
    DSSPM_CAPI_DLL void Relays_Set_SwitchedObj(char* Value);
    
    DSSPM_CAPI_DLL int32_t Relays_Get_SwitchedTerm(void);
    
    /*
    Terminal number of the switched object that will be opened when the relay trips.
    */
    DSSPM_CAPI_DLL void Relays_Set_SwitchedTerm(int32_t Value);
    
    /*
    Get/Set active Relay by index into the Relay list. 1..Count
    */
    DSSPM_CAPI_DLL int32_t Relays_Get_idx(void);
    
    /*
    Get/Set Relay active by index into relay list. 1..Count
    */
    DSSPM_CAPI_DLL void Relays_Set_idx(int32_t Value);
    
    /*
    Array of Sensor names.
    */
    DSSPM_CAPI_DLL void Sensors_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Number of Sensors in Active Circuit.
    */
    DSSPM_CAPI_DLL int32_t Sensors_Get_Count(void);
    
    /*
    Array of doubles for the line current measurements; don't use with kWS and kVARS.
    */
    DSSPM_CAPI_DLL void Sensors_Get_Currents(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Sets the first sensor active. Returns 0 if none.
    */
    DSSPM_CAPI_DLL int32_t Sensors_Get_First(void);
    
    /*
    True if measured voltages are line-line. Currents are always line currents.
    */
    DSSPM_CAPI_DLL uint16_t Sensors_Get_IsDelta(void);
    
    /*
    Array of doubles for Q measurements. Overwrites Currents with a new estimate using kWS.
    */
    DSSPM_CAPI_DLL void Sensors_Get_kVARS(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Array of doubles for the LL or LN (depending on Delta connection) voltage measurements.
    */
    DSSPM_CAPI_DLL void Sensors_Get_kVS(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Array of doubles for P measurements. Overwrites Currents with a new estimate using kVARS.
    */
    DSSPM_CAPI_DLL void Sensors_Get_kWS(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Full Name of the measured element
    */
    DSSPM_CAPI_DLL char* Sensors_Get_MeteredElement(void);
    
    /*
    Number of the measured terminal in the measured element.
    */
    DSSPM_CAPI_DLL int32_t Sensors_Get_MeteredTerminal(void);
    
    /*
    Name of the active sensor.
    */
    DSSPM_CAPI_DLL char* Sensors_Get_Name(void);
    
    /*
    Sets the next Sensor active. Returns 0 if no more.
    */
    DSSPM_CAPI_DLL int32_t Sensors_Get_Next(void);
    
    /*
    Assumed percent error in the Sensor measurement. Default is 1.
    */
    DSSPM_CAPI_DLL double Sensors_Get_PctError(void);
    
    /*
    True if voltage measurements are 1-3, 3-2, 2-1.
    */
    DSSPM_CAPI_DLL uint16_t Sensors_Get_ReverseDelta(void);
    
    /*
    Weighting factor for this Sensor measurement with respect to other Sensors. Default is 1.
    */
    DSSPM_CAPI_DLL double Sensors_Get_Weight(void);
    
    DSSPM_CAPI_DLL void Sensors_Reset(void);
    
    DSSPM_CAPI_DLL void Sensors_ResetAll(void);
    
    DSSPM_CAPI_DLL void Sensors_Set_Currents(double* ValuePtr, int32_t ValueCount);
    
    DSSPM_CAPI_DLL void Sensors_Set_IsDelta(uint16_t Value);
    
    DSSPM_CAPI_DLL void Sensors_Set_kVARS(double* ValuePtr, int32_t ValueCount);
    
    DSSPM_CAPI_DLL void Sensors_Set_kVS(double* ValuePtr, int32_t ValueCount);
    
    DSSPM_CAPI_DLL void Sensors_Set_kWS(double* ValuePtr, int32_t ValueCount);
    
    DSSPM_CAPI_DLL void Sensors_Set_MeteredElement(char* Value);
    
    DSSPM_CAPI_DLL void Sensors_Set_MeteredTerminal(int32_t Value);
    
    /*
    Set the active Sensor by name.
    */
    DSSPM_CAPI_DLL void Sensors_Set_Name(char* Value);
    
    DSSPM_CAPI_DLL void Sensors_Set_PctError(double Value);
    
    DSSPM_CAPI_DLL void Sensors_Set_ReverseDelta(uint16_t Value);
    
    DSSPM_CAPI_DLL void Sensors_Set_Weight(double Value);
    
    /*
    Voltage base for the sensor measurements. LL for 2 and 3-phase sensors, LN for 1-phase sensors.
    */
    DSSPM_CAPI_DLL double Sensors_Get_kVbase(void);
    
    DSSPM_CAPI_DLL void Sensors_Set_kVbase(double Value);
    
    /*
    {True | False*} Designates whether to allow duplicate names of objects
    */
    DSSPM_CAPI_DLL uint16_t Settings_Get_AllowDuplicates(void);
    
    /*
    List of Buses or (File=xxxx) syntax for the AutoAdd solution mode.
    */
    DSSPM_CAPI_DLL char* Settings_Get_AutoBusList(void);
    
    /*
    {dssMultiphase * | dssPositiveSeq} IIndicate if the circuit model is positive sequence.
    */
    DSSPM_CAPI_DLL int32_t Settings_Get_CktModel(void);
    
    /*
    Per Unit maximum voltage for Emergency conditions.
    */
    DSSPM_CAPI_DLL double Settings_Get_EmergVmaxpu(void);
    
    /*
    Per Unit minimum voltage for Emergency conditions.
    */
    DSSPM_CAPI_DLL double Settings_Get_EmergVminpu(void);
    
    /*
    Per Unit maximum voltage for Normal conditions.
    */
    DSSPM_CAPI_DLL double Settings_Get_NormVmaxpu(void);
    
    /*
    Per Unit minimum voltage for Normal conditions.
    */
    DSSPM_CAPI_DLL double Settings_Get_NormVminpu(void);
    
    /*
    {True | False*}  Locks Zones on energy meters to prevent rebuilding if a circuit change occurs.
    */
    DSSPM_CAPI_DLL uint16_t Settings_Get_ZoneLock(void);
    
    /*
    Sets all load allocation factors for all loads defined by XFKVA property to this value.
    */
    DSSPM_CAPI_DLL void Settings_Set_AllocationFactors(double Value);
    
    /*
    {True | False*} Designates whether to allow duplicate names of objects
    */
    DSSPM_CAPI_DLL void Settings_Set_AllowDuplicates(uint16_t Value);
    
    /*
    List of Buses or (File=xxxx) syntax for the AutoAdd solution mode.
    */
    DSSPM_CAPI_DLL void Settings_Set_AutoBusList(char* Value);
    
    /*
    {dssMultiphase * | dssPositiveSeq} IIndicate if the circuit model is positive sequence.
    */
    DSSPM_CAPI_DLL void Settings_Set_CktModel(int32_t Value);
    
    /*
    Per Unit maximum voltage for Emergency conditions.
    */
    DSSPM_CAPI_DLL void Settings_Set_EmergVmaxpu(double Value);
    
    /*
    Per Unit minimum voltage for Emergency conditions.
    */
    DSSPM_CAPI_DLL void Settings_Set_EmergVminpu(double Value);
    
    /*
    Per Unit maximum voltage for Normal conditions.
    */
    DSSPM_CAPI_DLL void Settings_Set_NormVmaxpu(double Value);
    
    /*
    Per Unit minimum voltage for Normal conditions.
    */
    DSSPM_CAPI_DLL void Settings_Set_NormVminpu(double Value);
    
    /*
    {True | False*}  Locks Zones on energy meters to prevent rebuilding if a circuit change occurs.
    */
    DSSPM_CAPI_DLL void Settings_Set_ZoneLock(uint16_t Value);
    
    /*
    Integer array defining which energy meter registers to use for computing losses
    */
    DSSPM_CAPI_DLL void Settings_Get_LossRegs(int32_t** ResultPtr, int32_t* ResultCount);
    
    /*
    Weighting factor applied to Loss register values.
    */
    DSSPM_CAPI_DLL double Settings_Get_LossWeight(void);
    
    /*
    {True | False *} Gets value of trapezoidal integration flag in energy meters.
    */
    DSSPM_CAPI_DLL uint16_t Settings_Get_Trapezoidal(void);
    
    /*
    Array of Integers defining energy meter registers to use for computing UE
    */
    DSSPM_CAPI_DLL void Settings_Get_UEregs(int32_t** ResultPtr, int32_t* ResultCount);
    
    /*
    Weighting factor applied to UE register values.
    */
    DSSPM_CAPI_DLL double Settings_Get_UEweight(void);
    
    /*
    Integer array defining which energy meter registers to use for computing losses
    */
    DSSPM_CAPI_DLL void Settings_Set_LossRegs(int32_t* ValuePtr, int32_t ValueCount);
    
    /*
    Weighting factor applied to Loss register values.
    */
    DSSPM_CAPI_DLL void Settings_Set_LossWeight(double Value);
    
    /*
    {True | False *} Gets value of trapezoidal integration flag in energy meters.
    */
    DSSPM_CAPI_DLL void Settings_Set_Trapezoidal(uint16_t Value);
    
    /*
    Array of Integers defining energy meter registers to use for computing UE
    */
    DSSPM_CAPI_DLL void Settings_Set_UEregs(int32_t* ValuePtr, int32_t ValueCount);
    
    /*
    Weighting factor applied to UE register values.
    */
    DSSPM_CAPI_DLL void Settings_Set_UEweight(double Value);
    
    /*
    {True | False*} Denotes whether to trace the control actions to a file.
    */
    DSSPM_CAPI_DLL uint16_t Settings_Get_ControlTrace(void);
    
    /*
    Array of doubles defining the legal voltage bases in kV L-L
    */
    DSSPM_CAPI_DLL void Settings_Get_VoltageBases(double** ResultPtr, int32_t* ResultCount);
    
    /*
    {True | False*} Denotes whether to trace the control actions to a file.
    */
    DSSPM_CAPI_DLL void Settings_Set_ControlTrace(uint16_t Value);
    
    /*
    Array of doubles defining the legal voltage bases in kV L-L
    */
    DSSPM_CAPI_DLL void Settings_Set_VoltageBases(double* ValuePtr, int32_t ValueCount);
    
    /*
    Name of LoadShape object that serves as the source of price signal data for yearly simulations, etc.
    */
    DSSPM_CAPI_DLL char* Settings_Get_PriceCurve(void);
    
    /*
    Price Signal for the Circuit
    */
    DSSPM_CAPI_DLL double Settings_Get_PriceSignal(void);
    
    /*
    Name of LoadShape object that serves as the source of price signal data for yearly simulations, etc.
    */
    DSSPM_CAPI_DLL void Settings_Set_PriceCurve(char* Value);
    
    /*
    Price Signal for the Circuit
    */
    DSSPM_CAPI_DLL void Settings_Set_PriceSignal(double Value);
    
    /*
    Set the Frequency for next solution
    */
    DSSPM_CAPI_DLL double Solution_Get_Frequency(void);
    
    /*
    Set Hour for time series solutions.
    */
    DSSPM_CAPI_DLL int32_t Solution_Get_Hour(void);
    
    /*
    Number of iterations taken for last solution. (Same as TotalIterations)
    */
    DSSPM_CAPI_DLL int32_t Solution_Get_Iterations(void);
    
    /*
    Default load multiplier applied to all non-fixed loads
    */
    DSSPM_CAPI_DLL double Solution_Get_LoadMult(void);
    
    /*
    Max allowable iterations.
    */
    DSSPM_CAPI_DLL int32_t Solution_Get_MaxIterations(void);
    
    /*
    Set present solution mode (by a text code - see DSS Help)
    */
    DSSPM_CAPI_DLL int32_t Solution_Get_Mode(void);
    
    /*
    Number of solutions to perform for Monte Carlo and time series simulations
    */
    DSSPM_CAPI_DLL int32_t Solution_Get_Number(void);
    
    /*
    Randomization mode for random variables "Gaussian" or "Uniform"
    */
    DSSPM_CAPI_DLL int32_t Solution_Get_Random(void);
    
    /*
    Seconds from top of the hour.
    */
    DSSPM_CAPI_DLL double Solution_Get_Seconds(void);
    
    /*
    Time step size in sec
    */
    DSSPM_CAPI_DLL double Solution_Get_StepSize(void);
    
    /*
    Solution convergence tolerance.
    */
    DSSPM_CAPI_DLL double Solution_Get_Tolerance(void);
    
    /*
    Set year for planning studies
    */
    DSSPM_CAPI_DLL int32_t Solution_Get_Year(void);
    
    /*
    Set the Frequency for next solution
    */
    DSSPM_CAPI_DLL void Solution_Set_Frequency(double Value);
    
    /*
    Set Hour for time series solutions.
    */
    DSSPM_CAPI_DLL void Solution_Set_Hour(int32_t Value);
    
    /*
    Default load multiplier applied to all non-fixed loads
    */
    DSSPM_CAPI_DLL void Solution_Set_LoadMult(double Value);
    
    /*
    Max allowable iterations.
    */
    DSSPM_CAPI_DLL void Solution_Set_MaxIterations(int32_t Value);
    
    /*
    Set present solution mode (by a text code - see DSS Help)
    */
    DSSPM_CAPI_DLL void Solution_Set_Mode(int32_t Mode);
    
    /*
    Number of solutions to perform for Monte Carlo and time series simulations
    */
    DSSPM_CAPI_DLL void Solution_Set_Number(int32_t Value);
    
    /*
    Randomization mode for random variables "Gaussian" or "Uniform"
    */
    DSSPM_CAPI_DLL void Solution_Set_Random(int32_t Random);
    
    /*
    Seconds from top of the hour.
    */
    DSSPM_CAPI_DLL void Solution_Set_Seconds(double Value);
    
    /*
    Time step size in sec
    */
    DSSPM_CAPI_DLL void Solution_Set_StepSize(double Value);
    
    /*
    Solution convergence tolerance.
    */
    DSSPM_CAPI_DLL void Solution_Set_Tolerance(double Value);
    
    /*
    Set year for planning studies
    */
    DSSPM_CAPI_DLL void Solution_Set_Year(int32_t Value);
    
    DSSPM_CAPI_DLL void Solution_Solve(void);
    
    /*
    ID (text) of the present solution mode
    */
    DSSPM_CAPI_DLL char* Solution_Get_ModeID(void);
    
    /*
    Load Model: {dssPowerFlow (default) | dssAdmittance}
    */
    DSSPM_CAPI_DLL int32_t Solution_Get_LoadModel(void);
    
    /*
    Load Model: {dssPowerFlow (default) | dssAdmittance}
    */
    DSSPM_CAPI_DLL void Solution_Set_LoadModel(int32_t Value);
    
    /*
    Load-Duration Curve name for LD modes
    */
    DSSPM_CAPI_DLL char* Solution_Get_LDCurve(void);
    
    /*
    Load-Duration Curve name for LD modes
    */
    DSSPM_CAPI_DLL void Solution_Set_LDCurve(char* Value);
    
    /*
    Percent default  annual load growth rate
    */
    DSSPM_CAPI_DLL double Solution_Get_pctGrowth(void);
    
    /*
    Percent default  annual load growth rate
    */
    DSSPM_CAPI_DLL void Solution_Set_pctGrowth(double Value);
    
    /*
    Type of device to add in AutoAdd Mode: {dssGen (Default) | dssCap}
    */
    DSSPM_CAPI_DLL int32_t Solution_Get_AddType(void);
    
    /*
    Type of device to add in AutoAdd Mode: {dssGen (Default) | dssCap}
    */
    DSSPM_CAPI_DLL void Solution_Set_AddType(int32_t Value);
    
    /*
    Generator kW for AutoAdd mode
    */
    DSSPM_CAPI_DLL double Solution_Get_GenkW(void);
    
    /*
    Generator kW for AutoAdd mode
    */
    DSSPM_CAPI_DLL void Solution_Set_GenkW(double Value);
    
    /*
    PF for generators in AutoAdd mode
    */
    DSSPM_CAPI_DLL double Solution_Get_GenPF(void);
    
    /*
    PF for generators in AutoAdd mode
    */
    DSSPM_CAPI_DLL void Solution_Set_GenPF(double Value);
    
    /*
    Capacitor kvar for adding capacitors in AutoAdd mode
    */
    DSSPM_CAPI_DLL double Solution_Get_Capkvar(void);
    
    /*
    Capacitor kvar for adding capacitors in AutoAdd mode
    */
    DSSPM_CAPI_DLL void Solution_Set_Capkvar(double Value);
    
    /*
    Base Solution algorithm: {dssNormalSolve | dssNewtonSolve}
    */
    DSSPM_CAPI_DLL int32_t Solution_Get_Algorithm(void);
    
    /*
    Base Solution algorithm: {dssNormalSolve | dssNewtonSolve}
    */
    DSSPM_CAPI_DLL void Solution_Set_Algorithm(int32_t Value);
    
    /*
    {dssStatic* | dssEvent | dssTime}  Modes for control devices
    */
    DSSPM_CAPI_DLL int32_t Solution_Get_ControlMode(void);
    
    /*
    {dssStatic* | dssEvent | dssTime}  Modes for control devices
    */
    DSSPM_CAPI_DLL void Solution_Set_ControlMode(int32_t Value);
    
    /*
    Default Multiplier applied to generators (like LoadMult)
    */
    DSSPM_CAPI_DLL double Solution_Get_GenMult(void);
    
    /*
    Default Multiplier applied to generators (like LoadMult)
    */
    DSSPM_CAPI_DLL void Solution_Set_GenMult(double Value);
    
    /*
    Default daily load shape (defaults to "Default\
    */
    DSSPM_CAPI_DLL char* Solution_Get_DefaultDaily(void);
    
    /*
    Default Yearly load shape (defaults to "Default\
    */
    DSSPM_CAPI_DLL char* Solution_Get_DefaultYearly(void);
    
    /*
    Default daily load shape (defaults to "Default\
    */
    DSSPM_CAPI_DLL void Solution_Set_DefaultDaily(char* Value);
    
    /*
    Default Yearly load shape (defaults to "Default\
    */
    DSSPM_CAPI_DLL void Solution_Set_DefaultYearly(char* Value);
    
    /*
    Array of strings containing the Event Log
    */
    DSSPM_CAPI_DLL void Solution_Get_EventLog(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Hour as a double, including fractional part
    */
    DSSPM_CAPI_DLL double Solution_Get_dblHour(void);
    
    /*
    Hour as a double, including fractional part
    */
    DSSPM_CAPI_DLL void Solution_Set_dblHour(double Value);
    
    /*
    Set Stepsize in Hr
    */
    DSSPM_CAPI_DLL void Solution_Set_StepsizeHr(double Value);
    
    /*
    Set Stepsize in minutes
    */
    DSSPM_CAPI_DLL void Solution_Set_StepsizeMin(double Value);
    
    /*
    Value of the control iteration counter
    */
    DSSPM_CAPI_DLL int32_t Solution_Get_ControlIterations(void);
    
    /*
    Maximum allowable control iterations
    */
    DSSPM_CAPI_DLL int32_t Solution_Get_MaxControlIterations(void);
    
    DSSPM_CAPI_DLL void Solution_Sample_DoControlActions(void);
    
    /*
    Value of the control iteration counter
    */
    DSSPM_CAPI_DLL void Solution_Set_ControlIterations(int32_t Value);
    
    /*
    Maximum allowable control iterations
    */
    DSSPM_CAPI_DLL void Solution_Set_MaxControlIterations(int32_t Value);
    
    DSSPM_CAPI_DLL void Solution_CheckFaultStatus(void);
    
    DSSPM_CAPI_DLL void Solution_SolveDirect(void);
    
    DSSPM_CAPI_DLL void Solution_SolveNoControl(void);
    
    DSSPM_CAPI_DLL void Solution_SolvePflow(void);
    
    DSSPM_CAPI_DLL void Solution_SolvePlusControl(void);
    
    DSSPM_CAPI_DLL void Solution_SolveSnap(void);
    
    DSSPM_CAPI_DLL void Solution_CheckControls(void);
    
    DSSPM_CAPI_DLL void Solution_InitSnap(void);
    
    /*
    Flag that indicates if elements of the System Y have been changed by recent activity.
    */
    DSSPM_CAPI_DLL uint16_t Solution_Get_SystemYChanged(void);
    
    DSSPM_CAPI_DLL void Solution_BuildYMatrix(int32_t BuildOption, int32_t AllocateVI);
    
    DSSPM_CAPI_DLL void Solution_DoControlActions(void);
    
    DSSPM_CAPI_DLL void Solution_SampleControlDevices(void);
    
    /*
    Flag to indicate whether the circuit solution converged
    */
    DSSPM_CAPI_DLL uint16_t Solution_Get_Converged(void);
    
    /*
    Flag to indicate whether the circuit solution converged
    */
    DSSPM_CAPI_DLL void Solution_Set_Converged(uint16_t Value);
    
    /*
    Total iterations including control iterations for most recent solution.
    */
    DSSPM_CAPI_DLL int32_t Solution_Get_Totaliterations(void);
    
    /*
    Max number of iterations required to converge at any control iteration of the most recent solution.
    */
    DSSPM_CAPI_DLL int32_t Solution_Get_MostIterationsDone(void);
    
    /*
    Flag indicating the control actions are done.
    */
    DSSPM_CAPI_DLL uint16_t Solution_Get_ControlActionsDone(void);
    
    DSSPM_CAPI_DLL void Solution_Set_ControlActionsDone(uint16_t Value);
    
    DSSPM_CAPI_DLL void Solution_Cleanup(void);
    
    DSSPM_CAPI_DLL void Solution_FinishTimeStep(void);
    
    /*
    Gets the time required to perform the latest solution (Read only)
    */
    DSSPM_CAPI_DLL double Solution_Get_Process_Time(void);
    
    /*
    Gets the accumulated time of the simulation
    */
    DSSPM_CAPI_DLL double Solution_Get_Total_Time(void);
    
    /*
    Sets the Accumulated time of the simulation
    */
    DSSPM_CAPI_DLL void Solution_Set_Total_Time(double Value);
    
    /*
    Get the solution process time + sample time for time step
    */
    DSSPM_CAPI_DLL double Solution_Get_Time_of_Step(void);
    
    /*
    Get/Set the Solution.IntervalHrs variable used for devices that integrate
    */
    DSSPM_CAPI_DLL double Solution_Get_IntervalHrs(void);
    
    /*
    Get/Set the Solution.IntervalHrs variable for custom solution algorithms
    */
    DSSPM_CAPI_DLL void Solution_Set_IntervalHrs(double Value);
    
    /*
    Minimum number of iterations required for a power flow solution.
    */
    DSSPM_CAPI_DLL int32_t Solution_Get_MinIterations(void);
    
    /*
    Mininum number of iterations required for a power flow solution.
    */
    DSSPM_CAPI_DLL void Solution_Set_MinIterations(int32_t Value);
    
    DSSPM_CAPI_DLL void Solution_SolveAll(void);
    
    DSSPM_CAPI_DLL void Solution_Get_IncMatrix(int32_t** ResultPtr, int32_t* ResultCount);
    
    DSSPM_CAPI_DLL void Solution_Get_BusLevels(int32_t** ResultPtr, int32_t* ResultCount);
    
    DSSPM_CAPI_DLL void Solution_Get_IncMatrixRows(char*** ResultPtr, int32_t* ResultCount);
    
    DSSPM_CAPI_DLL void Solution_Get_IncMatrixCols(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Open or Close the switch. No effect if switch is locked.  However, Reset removes any lock and then closes the switch (shelf state).
    */
    DSSPM_CAPI_DLL int32_t SwtControls_Get_Action(void);
    
    /*
    Array of strings with all SwtControl names in the active circuit.
    */
    DSSPM_CAPI_DLL void SwtControls_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Time delay [s] betwen arming and opening or closing the switch.  Control may reset before actually operating the switch.
    */
    DSSPM_CAPI_DLL double SwtControls_Get_Delay(void);
    
    /*
    Sets the first SwtControl active. Returns 0 if no more.
    */
    DSSPM_CAPI_DLL int32_t SwtControls_Get_First(void);
    
    /*
    The lock prevents both manual and automatic switch operation.
    */
    DSSPM_CAPI_DLL uint16_t SwtControls_Get_IsLocked(void);
    
    /*
    Sets a SwtControl active by Name.
    */
    DSSPM_CAPI_DLL char* SwtControls_Get_Name(void);
    
    /*
    Sets the next SwtControl active. Returns 0 if no more.
    */
    DSSPM_CAPI_DLL int32_t SwtControls_Get_Next(void);
    
    /*
    Full name of the switched element.
    */
    DSSPM_CAPI_DLL char* SwtControls_Get_SwitchedObj(void);
    
    /*
    Terminal number where the switch is located on the SwitchedObj
    */
    DSSPM_CAPI_DLL int32_t SwtControls_Get_SwitchedTerm(void);
    
    /*
    Open or Close the switch. No effect if switch is locked.  However, Reset removes any lock and then closes the switch (shelf state).
    */
    DSSPM_CAPI_DLL void SwtControls_Set_Action(int32_t Value);
    
    /*
    Time delay [s] betwen arming and opening or closing the switch.  Control may reset before actually operating the switch.
    */
    DSSPM_CAPI_DLL void SwtControls_Set_Delay(double Value);
    
    /*
    The lock prevents both manual and automatic switch operation.
    */
    DSSPM_CAPI_DLL void SwtControls_Set_IsLocked(uint16_t Value);
    
    /*
    Sets a SwtControl active by Name.
    */
    DSSPM_CAPI_DLL void SwtControls_Set_Name(char* Value);
    
    /*
    Full name of the switched element.
    */
    DSSPM_CAPI_DLL void SwtControls_Set_SwitchedObj(char* Value);
    
    /*
    Terminal number where the switch is located on the SwitchedObj
    */
    DSSPM_CAPI_DLL void SwtControls_Set_SwitchedTerm(int32_t Value);
    
    DSSPM_CAPI_DLL int32_t SwtControls_Get_Count(void);
    
    /*
    Get Normal state of switch
    */
    DSSPM_CAPI_DLL int32_t SwtControls_Get_NormalState(void);
    
    /*
    set Normal state of switch  (see actioncodes) dssActionOpen or dssActionClose
    */
    DSSPM_CAPI_DLL void SwtControls_Set_NormalState(int32_t Value);
    
    /*
    Force switch to specified state
    */
    DSSPM_CAPI_DLL int32_t SwtControls_Get_State(void);
    
    /*
    Get Present state of switch
    */
    DSSPM_CAPI_DLL void SwtControls_Set_State(int32_t Value);
    
    DSSPM_CAPI_DLL void SwtControls_Reset(void);
    
    /*
    Input command string for the DSS.
    */
    DSSPM_CAPI_DLL char* Text_Get_Command(void);
    
    /*
    Input command string for the DSS.
    */
    DSSPM_CAPI_DLL void Text_Set_Command(char* Value);
    
    /*
    Result string for the last command.
    */
    DSSPM_CAPI_DLL char* Text_Get_Result(void);
    
    /*
    Number of loops
    */
    DSSPM_CAPI_DLL int32_t Topology_Get_NumLoops(void);
    
    /*
    Returns index of the active branch
    */
    DSSPM_CAPI_DLL int32_t Topology_Get_ActiveBranch(void);
    
    /*
    Array of all isolated branch names.
    */
    DSSPM_CAPI_DLL void Topology_Get_AllIsolatedBranches(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Array of all looped element names, by pairs.
    */
    DSSPM_CAPI_DLL void Topology_Get_AllLoopedPairs(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    MOve back toward the source, return index of new active branch, or 0 if no more.
    */
    DSSPM_CAPI_DLL int32_t Topology_Get_BackwardBranch(void);
    
    /*
    Name of the active branch.
    */
    DSSPM_CAPI_DLL char* Topology_Get_BranchName(void);
    
    /*
    Sets the first branch active, returns 0 if none.
    */
    DSSPM_CAPI_DLL int32_t Topology_Get_First(void);
    
    /*
    Move forward in the tree, return index of new active branch or 0 if no more
    */
    DSSPM_CAPI_DLL int32_t Topology_Get_ForwardBranch(void);
    
    /*
    Move to looped branch, return index or 0 if none.
    */
    DSSPM_CAPI_DLL int32_t Topology_Get_LoopedBranch(void);
    
    /*
    Sets the next branch active, returns 0 if no more.
    */
    DSSPM_CAPI_DLL int32_t Topology_Get_Next(void);
    
    /*
    Number of isolated branches (PD elements and capacitors).
    */
    DSSPM_CAPI_DLL int32_t Topology_Get_NumIsolatedBranches(void);
    
    /*
    Move to directly parallel branch, return index or 0 if none.
    */
    DSSPM_CAPI_DLL int32_t Topology_Get_ParallelBranch(void);
    
    DSSPM_CAPI_DLL void Topology_Set_BranchName(char* Value);
    
    /*
    Array of all isolated load names.
    */
    DSSPM_CAPI_DLL void Topology_Get_AllIsolatedLoads(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    First load at the active branch, return index or 0 if none.
    */
    DSSPM_CAPI_DLL int32_t Topology_Get_FirstLoad(void);
    
    /*
    Next load at the active branch, return index or 0 if no more.
    */
    DSSPM_CAPI_DLL int32_t Topology_Get_NextLoad(void);
    
    /*
    Number of isolated loads
    */
    DSSPM_CAPI_DLL int32_t Topology_Get_NumIsolatedLoads(void);
    
    /*
    Topological depth of the active branch
    */
    DSSPM_CAPI_DLL int32_t Topology_Get_ActiveLevel(void);
    
    DSSPM_CAPI_DLL char* Topology_Get_BusName(void);
    
    /*
    Set the active branch to one containing this bus, return index or 0 if not found
    */
    DSSPM_CAPI_DLL void Topology_Set_BusName(char* Value);
    
    /*
    Array of strings with all Transformer names in the active circuit.
    */
    DSSPM_CAPI_DLL void Transformers_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Sets the first Transformer active. Returns 0 if no more.
    */
    DSSPM_CAPI_DLL int32_t Transformers_Get_First(void);
    
    /*
    Active Winding delta or wye connection?
    */
    DSSPM_CAPI_DLL uint16_t Transformers_Get_IsDelta(void);
    
    /*
    Active Winding kV rating.  Phase-phase for 2 or 3 phases, actual winding kV for 1 phase transformer.
    */
    DSSPM_CAPI_DLL double Transformers_Get_kV(void);
    
    /*
    Active Winding kVA rating. On winding 1, this also determines normal and emergency current ratings for all windings.
    */
    DSSPM_CAPI_DLL double Transformers_Get_kVA(void);
    
    /*
    Active Winding maximum tap in per-unit.
    */
    DSSPM_CAPI_DLL double Transformers_Get_MaxTap(void);
    
    /*
    Active Winding minimum tap in per-unit.
    */
    DSSPM_CAPI_DLL double Transformers_Get_MinTap(void);
    
    /*
    Sets a Transformer active by Name.
    */
    DSSPM_CAPI_DLL char* Transformers_Get_Name(void);
    
    /*
    Sets the next Transformer active. Returns 0 if no more.
    */
    DSSPM_CAPI_DLL int32_t Transformers_Get_Next(void);
    
    /*
    Active Winding number of tap steps betwein MinTap and MaxTap.
    */
    DSSPM_CAPI_DLL int32_t Transformers_Get_NumTaps(void);
    
    /*
    Number of windings on this transformer. Allocates memory; set or change this property first.
    */
    DSSPM_CAPI_DLL int32_t Transformers_Get_NumWindings(void);
    
    /*
    Active Winding resistance in %
    */
    DSSPM_CAPI_DLL double Transformers_Get_R(void);
    
    /*
    Active Winding neutral resistance [ohms] for wye connections. Set less than zero for ungrounded wye.
    */
    DSSPM_CAPI_DLL double Transformers_Get_Rneut(void);
    
    /*
    Active Winding tap in per-unit.
    */
    DSSPM_CAPI_DLL double Transformers_Get_Tap(void);
    
    /*
    Active Winding Number from 1..NumWindings. Update this before reading or setting a sequence of winding properties (R, Tap, kV, kVA, etc.)
    */
    DSSPM_CAPI_DLL int32_t Transformers_Get_Wdg(void);
    
    /*
    Name of an XfrmCode that supplies electircal parameters for this Transformer.
    */
    DSSPM_CAPI_DLL char* Transformers_Get_XfmrCode(void);
    
    /*
    Percent reactance between windings 1 and 2, on winding 1 kVA base. Use for 2-winding or 3-winding transformers.
    */
    DSSPM_CAPI_DLL double Transformers_Get_Xhl(void);
    
    /*
    Percent reactance between windigns 1 and 3, on winding 1 kVA base.  Use for 3-winding transformers only.
    */
    DSSPM_CAPI_DLL double Transformers_Get_Xht(void);
    
    /*
    Percent reactance between windings 2 and 3, on winding 1 kVA base. Use for 3-winding transformers only.
    */
    DSSPM_CAPI_DLL double Transformers_Get_Xlt(void);
    
    /*
    Active Winding neutral reactance [ohms] for wye connections.
    */
    DSSPM_CAPI_DLL double Transformers_Get_Xneut(void);
    
    /*
    Active Winding delta or wye connection?
    */
    DSSPM_CAPI_DLL void Transformers_Set_IsDelta(uint16_t Value);
    
    /*
    Active Winding kV rating.  Phase-phase for 2 or 3 phases, actual winding kV for 1 phase transformer.
    */
    DSSPM_CAPI_DLL void Transformers_Set_kV(double Value);
    
    /*
    Active Winding kVA rating. On winding 1, this also determines normal and emergency current ratings for all windings.
    */
    DSSPM_CAPI_DLL void Transformers_Set_kVA(double Value);
    
    /*
    Active Winding maximum tap in per-unit.
    */
    DSSPM_CAPI_DLL void Transformers_Set_MaxTap(double Value);
    
    /*
    Active Winding minimum tap in per-unit.
    */
    DSSPM_CAPI_DLL void Transformers_Set_MinTap(double Value);
    
    /*
    Sets a Transformer active by Name.
    */
    DSSPM_CAPI_DLL void Transformers_Set_Name(char* Value);
    
    /*
    Active Winding number of tap steps betwein MinTap and MaxTap.
    */
    DSSPM_CAPI_DLL void Transformers_Set_NumTaps(int32_t Value);
    
    /*
    Number of windings on this transformer. Allocates memory; set or change this property first.
    */
    DSSPM_CAPI_DLL void Transformers_Set_NumWindings(int32_t Value);
    
    /*
    Active Winding resistance in %
    */
    DSSPM_CAPI_DLL void Transformers_Set_R(double Value);
    
    /*
    Active Winding neutral resistance [ohms] for wye connections. Set less than zero for ungrounded wye.
    */
    DSSPM_CAPI_DLL void Transformers_Set_Rneut(double Value);
    
    /*
    Active Winding tap in per-unit.
    */
    DSSPM_CAPI_DLL void Transformers_Set_Tap(double Value);
    
    /*
    Active Winding Number from 1..NumWindings. Update this before reading or setting a sequence of winding properties (R, Tap, kV, kVA, etc.)
    */
    DSSPM_CAPI_DLL void Transformers_Set_Wdg(int32_t Value);
    
    /*
    Name of an XfrmCode that supplies electircal parameters for this Transformer.
    */
    DSSPM_CAPI_DLL void Transformers_Set_XfmrCode(char* Value);
    
    /*
    Percent reactance between windings 1 and 2, on winding 1 kVA base. Use for 2-winding or 3-winding transformers.
    */
    DSSPM_CAPI_DLL void Transformers_Set_Xhl(double Value);
    
    /*
    Percent reactance between windigns 1 and 3, on winding 1 kVA base.  Use for 3-winding transformers only.
    */
    DSSPM_CAPI_DLL void Transformers_Set_Xht(double Value);
    
    /*
    Percent reactance between windings 2 and 3, on winding 1 kVA base. Use for 3-winding transformers only.
    */
    DSSPM_CAPI_DLL void Transformers_Set_Xlt(double Value);
    
    /*
    Active Winding neutral reactance [ohms] for wye connections.
    */
    DSSPM_CAPI_DLL void Transformers_Set_Xneut(double Value);
    
    DSSPM_CAPI_DLL int32_t Transformers_Get_Count(void);
    
    /*
    Names of all Vsource objects in the circuit
    */
    DSSPM_CAPI_DLL void Vsources_Get_AllNames(char*** ResultPtr, int32_t* ResultCount);
    
    /*
    Number of Vsource Object
    */
    DSSPM_CAPI_DLL int32_t Vsources_Get_Count(void);
    
    /*
    Sets the first VSOURCE to be active; Returns 0 if none
    */
    DSSPM_CAPI_DLL int32_t Vsources_Get_First(void);
    
    /*
    Sets the next VSOURCE object to be active; returns zero if no more
    */
    DSSPM_CAPI_DLL int32_t Vsources_Get_Next(void);
    
    /*
    Get Active VSOURCE name
    */
    DSSPM_CAPI_DLL char* Vsources_Get_Name(void);
    
    /*
    Set Active VSOURCE by Name
    */
    DSSPM_CAPI_DLL void Vsources_Set_Name(char* Value);
    
    /*
    Source Voltage in kV
    */
    DSSPM_CAPI_DLL double Vsources_Get_BasekV(void);
    
    /*
    Source pu voltage.
    */
    DSSPM_CAPI_DLL double Vsources_Get_pu(void);
    
    /*
    Source voltage in kV
    */
    DSSPM_CAPI_DLL void Vsources_Set_BasekV(double Value);
    
    /*
    Per-unit value of source voltage based on kV
    */
    DSSPM_CAPI_DLL void Vsources_Set_pu(double Value);
    
    /*
    Phase angle of first phase in degrees
    */
    DSSPM_CAPI_DLL double Vsources_Get_AngleDeg(void);
    
    /*
    Source Frequency in Hz
    */
    DSSPM_CAPI_DLL double Vsources_Get_Frequency(void);
    
    /*
    Number of Phases
    */
    DSSPM_CAPI_DLL int32_t Vsources_Get_Phases(void);
    
    /*
    phase angle in degrees
    */
    DSSPM_CAPI_DLL void Vsources_Set_AngleDeg(double Value);
    
    /*
    Source frequency in Hz
    */
    DSSPM_CAPI_DLL void Vsources_Set_Frequency(double Value);
    
    /*
    Number of phases
    */
    DSSPM_CAPI_DLL void Vsources_Set_Phases(int32_t Value);
    
    /*
    Number of XYCurve Objects
    */
    DSSPM_CAPI_DLL int32_t XYCurves_Get_Count(void);
    
    /*
    Sets first XYcurve object active; returns 0 if none.
    */
    DSSPM_CAPI_DLL int32_t XYCurves_Get_First(void);
    
    /*
    Name of active XYCurve Object
    */
    DSSPM_CAPI_DLL char* XYCurves_Get_Name(void);
    
    /*
    Advances to next XYCurve object; returns 0 if no more objects of this class
    */
    DSSPM_CAPI_DLL int32_t XYCurves_Get_Next(void);
    
    /*
    Get Name of active XYCurve Object
    */
    DSSPM_CAPI_DLL void XYCurves_Set_Name(char* Value);
    
    /*
    Get/Set Number of points in X-Y curve
    */
    DSSPM_CAPI_DLL int32_t XYCurves_Get_Npts(void);
    
    /*
    Get/Set X values as a Array of doubles. Set Npts to max number expected if setting
    */
    DSSPM_CAPI_DLL void XYCurves_Get_Xarray(double** ResultPtr, int32_t* ResultCount);
    
    /*
    Get/Set Number of Points in X-Y curve
    */
    DSSPM_CAPI_DLL void XYCurves_Set_Npts(int32_t Value);
    
    /*
    Get/Set X values as a Array of doubles. Set Npts to max number expected if setting
    */
    DSSPM_CAPI_DLL void XYCurves_Set_Xarray(double* ValuePtr, int32_t ValueCount);
    
    /*
    Set X value or get interpolated value after setting Y
    */
    DSSPM_CAPI_DLL double XYCurves_Get_x(void);
    
    /*
    Y value for present X or set this value then get corresponding X
    */
    DSSPM_CAPI_DLL double XYCurves_Get_y(void);
    
    /*
    Get/Set Y values in curve; Set Npts to max number expected if setting
    */
    DSSPM_CAPI_DLL void XYCurves_Get_Yarray(double** ResultPtr, int32_t* ResultCount);
    
    DSSPM_CAPI_DLL void XYCurves_Set_x(double Value);
    
    /*
    Set Y value or get interpolated Y value after setting X
    */
    DSSPM_CAPI_DLL void XYCurves_Set_y(double Value);
    
    /*
    Factor to scale X values from original curve
    */
    DSSPM_CAPI_DLL double XYCurves_Get_Xscale(void);
    
    /*
    Amount to shift X value from original curve
    */
    DSSPM_CAPI_DLL double XYCurves_Get_Xshift(void);
    
    /*
    Factor to scale Y values from original curve
    */
    DSSPM_CAPI_DLL double XYCurves_Get_Yscale(void);
    
    /*
    amount to shift Y valiue from original curve
    */
    DSSPM_CAPI_DLL double XYCurves_Get_Yshift(void);
    
    /*
    Factor to scale X values from original curve
    */
    DSSPM_CAPI_DLL void XYCurves_Set_Xscale(double Value);
    
    DSSPM_CAPI_DLL void XYCurves_Set_Xshift(double Value);
    
    /*
    Amount to scale Y values from original curve. Represents a curve shift.
    */
    DSSPM_CAPI_DLL void XYCurves_Set_Yscale(double Value);
    
    DSSPM_CAPI_DLL void XYCurves_Set_Yshift(double Value);

    DSSPM_CAPI_DLL void YMatrix_GetCompressedYMatrix(uint16_t factor, uint32_t *nBus, uint32_t *nNz, int32_t **ColPtr, int32_t **RowIdxPtr, double **cValsPtr);
    DSSPM_CAPI_DLL void YMatrix_ZeroInjCurr(void);
    DSSPM_CAPI_DLL void YMatrix_GetSourceInjCurrents(void);
    DSSPM_CAPI_DLL void YMatrix_GetPCInjCurr(void);
    DSSPM_CAPI_DLL void YMatrix_BuildYMatrixD(int32_t BuildOps, int32_t AllocateVI);
    DSSPM_CAPI_DLL void YMatrix_AddInAuxCurrents(int32_t SType);
    DSSPM_CAPI_DLL void YMatrix_getIpointer(double **IvectorPtr);
    DSSPM_CAPI_DLL void YMatrix_getVpointer(double **VvectorPtr);
    DSSPM_CAPI_DLL int32_t YMatrix_SolveSystem(double **NodeVPtr);
    DSSPM_CAPI_DLL void YMatrix_Set_SystemYChanged(uint16_t arg);
    DSSPM_CAPI_DLL uint16_t YMatrix_Get_SystemYChanged(void);
    DSSPM_CAPI_DLL void YMatrix_Set_UseAuxCurrents(uint16_t arg);
    DSSPM_CAPI_DLL uint16_t YMatrix_Get_UseAuxCurrents(void);
    

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

#ifdef __cplusplus
}
#endif
#endif
