/*! \file enums.h */
#ifndef ALTDSS_CAPI_ENUMS_H
#define ALTDSS_CAPI_ENUMS_H

#ifdef __cplusplus
#ifdef ALTDSS_CAPI_NAMESPACE
namespace altdss { namespace capi {
#endif
extern "C" {
#else
#endif
    enum MonitorModes {
        MonitorModes_VI = 0x00000000, ///< Monitor records Voltage and Current at the terminal (Default)
        MonitorModes_Power = 0x00000001, ///< Monitor records kW, kvar or kVA, angle values, etc. at the terminal to which it is connected.
        MonitorModes_Taps = 0x00000002, ///< For monitoring Regulator and Transformer taps
        MonitorModes_States = 0x00000003, ///< For monitoring State Variables (for PC Elements only)
        MonitorModes_Sequence = 0x00000010, ///< Reports the monitored quantities as sequence quantities
        MonitorModes_Magnitude = 0x00000020, ///< Reports the monitored quantities in Magnitude Only
        MonitorModes_PosOnly = 0x00000040 ///< Reports the positive-sequence only or avg of all phases
    };

    enum SolveModes {
        SolveModes_SnapShot = 0, ///< Solve a single snapshot power flow
        SolveModes_Daily = 1, ///< Solve following Daily load shapes
        SolveModes_Yearly = 2, ///< Solve following Yearly load shapes
        SolveModes_Monte1 = 3, ///< Monte Carlo Mode 1
        SolveModes_LD1 = 4, ///< Load-duration Mode 1
        SolveModes_PeakDay = 5, ///< Solves for Peak Day using Daily load curve
        SolveModes_DutyCycle = 6, ///< Solve following Duty Cycle load shapes
        SolveModes_Direct = 7, ///< Solve direct (forced admittance model)
        SolveModes_MonteFault = 8, ///< Monte Carlo Fault Study
        SolveModes_FaultStudy = 9, ///< Fault study at all buses
        SolveModes_Monte2 = 10, ///< Monte Carlo Mode 2
        SolveModes_Monte3 = 11, ///< Monte Carlo Mode 3
        SolveModes_LD2 = 12, ///< Load-Duration Mode 2
        SolveModes_AutoAdd = 13, ///< Auto add generators or capacitors
        SolveModes_Dynamic = 14, ///< Solve for dynamics
        SolveModes_Harmonic = 15 ///< Harmonic solution mode
    };

    enum SolutionLoadModels { ///< Solution_[Get/Set]_LoadModel
        SolutionLoadModels_PowerFlow = 1, ///< Power Flow load model option
        SolutionLoadModels_Admittance = 2 ///< Admittance load model option
    };

    enum SolutionAlgorithms { ///< Solution_[Get/Set]_Algorithm
        SolutionAlgorithms_NormalSolve = 0, ///< Solution algorithm option - Normal solution mode
        SolutionAlgorithms_NewtonSolve = 1 ///< Solution algorithm option - Newton solution
    };

    enum ControlModes { ///< Solution_[Get/Set]_ControlMode
        ControlModes_Static = 0, ///< Control Mode option - Static
        ControlModes_Event = 1, ///< Control Mode Option - Event driven solution mode
        ControlModes_Time = 2, ///< Control mode option - Time driven mode
        ControlModes_Multirate = 3, ///< Control mode option - Multirate mode
        ControlModes_ControlOff = -1 ///< Control Mode OFF
    };

    enum CktModels { ///< Settings_[Get/Set]_CktModel
        CktModels_Multiphase = 0, ///< Circuit model is multiphase (default)
        CktModels_PositiveSeq = 1 ///< Circuit model is positive-sequence model only
    };

    enum RandomModes { ///< Solution_[Get/Set]_Random
        RandomModes_Gaussian = 1, ///< Gaussian
        RandomModes_Uniform = 2, ///< Uniform
        RandomModes_LogNormal = 3 ///< Log normal
    };

    enum AutoAddTypes { ///< Solution_[Get/Set]_AddType
        AutoAddTypes_AddGen = 1, ///< Add generators in AutoAdd mode
        AutoAddTypes_AddCap = 2 ///< Add capacitors in AutoAdd mode
    };

    enum CapControlModes {
        CapControlModes_Current = 0, ///< Current control, ON and OFF settings on CT secondary
        CapControlModes_Voltage = 1, ///< Voltage control, ON and OFF settings on the PT secondary base
        CapControlModes_KVAR = 2, ///< kVAR control, ON and OFF settings on PT / CT base
        CapControlModes_Time = 3, ///< Time control, ON and OFF settings are seconds from midnight
        CapControlModes_PF = 4 ///< ON and OFF settings are power factor, negative for leading
    };

    enum ActionCodes {
        ActionCodes_none = 0, ///< No action
        ActionCodes_Open = 1, ///< Open a switch
        ActionCodes_Close = 2, ///< Close a switch
        ActionCodes_Reset = 3, ///< Reset to the shelf state (unlocked, closed for a switch)
        ActionCodes_Lock = 4, ///< Lock a switch, preventing both manual and automatic operation
        ActionCodes_Unlock = 5, ///< Unlock a switch, permitting both manual and automatic operation
        ActionCodes_TapUp = 6, ///< Move a regulator tap up
        ActionCodes_TapDown = 7 ///< Move a regulator tap down
    };

    enum GeneratorStatus {
        GeneratorStatus_Variable = 0,
        GeneratorStatus_Fixed = 1
    };

    enum LoadStatus {
        LoadStatus_Variable = 0,
        LoadStatus_Fixed = 1,
        LoadStatus_Exempt = 2
    };

    enum LoadModels {
        LoadModels_ConstPQ = 1,
        LoadModels_ConstZ = 2,
        LoadModels_Motor = 3,
        LoadModels_CVR = 4,
        LoadModels_ConstI = 5,
        LoadModels_ConstPFixedQ = 6,
        LoadModels_ConstPFixedX = 7,
        LoadModels_ZIPV = 8
    };

    enum LineUnits {
        LineUnits_none = 0, ///< No line length unit.
        LineUnits_Miles = 1, ///< Line length units in miles.
        LineUnits_kFt = 2, ///< Line length units are in thousand feet.
        LineUnits_km = 3, ///< Line length units are km.
        LineUnits_meter = 4, ///< Line length units are meters.
        LineUnits_ft = 5, ///< Line units in feet.
        LineUnits_inch = 6, ///< Line length units are inches.
        LineUnits_cm = 7, ///< Line units are cm.
        LineUnits_mm = 8, ///< Line length units are mm.
        LineUnits_Maxnum = 9 ///< Maximum number of line units constants.
    };

    enum YMatrixModes { ///< Solution_BuildYMatrix, YMatrix_BuildYMatrixD
        YMatrixModes_SeriesOnly = 1,
        YMatrixModes_WholeMatrix = 2
    };

    enum StorageStates { ///< Storages_Set_State, Storages_Get_State
        StorageStates_Charging = -1,
        StorageStates_Idling = 0,
        StorageStates_Discharging = 1
    };

    /*!
    \brief Energy meter registers

    This enumeration lists the basic energy meter registers. Extra registers start
    at `VBaseStart`. This is exposed to make it easier to access common registers
    without needing to check the register names every time, plus makes it safer to
    access the registers by index directly without introducing bugs we found in
    OpenDSS code (both user code and engine code) in the past due to direct use
    of magic numbers.
    */
    enum EnergyMeterRegisters {
        EnergyMeterRegisters_kWh = 0,
        EnergyMeterRegisters_kvarh = 1,
        EnergyMeterRegisters_MaxkW = 2,
        EnergyMeterRegisters_MaxkVA = 3,
        EnergyMeterRegisters_ZonekWh = 4,
        EnergyMeterRegisters_Zonekvarh = 5,
        EnergyMeterRegisters_ZoneMaxkW = 6,
        EnergyMeterRegisters_ZoneMaxkVA = 7,
        EnergyMeterRegisters_OverloadkWhNorm = 8,
        EnergyMeterRegisters_OverloadkWhEmerg = 9,
        EnergyMeterRegisters_LoadEEN = 10,
        EnergyMeterRegisters_LoadUE = 11,
        EnergyMeterRegisters_ZoneLosseskWh = 12,
        EnergyMeterRegisters_ZoneLosseskvarh = 13,
        EnergyMeterRegisters_LossesMaxkW = 14,
        EnergyMeterRegisters_LossesMaxkvar = 15,
        EnergyMeterRegisters_LoadLosseskWh = 16,
        EnergyMeterRegisters_LoadLosseskvarh = 17,
        EnergyMeterRegisters_NoLoadLosseskWh = 18,
        EnergyMeterRegisters_NoLoadLosseskvarh = 19,
        EnergyMeterRegisters_MaxLoadLosses = 20,
        EnergyMeterRegisters_MaxNoLoadLosses = 21,
        EnergyMeterRegisters_LineLosseskWh = 22,
        EnergyMeterRegisters_TransformerLosseskWh = 23,
        EnergyMeterRegisters_LineModeLineLoss = 24,
        EnergyMeterRegisters_ZeroModeLineLoss = 25,
        EnergyMeterRegisters_ThreePhaseLineLoss = 26,
        EnergyMeterRegisters_OnePhaseLineLoss = 27,
        EnergyMeterRegisters_GenkWh = 28,
        EnergyMeterRegisters_Genkvarh = 29,
        EnergyMeterRegisters_GenMaxkW = 30,
        EnergyMeterRegisters_GenMaxkVA = 31,
        EnergyMeterRegisters_VBaseStart = 32 // anchor for the voltage base loss registers
    };

    /*!
    \brief Generator registers

    Enumeration of the generator registers by index. 
    Currently shared between the Generator, Storage and PVSystem models.
    */
    enum GeneratorRegisters {
        GeneratorRegisters_kWh = 0,
        GeneratorRegisters_kvarh = 1,
        GeneratorRegisters_MaxkW = 2,
        GeneratorRegisters_MaxkVA = 3,
        GeneratorRegisters_Hours = 4,
        GeneratorRegisters_Price = 5
    };

    /*!
    \brief Generator variables

    Enumeration of the generator state variables by (1-based) index.
    This is the implicit list and there can be more variables used by user-models
    and DynamicExp objects.
    */
    enum GeneratorVariables {
        GeneratorVariables_Frequency = 1, ///< Frequency (Hz)
        GeneratorVariables_Theta = 2, ///< Theta (degrees)
        GeneratorVariables_Vd = 3, ///< Vd (pu)
        GeneratorVariables_PShaft = 4, ///< PShaft (W)
        GeneratorVariables_dSpeed = 5, ///< dSpeed (degrees/second)
        GeneratorVariables_dTheta = 6 ///< dTheta (degrees)
    };

    /*!
    \brief IndMach012 variables

    Enumeration of the IndMach012 state variables by (1-based) index.
    */
    enum IndMach012Variables {
        IndMach012Variables_Frequency = 1, ///< Frequency
        IndMach012Variables_Theta = 2, ///< Theta (deg)
        IndMach012Variables_E1 = 3, ///< E1
        IndMach012Variables_Pshaft = 4, ///< Pshaft
        IndMach012Variables_dSpeed = 5, ///< dSpeed (deg/sec)
        IndMach012Variables_dTheta = 6, ///< dTheta (deg)
        IndMach012Variables_Slip = 7, ///< Slip
        IndMach012Variables_puRs = 8, ///< puRs
        IndMach012Variables_puXs = 9, ///< puXs
        IndMach012Variables_puRr = 10, ///< puRr
        IndMach012Variables_puXr = 11, ///< puXr
        IndMach012Variables_puXm = 12, ///< puXm
        IndMach012Variables_MaxSlip = 13, ///< Maxslip
        IndMach012Variables_Is1 = 14, ///< Is1
        IndMach012Variables_Is2 = 15, ///< Is2
        IndMach012Variables_Ir1 = 16, ///< Ir1
        IndMach012Variables_Ir2 = 17, ///< Ir2
        IndMach012Variables_StatorLosses = 18, ///< Stator Losses
        IndMach012Variables_RotorLosses = 19, ///< Rotor Losses
        IndMach012Variables_ShaftPowerHP = 20, ///< Shaft Power (hp)
        IndMach012Variables_PowerFactor = 21, ///< Power Factor
        IndMach012Variables_Efficiency = 22 ///< Efficiency (%)
    };

    /*!
    \brief PVSystem variables

    Enumeration of the PVSystem state variables by (1-based) index.
    This is the implicit list and there can be more variables used by user-models
    and DynamicExp objects.
    */
    enum PVSystemVariables {
        PVSystemVariables_Irradiance = 1, ///< Irradiance
        PVSystemVariables_PanelkW = 2, ///< PanelkW
        PVSystemVariables_P_TFactor = 3, ///< P_TFactor
        PVSystemVariables_Efficiency = 4, ///< Efficiency
        PVSystemVariables_Vreg = 5, ///< Vreg
        PVSystemVariables_Vavg_DRC = 6, ///< Vavg (DRC)
        PVSystemVariables_volt_var = 7, ///< volt-var
        PVSystemVariables_volt_watt = 8, ///< volt-watt
        PVSystemVariables_DRC = 9, ///< DRC
        PVSystemVariables_VV_DRC = 10, ///< VV_DRC
        PVSystemVariables_watt_pf = 11, ///< watt-pf
        PVSystemVariables_watt_var = 12, ///< watt-var
        PVSystemVariables_kW_out_desired = 13, ///< kW_out_desired
        PVSystemVariables_GridVoltage = 14, ///< Grid voltage
        PVSystemVariables_di_dt = 15, ///< di/dt
        PVSystemVariables_it = 16, ///< it
        PVSystemVariables_itHistory = 17, ///< it History
        PVSystemVariables_RatedVDC = 18, ///< Rated VDC
        PVSystemVariables_AvgDutyCycle = 19, ///< Avg duty cycle
        PVSystemVariables_Target_Amps = 20, ///< Target (Amps)
        PVSystemVariables_SeriesL = 21, ///< Series L
        PVSystemVariables_MaxAmps_phase = 22 ///< Max. Amps (phase)
    };

    /*!
    \brief Storage variables

    Enumeration of the Storage state variables by (1-based) index.
    This is the implicit list and there can be more variables used by user-models
    and DynamicExp objects.
    */
    enum StorageVariables {
        StorageVariables_kWh = 1, ///< kWh
        StorageVariables_State = 2, ///< State
        StorageVariables_kWOut = 3, ///< kWOut
        StorageVariables_kWIn = 4, ///< kWIn
        StorageVariables_kvarOut = 5, ///< kvarOut
        StorageVariables_DCkW = 6, ///< DCkW
        StorageVariables_kWTotalLosses = 7, ///< kWTotalLosses
        StorageVariables_kWInvLosses = 8, ///< kWInvLosses
        StorageVariables_kWIdlingLosses = 9, ///< kWIdlingLosses
        StorageVariables_kWChDchLosses = 10, ///< kWChDchLosses
        StorageVariables_kWhChng = 11, ///< kWh Chng
        StorageVariables_InvEff = 12, ///< InvEff
        StorageVariables_InverterON = 13, ///< InverterON
        StorageVariables_Vref = 14, ///< Vref
        StorageVariables_Vavg_DRC = 15, ///< Vavg (DRC)
        StorageVariables_VV_Oper = 16, ///< VV Oper
        StorageVariables_VW_Oper = 17, ///< VW Oper
        StorageVariables_DRC_Oper = 18, ///< DRC Oper
        StorageVariables_VV_DRC_Oper = 19, ///< VV_DRC Oper
        StorageVariables_WP_Oper = 20, ///< WP Oper
        StorageVariables_WV_Oper = 21, ///< WV Oper
        StorageVariables_kWDesired = 22, ///< kWDesired
        StorageVariables_kW_VW_Limit = 23, ///< kW VW Limit
        StorageVariables_Limit_kWOut_Function = 24, ///< Limit kWOut Function
        StorageVariables_kVA_Exceeded = 25, ///< kVA Exceeded
        StorageVariables_GridVoltage = 26, ///< Grid voltage
        StorageVariables_di_dt = 27, ///< di/dt
        StorageVariables_it = 28, ///< it
        StorageVariables_itHistory = 29, ///< it History
        StorageVariables_RatedVDC = 30, ///< Rated VDC
        StorageVariables_AvgDutyCycle = 31, ///< Avg duty cycle
        StorageVariables_Target_Amps = 32, ///< Target (Amps)
        StorageVariables_SeriesL = 33, ///< Series L
        StorageVariables_MaxAmps_phase = 34 ///< Max. Amps (phase)
    };


    /*!
    \brief UPFC variables

    Enumeration of the UPFC state variables by (1-based) index.
    */
    enum UPFCVariables {
        UPFCVariables_ModeUPFC = 1, ///< ModeUPFC
        UPFCVariables_IUPFC = 2, ///< IUPFC
        UPFCVariables_Re_Vbin = 3, ///< Re{Vbin}
        UPFCVariables_Im_Vbin = 4, ///< Im{Vbin}
        UPFCVariables_Re_Vbout = 5, ///< Re{Vbout}
        UPFCVariables_Im_Vbout = 6, ///< Im{Vbout}
        UPFCVariables_Loses= 7, ///< Losses
        UPFCVariables_P_UPFC = 8, ///< P_UPFC
        UPFCVariables_Q_UPFC = 9, ///< Q_UPFC
        UPFCVariables_Qideal = 10, ///< Qideal
        UPFCVariables_Re_Sr0_1 = 11, ///< Re{Sr0^[1]}
        UPFCVariables_Im_Sr0_1 = 12, ///< Im{Sr0^[1]}
        UPFCVariables_Re_Sr1_1 = 13, ///< Re{Sr1^[1]}
        UPFCVariables_Im_Sr1_1 = 14 ///< Im{Sr1^[1]}
    };

    /*!
    \brief VCCS RMS variables

    Enumeration of the VCCS state variables by (1-based) index, when used in RMS mode (`RMSMode=true`).
    */
    enum VCCSRMSVariables {
        VCCSRMSVariables_Vrms = 1, ///< Vrms
        VCCSRMSVariables_Ipwr = 2, ///< Ipwr
        VCCSRMSVariables_Hout = 3, ///< Hout
        VCCSRMSVariables_Irms = 4 ///< Irms
    };

    /*!
    \brief VCCS non-RMS variables

    Enumeration of the VCCS state variables by (1-based) index, when used in non-RMS mode (`RMSMode=false`).
    */
    enum VCCSNonRMSVariables {
        VCCSNonRMSVariables_Vwave = 1, ///< Vwave
        VCCSNonRMSVariables_Iwave = 2, ///< Iwave
        VCCSNonRMSVariables_Irms = 3, ///< Irms
        VCCSNonRMSVariables_Ipeak = 4, ///< Ipeak
        VCCSNonRMSVariables_BP1out = 5, ///< BP1out
        VCCSNonRMSVariables_Hout = 6 ///< Hout
    };

    /// EXPERIMENTAL: For message/write callbacks
    enum DSSMessageType {
        DSSMessageType_Error = -1,
        DSSMessageType_General = 0,
        DSSMessageType_Info = 1,
        DSSMessageType_Help = 2,
        DSSMessageType_Progress = 3,
        DSSMessageType_ProgressCaption = 4,
        DSSMessageType_ProgressFormCaption = 5,
        DSSMessageType_ProgressPercent = 6,
        DSSMessageType_FireOffEditor = 7,
        DSSMessageType_ProgressSummary = 8,
        DSSMessageType_ReportOutput = 9,
        DSSMessageType_ShowOutput = 10,
        DSSMessageType_ShowTreeView = 11
    };

    enum DSSJSONFlags {
        DSSJSONFlags_Full = 0x00000001, ///< Return all properties, regardless of order or if the property was filled by the user
        DSSJSONFlags_SkipRedundant = 0x00000002, ///< Skip redundant properties
        DSSJSONFlags_EnumAsInt = 0x00000004, ///< Return enums as integers instead of strings
        DSSJSONFlags_FullNames = 0x00000008, ///< Use full names for the elements, including the class name
        DSSJSONFlags_Pretty = 0x00000010, ///< Try to "pretty" format the JSON output
        DSSJSONFlags_ExcludeDisabled = 0x00000020, ///< Exclude disabled elements (only valid when exporting a collection)
        DSSJSONFlags_IncludeDSSClass = 0x00000040, ///< Add the "DSSClass" property to the output objects
        DSSJSONFlags_LowercaseKeys = 0x00000080, ///< Use lowercase representation for the property names (and other keys) instead of the internal variants.
        DSSJSONFlags_IncludeDefaultObjs = 0x00000100, ///< Include default unchanged objects in the exports. Any default object that has been edited is always exported. Affects whole circuit and batch exports.
        DSSJSONFlags_SkipTimestamp = 0x00000200, ///< Skip timestamp/version comment, which is added a pre-command by default. Affects whole circuit exports.
        DSSJSONFlags_SkipBuses = 0x00000400 ///< Skip exporting buses. Affects whole circuit exports.
        // DSSJSONFlags_State = 0x00000800, ///< NOT IMPLEMENTED, avoid using until it's implemented.
        // DSSJSONFlags_Debug = 0x00001000 ///< NOT IMPLEMENTED, avoid using until it's implemented.
    };

    enum DSSPropertyNameStyle {
        DSSPropertyNameStyle_Modern = 0, ///< By default, the modern names are used. The names were reviewed to try to reach a convention across all components.
        DSSPropertyNameStyle_Lowercase = 1, ///< Use all lowercase strings.
        DSSPropertyNameStyle_Legacy = 2 ///< Use the previous capitalization of the property names.
    };

    /*!
    DSSSaveFlags are bit flags used in the Circuit_Save function to
    customize the saved circuit.
    */
    enum DSSSaveFlags {
        DSSSaveFlags_CalcVoltageBases = 0x0001, ///< Include the command CalcVoltageBases.
        DSSSaveFlags_SetVoltageBases = 0x0002, ///< Include commands to set the voltage bases individually.
        DSSSaveFlags_IncludeOptions = 0x0004, ///< Include most of the options (from the Set/Get DSS commands).
        DSSSaveFlags_IncludeDisabled = 0x0008, ///< Include disabled circuit elements (and LoadShapes).
        DSSSaveFlags_ExcludeDefault = 0x0010, ///< Exclude default DSS items if they are not modified by the user.
        DSSSaveFlags_SingleFile = 0x0020, ///< Use a single file instead of a folder for output.
        DSSSaveFlags_KeepOrder = 0x0040, ///< Save the circuit elements in the order they were loaded in the active circuit. Guarantees better reproducibility, especially when the system is ill-conditioned. Requires "SingleFile" flag.
        DSSSaveFlags_ExcludeMeterZones = 0x0080, ///< Do not export meter zones (as "feeders") separately. Has no effect when using a single file.
        DSSSaveFlags_IsOpen = 0x0100, ///< Export commands to open terminals of elements.
        DSSSaveFlags_ToString = 0x0200 ///< Export to the result string. Requires "SingleFile" flag.
    };

    enum BatchOperation {
        BatchOperation_Set = 0,
        BatchOperation_Multiply = 1,
        BatchOperation_Increment = 2,
        BatchOperation_Divide = 3
    };

    /// The values themselves are subject to change in future versions,
    /// use this enum for easier upgrades
    enum SolverOptions {
        SolverOptions_ReuseNothing = 0,
        SolverOptions_ReuseCompressedMatrix = 1, ///< Reuse only the prepared CSC matrix
        SolverOptions_ReuseSymbolicFactorization = 2, ///< Reuse the symbolic factorization, implies ReuseCompressedMatrix
        SolverOptions_ReuseNumericFactorization = 3, ///< Reuse the numeric factorization, implies ReuseSymbolicFactorization
        SolverOptions_AlwaysResetYPrimInvalid = 0x10000000 ///< Bit flag, see CktElement.pas
    };

    enum DSSCompatFlags {
        DSSCompatFlags_NoSolverFloatChecks = 0x00000001, /*!< 
            If enabled, don't check for NaNs in the inner solution loop. 
            This can lead to various errors. 
            This flag is useful for legacy applications that don't handle OpenDSS API errors properly.
            Through the development of DSS-Extensions, we noticed this is actually a quite common issue.
        */

        DSSCompatFlags_BadPrecision = 0x00000002, /*!< 
            If enabled, toggle worse precision for certain aspects of the engine. For example, the sequence-to-phase 
            (`As2p`) and sequence-to-phase (`Ap2s`) transform matrices. On DSS C-API, we fill the matrix explicitly
            using higher precision, while numerical inversion of an initially worse precision matrix is used in the 
            official OpenDSS. We will introduce better precision for other aspects of the engine in the future, 
            so this flag can be used to toggle the old/bad values where feasible.
        */

        DSSCompatFlags_InvControl9611 = 0x00000004, /*!< 
            Toggle some InvControl behavior introduced in OpenDSS 9.6.1.1. It was confirmed as a 
            regression and was fixed in OpenDSS v10. The flag still has effects for a few more
            releases, in case users need to investigate differences across versions.
        */

       DSSCompatFlags_SaveCalcVoltageBases = 0x00000008, /*!< 
            When using "save circuit", the official OpenDSS always includes the "CalcVoltageBases" command in the
            saved script. We found that it is not always a good idea, so we removed the command (leaving it commented).
            Use this flag to enable the command in the saved script.
        */

       DSSCompatFlags_ActiveLine = 0x00000010, /*!< 
            In the official OpenDSS implementation, the Lines API use the active circuit element instead of the
            active line. This can lead to unexpected behavior if the user is not aware of this detail.
            For example, if the user accidentally enables any other circuit element, the next time they use
            the Lines API, the line object that was previously enabled is overwritten with another unrelated
            object.
            This flag enables this behavior above if compatibility at this level is required. On DSS-Extensions,
            we changed the behavior to follow what most of the other APIs do: use the active object in the internal
            list. This change was done for DSS C-API v0.13.5, as well as the introduction of this flag.
        */

       DSSCompatFlags_NoPropertyTracking = 0x00000020, /*!< 
            On DSS-Extensions/AltDSS, when setting a property invalidates a previous input value, the engine
            will try to mark the invalidated data as unset. This allows for better exports and tracking of 
            the current state of DSS objects.
            Set this flag to disable this behavior, following the original OpenDSS implementation for potential
            compatibility with older software that may require the original behavior; note that may lead to
            erroneous interpretation of the data in the DSS properties. This was introduced in DSS C-API v0.14.0
            and will be further developed for future versions.
        */

       DSSCompatFlags_SkipSideEffects = 0x00000040, /*!< 
            Some specific functions on the official OpenDSS APIs and internal code skip important side-effects.
            By default, on DSS-Extensions/AltDSS, those side-effects are enabled. Use this flag
            to try to follow the behavior of the official APIs. Beware that some side-effects are
            important and skipping them may result in incorrect results.
            This flag affects some of the classic API functions (Loads, Generators, Vsources)
            as well as the behavior of some DSS properties (Line: Rg, Xg, rho, Transformer/AutoTrans: XscArray).
        */

       DSSCompatFlags_MonitorHeader = 0x00000080, /*!< 
            Add extra spaces (and trailing comma) to the monitor headers to match the official OpenDSS implementation.
            This affects both the Header function/property in the API, and the exported CSVs.

            The extra spaces can cause issues with third-party software. For example, Pandas adds 
            an extra empty column for monitor exports, and keeps the spaces in the column names.
            This typically requires extra steps to both remove the spaces in the column names, and 
            discard the extra column.
        */

        DSSCompatFlags_InvControlDeltaV = 0x00000100 /*!<
            An issue with the voltage delta across iterations was found and fixed in AltDSS/DSS C-API 0.15.0.
            Use this flag to restore the previous behavior, which also matches the official OpenDSS.

            The issue affects situations where an InvControl object tracks multiple DERs, while using one of the volt-var modes.
            It is not always apparent and does not always affect the end results.
        */
    };

    /*!
    Object flags are bit flags used by various of the internal processes of the DSS engine.

    Most are internal state, but advanced/expert users can manipulate them for some interesting uses.
    */
    enum DSSObjectFlags {
        DSSObjectFlags_Editing = 0x0001,
        DSSObjectFlags_HasBeenSaved = 0x0002,
        DSSObjectFlags_DefaultAndUnedited = 0x0004,
        DSSObjectFlags_Checked = 0x0008,
        DSSObjectFlags_Flag = 0x0010, ///< General purpose flag for each object
        DSSObjectFlags_HasEnergyMeter = 0x0020,
        DSSObjectFlags_HasSensorObj = 0x0040,
        DSSObjectFlags_IsIsolated = 0x0080,
        DSSObjectFlags_HasControl = 0x0100,
        DSSObjectFlags_IsMonitored = 0x0200, ///< Indicates some control is monitoring this element
        DSSObjectFlags_HasOCPDevice = 0x0400, ///< Fuse, Relay, or Recloser
        DSSObjectFlags_HasAutoOCPDevice = 0x0800, ///< Relay or Recloser only
        DSSObjectFlags_NeedsRecalc = 0x1000, ///< Used for Edit command loops
        DSSObjectFlags_NeedsYPrim = 0x2000 ///< Used for Edit command loops + setter flags
    };

    /*!
    Setter flags customize down how the update of DSS properties are handled by the
    engine and parts of the API. Use especially in the `Obj` and `Batch` APIs
    */
    enum SetterFlags {
        SetterFlags_ImplicitSizes = 0x00000001, /*!< 
            Most array properties depend on sizes defined by other properties.
            Using this flag, many properties allow users to skip setting the other property
            directly, allowing the engine to use the size of the provided array to
            initialize the other property.
        */

        SetterFlags_AvoidFullRecalc = 0x00000002, /*!<
            Some components like Loads don't need to update YPrim for every change, e.g. setting
            "`load.a_load.kW=1`" if was "kW" previously 2 should not force a YPrim update, but it does
            force an update by default.
            Using this flag will reproduce what the classic OpenDSS API for Loads (DSS.ActiveCircuit.Loads)
            does, but removes a lot of duplicated code. Besides that, we can extend the feature 
            for other components if we think it fits.
        */

        SetterFlags_SkipNA = 0x00000004, /*!<
            For batch operations with arrays, skip NA values
            
            Currently, NA values are interpret as:
            - NaN for float64
            - INT32_MAX (0x7FFFFFFF) for int32
            - Null pointers for strings (in this case, use a `"\0"` string for empty strings)
        */
        
        SetterFlags_AllowAllConductors = 0x40000000 /*!< 
            Used internally for the "Wires" property ("Conductors").
            This was left public in case someone tries to implement some internal aspects in
            external functions.
        */
    };

    /*!
    The values from AltDSSEvent are used in the updated DSSEvents_* functions to
    register callbacks for different events. Note that in the official OpenDSS
    (COM implementation) only the first three event types (marked as Legacy) are
    available and the callback functions do not receive the extra arguments. To
    simplify our implementation, we decided to merge the legacy events in our
    new system. As such, some functions were removed and the old callback
    "dss_callback_solution_t" was replaced with "altdss_callback_event_t".
    */
    enum AltDSSEvent {
        AltDSSEvent_Legacy_InitControls = 0,
        AltDSSEvent_Legacy_CheckControls = 1,
        AltDSSEvent_Legacy_StepControls = 2,
        AltDSSEvent_Clear = 3,
        AltDSSEvent_ReprocessBuses = 4,
        AltDSSEvent_BuildSystemY = 5
    };

    /*!
    Extra class IDs, currently used by Obj_GetListPointer and Obj_GetCount
    */
    enum ExtraClassIDs {
        ExtraClassIDs_CktElements = -1,
        ExtraClassIDs_PCElements = -2,
        ExtraClassIDs_PDElements = -3
    };

#ifdef __cplusplus
} // extern "C"
#ifdef ALTDSS_CAPI_NAMESPACE
} } // namespace altdss::capi
#endif
#endif
#endif
