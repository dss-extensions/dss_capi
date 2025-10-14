/*!

altdss_classic.hpp: a C++ layer for the DSS-Extensions/C-API, classic API of OpenDSS and AltDSS
Copyright (c) 2021-2025 Paulo Meira
Copyright (c) 2021-2025 DSS-Extensions contributors

Version: 0.6.0 (2025-05)

**POTENTIAL BREAKING CHANGES UNTIL VERSION 1.0**
*/
        
#pragma once
#ifndef ALTDSS_CPP_CLASSIC_API
#define ALTDSS_CPP_CLASSIC_API

#include "altdss_common.hpp"
#include "altdss_obj.hpp"

namespace altdss { namespace classic {

#ifdef ALTDSS_CAPI_NAMESPACE
using namespace altdss::capi;
#endif
#pragma region classic_enums
    enum class ActionCodes: int32_t {
    	none = 0,
    	Open = 1,
    	Close = 2,
    	Reset = 3,
    	Lock = 4,
    	Unlock = 5,
    	TapUp = 6,
    	TapDown = 7
    };

    /// Event codes used by the event callback system
    ///
    /// Legacy events are the events present the classic OpenDSS COM implementation,
    /// while the rest are extensions added here.
    enum class AltDSSEvent: int32_t {
    	Legacy_InitControls = 0,
    	Legacy_CheckControls = 1,
    	Legacy_StepControls = 2,
    	Clear = 3,
    	ReprocessBuses = 4,
    	BuildSystemY = 5
    };

    enum class AutoAddTypes: int32_t {
    	AddGen = 1,
    	AddCap = 2
    };

    enum class CapControlModes: int32_t {
    	Current = 0,
    	Voltage = 1,
    	KVAR = 2,
    	Time = 3,
    	PF = 4
    };

    enum class CktModels: int32_t {
    	Multiphase = 0,
    	PositiveSeq = 1
    };

    enum class ControlModes: int32_t {
    	Static = 0,
    	Event = 1,
    	Time = 2,
    	Multirate = 3,
    	Off = -1
    };

    /// Transformer Core Type
    enum class CoreType: int32_t {
    	shell = 0,
    	one_phase = 1,
    	three_leg = 3,
    	four_leg = 4,
    	five_leg = 5,
    	core_1_phase = 9
    };

    enum class DSSCompatFlags: uint32_t {
    	NoSolverFloatChecks = 1,
    	BadPrecision = 2,
    	InvControl9611 = 4,
    	SaveCalcVoltageBases = 8,
    	ActiveLine = 16,
    	NoPropertyTracking = 32,
    	SkipSideEffects = 64,
    	MonitorHeader = 128,
    	InvControlDeltaV = 256,
    	PermissiveProperties = 512,
    	LegacySMARTDS = 2048
    };

    DSSCompatFlags operator|(DSSCompatFlags f1, DSSCompatFlags f2)
    {
        return static_cast<DSSCompatFlags>(static_cast<uint32_t>(f1) | static_cast<uint32_t>(f2));
    }

    enum class DSSJSONFlags: uint32_t {
    	Full = 1,
    	SkipRedundant = 2,
    	EnumAsInt = 4,
    	FullNames = 8,
    	Pretty = 16,
    	ExcludeDisabled = 32,
    	IncludeDSSClass = 64,
    	LowercaseKeys = 128,
    	IncludeDefaultObjs = 256,
    	SkipTimestamp = 512,
    	SkipBuses = 1024
    };

    DSSJSONFlags operator|(DSSJSONFlags f1, DSSJSONFlags f2)
    {
        return static_cast<DSSJSONFlags>(static_cast<uint32_t>(f1) | static_cast<uint32_t>(f2));
    }

    /// Object flags are bit flags used by various of the internal processes of the DSS engine.
    ///
    /// Most are internal state, but advanced/expert users can manipulate them for some interesting uses.
    enum class DSSObjectFlags: uint32_t {
    	Editing = 1,
    	HasBeenSaved = 2,
    	DefaultAndUnedited = 4,
    	Checked = 8,
    	Flag = 16,
    	HasEnergyMeter = 32,
    	HasSensorObj = 64,
    	IsIsolated = 128,
    	HasControl = 256,
    	IsMonitored = 512,
    	HasOCPDevice = 1024,
    	HasAutoOCPDevice = 2048,
    	NeedsRecalc = 4096,
    	NeedsYPrim = 8192
    };

    DSSObjectFlags operator|(DSSObjectFlags f1, DSSObjectFlags f2)
    {
        return static_cast<DSSObjectFlags>(static_cast<uint32_t>(f1) | static_cast<uint32_t>(f2));
    }

    /// This enum is used in the PropertyNameStyle property to control the naming convention.
    /// Currently, this only affects capitalization, i.e., if your software already uses case
    /// insensitive string comparisons for the property names, this is not useful. Otherwise,
    /// you can use `Legacy` to use the older names.
    enum class DSSPropertyNameStyle: int32_t {
    	Modern = 0,
    	Lowercase = 1,
    	Legacy = 2
    };

    /// DSSSaveFlags are bit flags used in the Circuit_Save function to
    /// customize the saved circuit.
    enum class DSSSaveFlags: uint32_t {
    	CalcVoltageBases = 1,
    	SetVoltageBases = 2,
    	IncludeOptions = 4,
    	IncludeDisabled = 8,
    	ExcludeDefault = 16,
    	SingleFile = 32,
    	KeepOrder = 64,
    	ExcludeMeterZones = 128,
    	IsOpen = 256,
    	ToString = 512
    };

    DSSSaveFlags operator|(DSSSaveFlags f1, DSSSaveFlags f2)
    {
        return static_cast<DSSSaveFlags>(static_cast<uint32_t>(f1) | static_cast<uint32_t>(f2));
    }

    /// Energy meter registers
    ///
    /// This enumeration lists the basic energy meter registers. Extra registers start
    /// at `VBaseStart`. This is exposed to make it easier to access common registers
    /// without needing to check the register names every time, plus makes it safer to
    /// access the registers by index directly without introducing bugs we found in
    /// OpenDSS code (both user code and engine code) in the past due to direct use
    /// of magic numbers.
    enum class EnergyMeterRegisters: int32_t {
    	kWh = 0,
    	kvarh = 1,
    	MaxkW = 2,
    	MaxkVA = 3,
    	ZonekWh = 4,
    	Zonekvarh = 5,
    	ZoneMaxkW = 6,
    	ZoneMaxkVA = 7,
    	OverloadkWhNorm = 8,
    	OverloadkWhEmerg = 9,
    	LoadEEN = 10,
    	LoadUE = 11,
    	ZoneLosseskWh = 12,
    	ZoneLosseskvarh = 13,
    	LossesMaxkW = 14,
    	LossesMaxkvar = 15,
    	LoadLosseskWh = 16,
    	LoadLosseskvarh = 17,
    	NoLoadLosseskWh = 18,
    	NoLoadLosseskvarh = 19,
    	MaxLoadLosses = 20,
    	MaxNoLoadLosses = 21,
    	LineLosseskWh = 22,
    	TransformerLosseskWh = 23,
    	LineModeLineLoss = 24,
    	ZeroModeLineLoss = 25,
    	ThreePhaseLineLoss = 26,
    	OnePhaseLineLoss = 27,
    	GenkWh = 28,
    	Genkvarh = 29,
    	GenMaxkW = 30,
    	GenMaxkVA = 31,
    	VBaseStart = 32
    };

    /// Generator registers
    ///
    /// Enumeration of the generator registers by index.
    /// Currently shared between the Generator, Storage and PVSystem models.
    enum class GeneratorRegisters: int32_t {
    	kWh = 0,
    	kvarh = 1,
    	MaxkW = 2,
    	MaxkVA = 3,
    	Hours = 4,
    	Price = 5
    };

    enum class GeneratorStatus: int32_t {
    	Variable = 0,
    	Fixed = 1
    };

    /// Generator variables
    ///
    /// Enumeration of the generator *state variables* by (1-based) index.
    /// This is the implicit list and there can be more variables used by user-models
    /// and DynamicExp objects. For those, users can get the variable names from the
    /// API.
    enum class GeneratorVariables: int32_t {
    	Frequency = 1,
    	Theta = 2,
    	Vd = 3,
    	PShaft = 4,
    	dSpeed = 5,
    	dTheta = 6
    };

    /// IndMach012 variables
    ///
    /// Enumeration of the IndMach012 *state variables* by (1-based) index.
    enum class IndMach012Variables: int32_t {
    	Frequency = 1,
    	Theta = 2,
    	E1 = 3,
    	Pshaft = 4,
    	dSpeed = 5,
    	dTheta = 6,
    	Slip = 7,
    	puRs = 8,
    	puXs = 9,
    	puRr = 10,
    	puXr = 11,
    	puXm = 12,
    	MaxSlip = 13,
    	Is1 = 14,
    	Is2 = 15,
    	Ir1 = 16,
    	Ir2 = 17,
    	StatorLosses = 18,
    	RotorLosses = 19,
    	ShaftPowerHP = 20,
    	PowerFactor = 21,
    	Efficiency = 22
    };

    enum class LineUnits: int32_t {
    	none = 0,
    	Miles = 1,
    	kFt = 2,
    	km = 3,
    	meter = 4,
    	ft = 5,
    	inch = 6,
    	cm = 7,
    	mm = 8
    };

    enum class LoadModels: int32_t {
    	ConstPQ = 1,
    	ConstZ = 2,
    	Motor = 3,
    	CVR = 4,
    	ConstI = 5,
    	ConstPFixedQ = 6,
    	ConstPFixedX = 7,
    	ZIPV = 8
    };

    enum class LoadStatus: int32_t {
    	Variable = 0,
    	Fixed = 1,
    	Exempt = 2
    };

    enum class MonitorModes: int32_t {
    	VI = 0,
    	Power = 1,
    	Taps = 2,
    	States = 3,
    	Sequence = 16,
    	Magnitude = 32,
    	PosOnly = 64
    };

    /// Overcurrent Protection Device Type
    enum class OCPDevType: int32_t {
    	none = 0,
    	Fuse = 1,
    	Recloser = 2,
    	Relay = 3
    };

    /// Deprecated. Please use instead:
    /// - AutoAddTypes
    /// - CktModels
    /// - ControlModes
    /// - SolutionLoadModels
    /// - SolutionAlgorithms
    /// - RandomModes
    enum class Options: int32_t {
    	PowerFlow = 1,
    	Admittance = 2,
    	NormalSolve = 0,
    	LogNormal = 3,
    	ControlOFF = -1
    };

    /// PVSystem variables
    ///
    /// Enumeration of the PVSystem *state variables* by (1-based) index.
    /// This is the implicit list and there can be more variables used by user-models
    /// and DynamicExp objects.
    enum class PVSystemVariables: int32_t {
    	Irradiance = 1,
    	PanelkW = 2,
    	P_TFactor = 3,
    	Efficiency = 4,
    	Vreg = 5,
    	Vavg_DRC = 6,
    	volt_var = 7,
    	volt_watt = 8,
    	DRC = 9,
    	VV_DRC = 10,
    	watt_pf = 11,
    	watt_var = 12,
    	kW_out_desired = 13,
    	GridVoltage = 14,
    	di_dt = 15,
    	it = 16,
    	itHistory = 17,
    	RatedVDC = 18,
    	AvgDutyCycle = 19,
    	Target_Amps = 20,
    	SeriesL = 21,
    	MaxAmps_phase = 22
    };

    enum class RandomModes: int32_t {
    	Gaussian = 1,
    	Uniform = 2,
    	LogNormal = 3
    };

    /// Setter flags customize how the update of DSS properties are handled by the
    /// engine and parts of the API. Use especially in the `Obj` and `Batch` APIs
    enum class SetterFlags: uint32_t {
    	ImplicitSizes = 1,
    	AvoidFullRecalc = 2,
    	SkipNA = 4,
    	AllowAllConductors = 1073741824
    };

    SetterFlags operator|(SetterFlags f1, SetterFlags f2)
    {
        return static_cast<SetterFlags>(static_cast<uint32_t>(f1) | static_cast<uint32_t>(f2));
    }

    enum class SolutionAlgorithms: int32_t {
    	NormalSolve = 0,
    	NewtonSolve = 1,
    	NCIMSolve = 2
    };

    enum class SolutionLoadModels: int32_t {
    	PowerFlow = 1,
    	Admittance = 2
    };

    enum class SolveModes: int32_t {
    	SnapShot = 0,
    	Daily = 1,
    	Yearly = 2,
    	Monte1 = 3,
    	LD1 = 4,
    	PeakDay = 5,
    	DutyCycle = 6,
    	Direct = 7,
    	MonteFault = 8,
    	FaultStudy = 9,
    	Monte2 = 10,
    	Monte3 = 11,
    	LD2 = 12,
    	AutoAdd = 13,
    	Dynamic = 14,
    	Harmonic = 15,
    	Time = 16,
    	HarmonicT = 17
    };

    enum class SparseSolverOptions: int32_t {
    	ReuseNothing = 0,
    	ReuseCompressedMatrix = 1,
    	ReuseSymbolicFactorization = 2,
    	ReuseNumericFactorization = 3,
    	AlwaysResetYPrimInvalid = 268435456
    };

    enum class StorageStates: int32_t {
    	Charging = -1,
    	Idling = 0,
    	Discharging = 1
    };

    /// Storage variables
    ///
    /// Enumeration of the Storage state variables by (1-based) index.
    /// This is the implicit list and there can be more variables used by user-models
    /// and DynamicExp objects.
    enum class StorageVariables: int32_t {
    	kWh = 1,
    	State = 2,
    	kWOut = 3,
    	kWIn = 4,
    	kvarOut = 5,
    	DCkW = 6,
    	kWTotalLosses = 7,
    	kWInvLosses = 8,
    	kWIdlingLosses = 9,
    	kWChDchLosses = 10,
    	kWhChng = 11,
    	InvEff = 12,
    	InverterON = 13,
    	Vref = 14,
    	Vavg_DRC = 15,
    	VV_Oper = 16,
    	VW_Oper = 17,
    	DRC_Oper = 18,
    	VV_DRC_Oper = 19,
    	WP_Oper = 20,
    	WV_Oper = 21,
    	kWDesired = 22,
    	kW_VW_Limit = 23,
    	Limit_kWOut_Function = 24,
    	kVA_Exceeded = 25,
    	GridVoltage = 26,
    	di_dt = 27,
    	it = 28,
    	itHistory = 29,
    	RatedVDC = 30,
    	AvgDutyCycle = 31,
    	Target_Amps = 32,
    	SeriesL = 33,
    	MaxAmps_phase = 34
    };

    /// UPFC variables
    ///
    /// Enumeration of the UPFC state variables by (1-based) index.
    enum class UPFCVariables: int32_t {
    	ModeUPFC = 1,
    	IUPFC = 2,
    	Re_Vbin = 3,
    	Im_Vbin = 4,
    	Re_Vbout = 5,
    	Im_Vbout = 6,
    	Losses = 7,
    	P_UPFC = 8,
    	Q_UPFC = 9,
    	Qideal = 10,
    	Re_Sr0_1 = 11,
    	Im_Sr0_1 = 12,
    	Re_Sr1_1 = 13,
    	Im_Sr1_1 = 14
    };

    /// VCCS non-RMS variables
    ///
    /// Enumeration of the VCCS state variables by (1-based) index, when used in non-RMS mode (`RMSMode=false`).
    enum class VCCSNonRMSVariables: int32_t {
    	Vwave = 1,
    	Iwave = 2,
    	Irms = 3,
    	Ipeak = 4,
    	BP1out = 5,
    	Hout = 6
    };

    /// VCCS RMS variables
    ///
    /// Enumeration of the VCCS state variables by (1-based) index, when used in RMS mode (`RMSMode=true`).
    enum class VCCSRMSVariables: int32_t {
    	Vrms = 1,
    	Ipwr = 2,
    	Hout = 3,
    	Irms = 4
    };

    enum class YMatrixModes: int32_t {
    	SeriesOnly = 1,
    	WholeMatrix = 2
    };

#pragma endregion classic_enums



    class IDSSProgress: public ContextState
    {
    public:

        IDSSProgress(altdss::APIUtil *util) :
            ContextState(util)
        {
        }
        /// 
        /// Close progress form
        /// 
        /// Typically used with EPRI's OpenDSS, on Windows. Otherwise, it could be a no-op.
        /// 
        void Close()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_DSSProgress_Close(ctx);
        }
        /// 
        /// Show progress form
        /// 
        /// Typically used with EPRI's OpenDSS, on Windows. Otherwise, it could be a no-op.
        /// 
        void Show()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_DSSProgress_Show(ctx);
        }

        /// 
        /// Set the caption to appear on the bottom of the DSS Progress form.
        /// 
        /// Typically used with EPRI's OpenDSS, on Windows. Otherwise, it could be a no-op.
        /// 
        /// Original COM help: https://opendss.epri.com/Caption.html
        /// 
        IDSSProgress& Caption(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_DSSProgress_Set_Caption(ctx, value);
            return *this;
        }
        IDSSProgress& Caption(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_DSSProgress_Set_Caption(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Set the percent progress to indicate [0..100] on the progress form.
        /// 
        /// Typically used with EPRI's OpenDSS, on Windows. Otherwise, it could be a no-op.
        /// 
        /// Original COM help: https://opendss.epri.com/PctProgress.html
        /// 
        IDSSProgress& PctProgress(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_DSSProgress_Set_PctProgress(ctx, value);
            return *this;
        }
    };

    class IDSSProperty: public ContextState
    {
    public:

        IDSSProperty(altdss::APIUtil *util) :
            ContextState(util)
        {
        }
    
        IDSSProperty& operator[](int32_t key) // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_DSSProperty_Set_Index(ctx, key);
            return *this;
        }

        IDSSProperty& operator[](const char *key) // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_DSSProperty_Set_Name(ctx, key);
            return *this;
        }
        IDSSProperty& operator[](const string &key) // getter
        {
            return (*this)[key.c_str()];
        }

        /// 
        /// Description of the property.
        /// 
        /// Original COM help: https://opendss.epri.com/Description.html
        /// 
        string Description() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSSProperty_Get_Description(ctx);
        }

        /// 
        /// Name of Property
        /// 
        /// Original COM help: https://opendss.epri.com/Name6.html
        /// 
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSSProperty_Get_Name(ctx);
        }

        /// 
        /// Get/set the value of the active property. The value must be specified as a string.
        /// 
        /// Original COM help: https://opendss.epri.com/Val.html
        /// 
        string Val() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSSProperty_Get_Val(ctx);
        }
        IDSSProperty& Val(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_DSSProperty_Set_Val(ctx, value);
            return *this;
        }
        IDSSProperty& Val(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_DSSProperty_Set_Val(ctx, value.c_str());
            return *this;
        }
    };

    class IDSS_Executive: public ContextState
    {
    public:

        IDSS_Executive(altdss::APIUtil *util) :
            ContextState(util)
        {
        }
        /// 
        /// Get i-th command
        /// 
        /// Original COM help: https://opendss.epri.com/Command.html
        /// 
        string Command(int32_t i)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSS_Executive_Get_Command(ctx, i);
        }
        /// 
        /// Get help string for i-th command
        /// 
        /// Original COM help: https://opendss.epri.com/CommandHelp.html
        /// 
        string CommandHelp(int32_t i)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSS_Executive_Get_CommandHelp(ctx, i);
        }
        /// 
        /// Get i-th option
        /// 
        /// Original COM help: https://opendss.epri.com/Option.html
        /// 
        string Option(int32_t i)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSS_Executive_Get_Option(ctx, i);
        }
        /// 
        /// Get help string for i-th option
        /// 
        /// Original COM help: https://opendss.epri.com/OptionHelp.html
        /// 
        string OptionHelp(int32_t i)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSS_Executive_Get_OptionHelp(ctx, i);
        }
        /// 
        /// Get present value of i-th option
        /// 
        /// Original COM help: https://opendss.epri.com/OptionValue.html
        /// 
        string OptionValue(int32_t i)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSS_Executive_Get_OptionValue(ctx, i);
        }

        /// 
        /// Number of DSS Executive Commands
        /// 
        /// Original COM help: https://opendss.epri.com/NumCommands.html
        /// 
        int32_t NumCommands() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSS_Executive_Get_NumCommands(ctx);
        }

        /// 
        /// Number of DSS Executive Options
        /// 
        /// Original COM help: https://opendss.epri.com/NumOptions.html
        /// 
        int32_t NumOptions() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSS_Executive_Get_NumOptions(ctx);
        }
    };

    class IDSSimComs: public ContextState
    {
    public:

        IDSSimComs(dss::APIUtil *util) :
            ContextState(util)
        {
        }
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT BusVoltage(size_t Index)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_DSSimComs_BusVoltage_GR(ctx, Index);
            return api_util->get_float64_gr_array<VectorT>();
        }
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT BusVoltagepu(size_t Index)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_DSSimComs_BusVoltagepu_GR(ctx, Index);
            return api_util->get_float64_gr_array<VectorT>();
        }
    };

    class IError: public ContextState
    {
    public:

        IError(altdss::APIUtil *util) :
            ContextState(util)
        {
        }

        /// 
        /// Description of error for last operation
        /// 
        /// Original COM help: https://opendss.epri.com/Description1.html
        /// 
        string Description() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Error_Get_Description(ctx);
        }

        /// 
        /// Error Number (returns current value and then resets to zero)
        /// 
        /// Original COM help: https://opendss.epri.com/Number.html
        /// 
        int32_t Number() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Error_Get_Number(ctx);
        }

        /// 
        /// EarlyAbort controls whether all errors halts the DSS script processing (Compile/Redirect), defaults to True.
        /// 
        /// **(API Extension)**
        /// 
        bool EarlyAbort() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Error_Get_EarlyAbort(ctx);
        }
        IError& EarlyAbort(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Error_Set_EarlyAbort(ctx, value);
            return *this;
        }

        /// 
        /// Controls whether the extended error mechanism is used. Defaults to True.
        /// 
        /// Extended errors are errors derived from checks across the API to ensure
        /// a valid state. Although many of these checks are already present in the 
        /// original/EPRI's COM interface, the checks do not produce any error 
        /// message. An error value can be returned by a function but this value
        /// can, for many of the functions, be a valid value. As such, the user
        /// has no means to detect an invalid API call. 
        /// 
        /// Extended errors use the Error interface to provide a more clear message
        /// and should help users, especially new users, to find usage issues earlier.
        /// 
        /// At C++ level, an exception is raised when an error is detected through
        /// the Error interface.
        /// 
        /// The current default state is ON. For compatibility, the user can turn it
        /// off to restore the previous behavior.
        /// 
        /// **(API Extension)**
        /// 
        bool ExtendedErrors() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Error_Get_ExtendedErrors(ctx);
        }
        IError& ExtendedErrors(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Error_Set_ExtendedErrors(ctx, value);
            return *this;
        }
    };

    class IFuses: public ContextState
    {
    public:

        IFuses(altdss::APIUtil *util) :
            ContextState(util)
        {
        }

        ///
        /// Array of strings with all Fuse names in the circuit.
        ///
        strings AllNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Fuses_Get_AllNames);
        }

        ///
        /// Number of Fuse objects in active circuit.
        ///
        int32_t Count() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Fuses_Get_Count(ctx);
        }

        ///
        /// Sets the first Fuse active. Returns 0 if no more.
        ///
        int32_t First() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Fuses_Get_First(ctx);
        }

        ///
        /// Get the name of the current active Fuse
        ///
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Fuses_Get_Name(ctx);
        }

        ///
        /// Sets the active Fuse by Name.
        ///
        IFuses& Name(const char *value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Fuses_Set_Name(ctx, value);
            return *this;
        }
        IFuses& Name(const string &value)
        {
            return Name(value.c_str());
        }

        ///
        /// Sets the next Fuse active. Returns 0 if no more.
        ///
        int32_t Next() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Fuses_Get_Next(ctx);
        }

        ///
        /// Get active Fuse by index; index is 1-based: 1..count
        ///
        int32_t idx() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Fuses_Get_idx(ctx);
        }

        ///
        /// Get active Fuse by index; index is 1-based: 1..count
        ///
        void idx(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Fuses_Set_idx(ctx, value);
        }
        /// 
        /// Close all phases of the fuse.
        /// 
        /// Original COM help: https://opendss.epri.com/Close3.html
        /// 
        void Close()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Fuses_Close(ctx);
        }
        /// 
        /// Current state of the fuses. TRUE if any fuse on any phase is blown. Else FALSE.
        /// 
        /// Original COM help: https://opendss.epri.com/IsBlown.html
        /// 
        bool IsBlown()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Fuses_IsBlown(ctx);
        }
        /// 
        /// Manual opening of all phases of the fuse.
        /// 
        /// Original COM help: https://opendss.epri.com/Open2.html
        /// 
        void Open()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Fuses_Open(ctx);
        }
        /// 
        /// Reset fuse to normal state.
        /// 
        /// Original COM help: https://opendss.epri.com/Reset7.html
        /// 
        void Reset()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Fuses_Reset(ctx);
        }

        /// 
        /// A fixed delay time in seconds added to the fuse blowing time determined by the TCC curve. Default is 0.
        /// This represents a fuse clear or other delay.
        /// 
        /// Original COM help: https://opendss.epri.com/Delay1.html
        /// 
        double Delay() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Fuses_Get_Delay(ctx);
        }
        IFuses& Delay(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Fuses_Set_Delay(ctx, value);
            return *this;
        }

        /// 
        /// Full name of the circuit element to which the fuse is connected.
        /// 
        /// Original COM help: https://opendss.epri.com/MonitoredObj1.html
        /// 
        string MonitoredObj() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Fuses_Get_MonitoredObj(ctx);
        }
        IFuses& MonitoredObj(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Fuses_Set_MonitoredObj(ctx, value);
            return *this;
        }
        IFuses& MonitoredObj(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Fuses_Set_MonitoredObj(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Terminal number to which the fuse is connected.
        /// 
        /// Original COM help: https://opendss.epri.com/MonitoredTerm1.html
        /// 
        int32_t MonitoredTerm() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Fuses_Get_MonitoredTerm(ctx);
        }
        IFuses& MonitoredTerm(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Fuses_Set_MonitoredTerm(ctx, value);
            return *this;
        }

        /// 
        /// Number of phases, this fuse. 
        /// 
        /// Original COM help: https://opendss.epri.com/NumPhases1.html
        /// 
        int32_t NumPhases() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Fuses_Get_NumPhases(ctx);
        }

        /// 
        /// Multiplier or actual amps for the TCCcurve object. Defaults to 1.0. 
        /// 
        /// Multiply current values of TCC curve by this to get actual amps.
        /// 
        /// Original COM help: https://opendss.epri.com/RatedCurrent.html
        /// 
        double RatedCurrent() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Fuses_Get_RatedCurrent(ctx);
        }
        IFuses& RatedCurrent(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Fuses_Set_RatedCurrent(ctx, value);
            return *this;
        }

        /// 
        /// Full name of the circuit element switch that the fuse controls. 
        /// Defaults to the MonitoredObj.
        /// 
        /// Original COM help: https://opendss.epri.com/SwitchedObj.html
        /// 
        string SwitchedObj() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Fuses_Get_SwitchedObj(ctx);
        }
        IFuses& SwitchedObj(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Fuses_Set_SwitchedObj(ctx, value);
            return *this;
        }
        IFuses& SwitchedObj(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Fuses_Set_SwitchedObj(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Number of the terminal of the controlled element containing the switch controlled by the fuse.
        /// 
        /// Original COM help: https://opendss.epri.com/SwitchedTerm.html
        /// 
        int32_t SwitchedTerm() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Fuses_Get_SwitchedTerm(ctx);
        }
        IFuses& SwitchedTerm(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Fuses_Set_SwitchedTerm(ctx, value);
            return *this;
        }

        /// 
        /// Name of the TCCcurve object that determines fuse blowing.
        /// 
        /// Original COM help: https://opendss.epri.com/TCCcurve.html
        /// 
        string TCCcurve() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Fuses_Get_TCCcurve(ctx);
        }
        IFuses& TCCcurve(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Fuses_Set_TCCcurve(ctx, value);
            return *this;
        }
        IFuses& TCCcurve(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Fuses_Set_TCCcurve(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Array of strings indicating the state of each phase of the fuse.
        /// 
        /// Original COM help: https://opendss.epri.com/State2.html
        /// 
        strings State() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Fuses_Get_State);
        }
        IFuses& State(const strings &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            api_util->set_string_array(ctx_Fuses_Set_State, value);
            return *this;
        }

        /// 
        /// Array of strings indicating the normal state of each phase of the fuse.
        /// 
        /// Original COM help: https://opendss.epri.com/NormalState2.html
        /// 
        strings NormalState() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Fuses_Get_NormalState);
        }
        IFuses& NormalState(const strings &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            api_util->set_string_array(ctx_Fuses_Set_NormalState, value);
            return *this;
        }
    };

    class IGenerators: public ContextState
    {
    public:

        IGenerators(altdss::APIUtil *util) :
            ContextState(util)
        {
        }

        ///
        /// Array of strings with all Generator names in the circuit.
        ///
        strings AllNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Generators_Get_AllNames);
        }

        ///
        /// Number of Generator objects in active circuit.
        ///
        int32_t Count() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Generators_Get_Count(ctx);
        }

        ///
        /// Sets the first Generator active. Returns 0 if no more.
        ///
        int32_t First() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Generators_Get_First(ctx);
        }

        ///
        /// Get the name of the current active Generator
        ///
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Generators_Get_Name(ctx);
        }

        ///
        /// Sets the active Generator by Name.
        ///
        IGenerators& Name(const char *value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Generators_Set_Name(ctx, value);
            return *this;
        }
        IGenerators& Name(const string &value)
        {
            return Name(value.c_str());
        }

        ///
        /// Sets the next Generator active. Returns 0 if no more.
        ///
        int32_t Next() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Generators_Get_Next(ctx);
        }

        ///
        /// Get active Generator by index; index is 1-based: 1..count
        ///
        int32_t idx() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Generators_Get_idx(ctx);
        }

        ///
        /// Get active Generator by index; index is 1-based: 1..count
        ///
        void idx(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Generators_Set_idx(ctx, value);
        }

        /// 
        /// Indicates whether the generator is forced ON regardless of other dispatch criteria.
        /// 
        /// Original COM help: https://opendss.epri.com/ForcedON.html
        /// 
        bool ForcedON() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Generators_Get_ForcedON(ctx);
        }
        IGenerators& ForcedON(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Generators_Set_ForcedON(ctx, value);
            return *this;
        }

        /// 
        /// Generator Model
        /// 
        /// Original COM help: https://opendss.epri.com/Model.html
        /// 
        int32_t Model() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Generators_Get_Model(ctx);
        }
        IGenerators& Model(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Generators_Set_Model(ctx, value);
            return *this;
        }

        /// 
        /// Power factor (pos. = producing vars). Updates kvar based on present kW value.
        /// 
        /// Original COM help: https://opendss.epri.com/PF.html
        /// 
        double PF() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Generators_Get_PF(ctx);
        }
        IGenerators& PF(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Generators_Set_PF(ctx, value);
            return *this;
        }

        /// 
        /// Number of phases
        /// 
        /// Original COM help: https://opendss.epri.com/Phases.html
        /// 
        int32_t Phases() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Generators_Get_Phases(ctx);
        }
        IGenerators& Phases(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Generators_Set_Phases(ctx, value);
            return *this;
        }

        /// 
        /// Array of Names of all generator energy meter registers
        /// 
        /// See also the enum `GeneratorRegisters`.
        /// 
        strings RegisterNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Generators_Get_RegisterNames);
        }

        /// 
        /// Array of values in generator energy meter registers.
        /// 
        /// Original COM help: https://opendss.epri.com/RegisterValues.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT RegisterValues() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Generators_Get_RegisterValues_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Vmaxpu for generator model
        /// 
        /// Original COM help: https://opendss.epri.com/Vmaxpu.html
        /// 
        double Vmaxpu() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Generators_Get_Vmaxpu(ctx);
        }
        IGenerators& Vmaxpu(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Generators_Set_Vmaxpu(ctx, value);
            return *this;
        }

        /// 
        /// Vminpu for Generator model
        /// 
        /// Original COM help: https://opendss.epri.com/Vminpu.html
        /// 
        double Vminpu() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Generators_Get_Vminpu(ctx);
        }
        IGenerators& Vminpu(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Generators_Set_Vminpu(ctx, value);
            return *this;
        }

        /// 
        /// Voltage base for the active generator, kV
        /// 
        /// Original COM help: https://opendss.epri.com/kV1.html
        /// 
        double kV() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Generators_Get_kV(ctx);
        }
        IGenerators& kV(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Generators_Set_kV(ctx, value);
            return *this;
        }

        /// 
        /// kVA rating of the generator
        /// 
        /// Original COM help: https://opendss.epri.com/kVArated.html
        /// 
        double kVArated() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Generators_Get_kVArated(ctx);
        }
        IGenerators& kVArated(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Generators_Set_kVArated(ctx, value);
            return *this;
        }

        /// 
        /// kW output for the active generator. kvar is updated for current power factor.
        /// 
        /// Original COM help: https://opendss.epri.com/kW.html
        /// 
        double kW() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Generators_Get_kW(ctx);
        }
        IGenerators& kW(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Generators_Set_kW(ctx, value);
            return *this;
        }

        /// 
        /// kvar output for the active generator. Updates power factor based on present kW value.
        /// 
        /// Original COM help: https://opendss.epri.com/kvar.html
        /// 
        double kvar() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Generators_Get_kvar(ctx);
        }
        IGenerators& kvar(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Generators_Set_kvar(ctx, value);
            return *this;
        }

        /// 
        /// Name of the loadshape for a daily generation profile.
        /// 
        /// **(API Extension)**
        /// 
        string daily() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Generators_Get_daily(ctx);
        }
        IGenerators& daily(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Generators_Set_daily(ctx, value);
            return *this;
        }
        IGenerators& daily(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Generators_Set_daily(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Name of the loadshape for a duty cycle simulation.
        /// 
        /// **(API Extension)**
        /// 
        string duty() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Generators_Get_duty(ctx);
        }
        IGenerators& duty(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Generators_Set_duty(ctx, value);
            return *this;
        }
        IGenerators& duty(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Generators_Set_duty(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Name of yearly loadshape
        /// 
        /// **(API Extension)**
        /// 
        string Yearly() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Generators_Get_Yearly(ctx);
        }
        IGenerators& Yearly(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Generators_Set_Yearly(ctx, value);
            return *this;
        }
        IGenerators& Yearly(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Generators_Set_Yearly(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Response to dispatch multipliers: Fixed=1 (dispatch multipliers do not apply), Variable=0 (follows curves).
        /// 
        /// Related enumeration: GeneratorStatus
        /// 
        /// **(API Extension)**
        /// 
        GeneratorStatus Status() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return static_cast<GeneratorStatus>(ctx_Generators_Get_Status(ctx));
        }
        IGenerators& Status(GeneratorStatus value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Generators_Set_Status(ctx, static_cast<int32_t>(value));
            return *this;
        }

        /// 
        /// Generator connection. True/1 if delta connection, False/0 if wye.
        /// 
        /// **(API Extension)**
        /// 
        bool IsDelta() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Generators_Get_IsDelta(ctx);
        }
        IGenerators& IsDelta(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Generators_Set_IsDelta(ctx, value);
            return *this;
        }

        /// 
        /// kVA rating of electrical machine. Applied to machine or inverter definition for Dynamics mode solutions.
        /// 
        /// **(API Extension)**
        /// 
        double kva() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Generators_Get_kva(ctx);
        }
        IGenerators& kva(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Generators_Set_kva(ctx, value);
            return *this;
        }

        /// 
        /// An arbitrary integer number representing the class of Generator so that Generator values may be segregated by class.
        /// No effect on the solution.
        /// 
        /// **(API Extension)**
        /// 
        int32_t Class() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Generators_Get_Class_(ctx);
        }
        IGenerators& Class(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Generators_Set_Class_(ctx, value);
            return *this;
        }

        /// 
        /// Bus to which the Generator is connected. May include specific node specification.
        /// 
        /// **(API Extension)**
        /// 
        string Bus1() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Generators_Get_Bus1(ctx);
        }
        IGenerators& Bus1(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Generators_Set_Bus1(ctx, value);
            return *this;
        }
        IGenerators& Bus1(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Generators_Set_Bus1(ctx, value.c_str());
            return *this;
        }
    };

    class IISources: public ContextState
    {
    public:

        IISources(altdss::APIUtil *util) :
            ContextState(util)
        {
        }

        ///
        /// Array of strings with all ISource names in the circuit.
        ///
        strings AllNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_ISources_Get_AllNames);
        }

        ///
        /// Number of ISource objects in active circuit.
        ///
        int32_t Count() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_ISources_Get_Count(ctx);
        }

        ///
        /// Sets the first ISource active. Returns 0 if no more.
        ///
        int32_t First() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_ISources_Get_First(ctx);
        }

        ///
        /// Get the name of the current active ISource
        ///
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_ISources_Get_Name(ctx);
        }

        ///
        /// Sets the active ISource by Name.
        ///
        IISources& Name(const char *value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ISources_Set_Name(ctx, value);
            return *this;
        }
        IISources& Name(const string &value)
        {
            return Name(value.c_str());
        }

        ///
        /// Sets the next ISource active. Returns 0 if no more.
        ///
        int32_t Next() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_ISources_Get_Next(ctx);
        }

        ///
        /// Get active ISource by index; index is 1-based: 1..count
        ///
        int32_t idx() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_ISources_Get_idx(ctx);
        }

        ///
        /// Get active ISource by index; index is 1-based: 1..count
        ///
        void idx(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ISources_Set_idx(ctx, value);
        }

        /// 
        /// Magnitude of the ISource in amps
        /// 
        /// Original COM help: https://opendss.epri.com/Amps.html
        /// 
        double Amps() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_ISources_Get_Amps(ctx);
        }
        IISources& Amps(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ISources_Set_Amps(ctx, value);
            return *this;
        }

        /// 
        /// Phase angle for ISource, degrees
        /// 
        /// Original COM help: https://opendss.epri.com/AngleDeg.html
        /// 
        double AngleDeg() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_ISources_Get_AngleDeg(ctx);
        }
        IISources& AngleDeg(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ISources_Set_AngleDeg(ctx, value);
            return *this;
        }

        /// 
        /// The present frequency of the ISource, Hz
        /// 
        /// Original COM help: https://opendss.epri.com/Frequency.html
        /// 
        double Frequency() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_ISources_Get_Frequency(ctx);
        }
        IISources& Frequency(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ISources_Set_Frequency(ctx, value);
            return *this;
        }
    };

    class ILineCodes: public ContextState
    {
    public:

        ILineCodes(altdss::APIUtil *util) :
            ContextState(util)
        {
        }

        ///
        /// Array of strings with all LineCode names in the circuit.
        ///
        strings AllNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_LineCodes_Get_AllNames);
        }

        ///
        /// Number of LineCode objects in active circuit.
        ///
        int32_t Count() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LineCodes_Get_Count(ctx);
        }

        ///
        /// Sets the first LineCode active. Returns 0 if no more.
        ///
        int32_t First() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LineCodes_Get_First(ctx);
        }

        ///
        /// Get the name of the current active LineCode
        ///
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LineCodes_Get_Name(ctx);
        }

        ///
        /// Sets the active LineCode by Name.
        ///
        ILineCodes& Name(const char *value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineCodes_Set_Name(ctx, value);
            return *this;
        }
        ILineCodes& Name(const string &value)
        {
            return Name(value.c_str());
        }

        ///
        /// Sets the next LineCode active. Returns 0 if no more.
        ///
        int32_t Next() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LineCodes_Get_Next(ctx);
        }

        ///
        /// Get active LineCode by index; index is 1-based: 1..count
        ///
        int32_t idx() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LineCodes_Get_idx(ctx);
        }

        ///
        /// Get active LineCode by index; index is 1-based: 1..count
        ///
        void idx(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineCodes_Set_idx(ctx, value);
        }

        /// 
        /// Zero-sequence capacitance, nF per unit length
        /// 
        /// Original COM help: https://opendss.epri.com/C2.html
        /// 
        double C0() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LineCodes_Get_C0(ctx);
        }
        ILineCodes& C0(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineCodes_Set_C0(ctx, value);
            return *this;
        }

        /// 
        /// Positive-sequence capacitance, nF per unit length
        /// 
        /// Original COM help: https://opendss.epri.com/C3.html
        /// 
        double C1() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LineCodes_Get_C1(ctx);
        }
        ILineCodes& C1(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineCodes_Set_C1(ctx, value);
            return *this;
        }

        /// 
        /// Capacitance matrix, nF per unit length
        /// 
        /// Original COM help: https://opendss.epri.com/Cmatrix1.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Cmatrix() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineCodes_Get_Cmatrix_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        ILineCodes& Cmatrix(const VectorT &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineCodes_Set_Cmatrix(ctx, &value[0], value.size());
            return *this;
        }

        /// 
        /// Emergency ampere rating
        /// 
        /// Original COM help: https://opendss.epri.com/EmergAmps2.html
        /// 
        double EmergAmps() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LineCodes_Get_EmergAmps(ctx);
        }
        ILineCodes& EmergAmps(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineCodes_Set_EmergAmps(ctx, value);
            return *this;
        }

        /// 
        /// Flag denoting whether impedance data were entered in symmetrical components
        /// 
        /// Original COM help: https://opendss.epri.com/IsZ1Z0.html
        /// 
        bool IsZ1Z0() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LineCodes_Get_IsZ1Z0(ctx);
        }

        /// 
        /// Normal Ampere rating
        /// 
        /// Original COM help: https://opendss.epri.com/NormAmps1.html
        /// 
        double NormAmps() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LineCodes_Get_NormAmps(ctx);
        }
        ILineCodes& NormAmps(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineCodes_Set_NormAmps(ctx, value);
            return *this;
        }

        /// 
        /// Number of Phases
        /// 
        /// Original COM help: https://opendss.epri.com/Phases2.html
        /// 
        int32_t Phases() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LineCodes_Get_Phases(ctx);
        }
        ILineCodes& Phases(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineCodes_Set_Phases(ctx, value);
            return *this;
        }

        /// 
        /// Zero-Sequence Resistance, ohms per unit length
        /// 
        /// Original COM help: https://opendss.epri.com/R2.html
        /// 
        double R0() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LineCodes_Get_R0(ctx);
        }
        ILineCodes& R0(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineCodes_Set_R0(ctx, value);
            return *this;
        }

        /// 
        /// Positive-sequence resistance ohms per unit length
        /// 
        /// Original COM help: https://opendss.epri.com/R3.html
        /// 
        double R1() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LineCodes_Get_R1(ctx);
        }
        ILineCodes& R1(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineCodes_Set_R1(ctx, value);
            return *this;
        }

        /// 
        /// Resistance matrix, ohms per unit length
        /// 
        /// Original COM help: https://opendss.epri.com/Rmatrix1.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Rmatrix() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineCodes_Get_Rmatrix_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        ILineCodes& Rmatrix(const VectorT &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineCodes_Set_Rmatrix(ctx, &value[0], value.size());
            return *this;
        }

        LineUnits Units() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return static_cast<LineUnits>(ctx_LineCodes_Get_Units(ctx));
        }
        ILineCodes& Units(LineUnits value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineCodes_Set_Units(ctx, static_cast<int32_t>(value));
            return *this;
        }

        /// 
        /// Zero Sequence Reactance, Ohms per unit length
        /// 
        /// Original COM help: https://opendss.epri.com/X2.html
        /// 
        double X0() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LineCodes_Get_X0(ctx);
        }
        ILineCodes& X0(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineCodes_Set_X0(ctx, value);
            return *this;
        }

        /// 
        /// Positive-sequence reactance, ohms per unit length
        /// 
        /// Original COM help: https://opendss.epri.com/X3.html
        /// 
        double X1() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LineCodes_Get_X1(ctx);
        }
        ILineCodes& X1(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineCodes_Set_X1(ctx, value);
            return *this;
        }

        /// 
        /// Reactance matrix, ohms per unit length
        /// 
        /// Original COM help: https://opendss.epri.com/Xmatrix1.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Xmatrix() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineCodes_Get_Xmatrix_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        ILineCodes& Xmatrix(const VectorT &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineCodes_Set_Xmatrix(ctx, &value[0], value.size());
            return *this;
        }
    };

    class ILineSpacings: public ContextState
    {
    public:

        ILineSpacings(altdss::APIUtil *util) :
            ContextState(util)
        {
        }

        ///
        /// Array of strings with all LineSpacing names in the circuit.
        ///
        strings AllNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_LineSpacings_Get_AllNames);
        }

        ///
        /// Number of LineSpacing objects in active circuit.
        ///
        int32_t Count() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LineSpacings_Get_Count(ctx);
        }

        ///
        /// Sets the first LineSpacing active. Returns 0 if no more.
        ///
        int32_t First() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LineSpacings_Get_First(ctx);
        }

        ///
        /// Get the name of the current active LineSpacing
        ///
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LineSpacings_Get_Name(ctx);
        }

        ///
        /// Sets the active LineSpacing by Name.
        ///
        ILineSpacings& Name(const char *value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineSpacings_Set_Name(ctx, value);
            return *this;
        }
        ILineSpacings& Name(const string &value)
        {
            return Name(value.c_str());
        }

        ///
        /// Sets the next LineSpacing active. Returns 0 if no more.
        ///
        int32_t Next() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LineSpacings_Get_Next(ctx);
        }

        ///
        /// Get active LineSpacing by index; index is 1-based: 1..count
        ///
        int32_t idx() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LineSpacings_Get_idx(ctx);
        }

        ///
        /// Get active LineSpacing by index; index is 1-based: 1..count
        ///
        void idx(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineSpacings_Set_idx(ctx, value);
        }

        /// 
        /// Number of Phases
        /// 
        int32_t Phases() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LineSpacings_Get_Phases(ctx);
        }
        ILineSpacings& Phases(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineSpacings_Set_Phases(ctx, value);
            return *this;
        }

        int32_t Nconds() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LineSpacings_Get_Nconds(ctx);
        }
        ILineSpacings& Nconds(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineSpacings_Set_Nconds(ctx, value);
            return *this;
        }

        LineUnits Units() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return static_cast<LineUnits>(ctx_LineSpacings_Get_Units(ctx));
        }
        ILineSpacings& Units(LineUnits value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineSpacings_Set_Units(ctx, static_cast<int32_t>(value));
            return *this;
        }

        /// 
        /// Get/Set the X (horizontal) coordinates of the conductors
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Xcoords() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineSpacings_Get_Xcoords_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        ILineSpacings& Xcoords(const VectorT &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineSpacings_Set_Xcoords(ctx, &value[0], value.size());
            return *this;
        }

        /// 
        /// Get/Set the Y (vertical/height) coordinates of the conductors
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Ycoords() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineSpacings_Get_Ycoords_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        ILineSpacings& Ycoords(const VectorT &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineSpacings_Set_Ycoords(ctx, &value[0], value.size());
            return *this;
        }
    };

    class ILoadShapes: public ContextState
    {
    public:

        ILoadShapes(altdss::APIUtil *util) :
            ContextState(util)
        {
        }

        ///
        /// Array of strings with all LoadShape names in the circuit.
        ///
        strings AllNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_LoadShapes_Get_AllNames);
        }

        ///
        /// Number of LoadShape objects in active circuit.
        ///
        int32_t Count() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LoadShapes_Get_Count(ctx);
        }

        ///
        /// Sets the first LoadShape active. Returns 0 if no more.
        ///
        int32_t First() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LoadShapes_Get_First(ctx);
        }

        ///
        /// Get the name of the current active LoadShape
        ///
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LoadShapes_Get_Name(ctx);
        }

        ///
        /// Sets the active LoadShape by Name.
        ///
        ILoadShapes& Name(const char *value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LoadShapes_Set_Name(ctx, value);
            return *this;
        }
        ILoadShapes& Name(const string &value)
        {
            return Name(value.c_str());
        }

        ///
        /// Sets the next LoadShape active. Returns 0 if no more.
        ///
        int32_t Next() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LoadShapes_Get_Next(ctx);
        }

        ///
        /// Get active LoadShape by index; index is 1-based: 1..count
        ///
        int32_t idx() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LoadShapes_Get_idx(ctx);
        }

        ///
        /// Get active LoadShape by index; index is 1-based: 1..count
        ///
        void idx(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LoadShapes_Set_idx(ctx, value);
        }
        /// 
        /// Create a new LoadShape, with default parameters
        /// 
        int32_t New(const char *Name)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LoadShapes_New(ctx, Name);
        }
        /// 
        /// Create a new LoadShape, with default parameters
        /// 
        int32_t New(const string &Name)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LoadShapes_New(ctx, Name.c_str());
        }
        /// 
        /// Normalize the LoadShape data inplace
        /// 
        void Normalize()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LoadShapes_Normalize(ctx);
        }

        /// 
        /// Fixed interval time value, in hours.
        /// 
        /// Original COM help: https://opendss.epri.com/HrInterval.html
        /// 
        double HrInterval() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LoadShapes_Get_HrInterval(ctx);
        }
        ILoadShapes& HrInterval(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LoadShapes_Set_HrInterval(ctx, value);
            return *this;
        }

        /// 
        /// Fixed Interval time value, in minutes
        /// 
        /// Original COM help: https://opendss.epri.com/MinInterval.html
        /// 
        double MinInterval() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LoadShapes_Get_MinInterval(ctx);
        }
        ILoadShapes& MinInterval(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LoadShapes_Set_MinInterval(ctx, value);
            return *this;
        }

        /// 
        /// Get/set Number of points in active Loadshape.
        /// 
        /// Original COM help: https://opendss.epri.com/Npts.html
        /// 
        int32_t Npts() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LoadShapes_Get_Npts(ctx);
        }
        ILoadShapes& Npts(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LoadShapes_Set_Npts(ctx, value);
            return *this;
        }

        /// 
        /// Base P value for normalization. Default is zero, meaning the peak will be used.
        /// 
        /// Original COM help: https://opendss.epri.com/Pbase.html
        /// 
        double PBase() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LoadShapes_Get_PBase(ctx);
        }
        ILoadShapes& PBase(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LoadShapes_Set_PBase(ctx, value);
            return *this;
        }

        /// 
        /// Array of doubles for the P multiplier in the Loadshape.
        /// 
        /// Original COM help: https://opendss.epri.com/Pmult.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Pmult() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LoadShapes_Get_Pmult_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        ILoadShapes& Pmult(const VectorT &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LoadShapes_Set_Pmult(ctx, &value[0], value.size());
            return *this;
        }

        /// 
        /// Base for normalizing Q curve. If left at zero, the peak value is used.
        /// 
        /// Original COM help: https://opendss.epri.com/Qbase.html
        /// 
        double QBase() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LoadShapes_Get_Qbase(ctx);
        }
        ILoadShapes& QBase(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LoadShapes_Set_Qbase(ctx, value);
            return *this;
        }

        /// 
        /// Array of doubles containing the Q multipliers.
        /// 
        /// Original COM help: https://opendss.epri.com/Qmult.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Qmult() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LoadShapes_Get_Qmult_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        ILoadShapes& Qmult(const VectorT &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LoadShapes_Set_Qmult(ctx, &value[0], value.size());
            return *this;
        }

        /// 
        /// Time array in hours corresponding to P and Q multipliers when the Interval=0.
        /// 
        /// Original COM help: https://opendss.epri.com/TimeArray.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT TimeArray() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LoadShapes_Get_TimeArray_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        ILoadShapes& TimeArray(const VectorT &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LoadShapes_Set_TimeArray(ctx, &value[0], value.size());
            return *this;
        }

        /// 
        /// Boolean flag to let Loads know to use the actual value in the curve rather than use the value as a multiplier.
        /// 
        /// Original COM help: https://opendss.epri.com/UseActual.html
        /// 
        bool UseActual() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LoadShapes_Get_UseActual(ctx);
        }
        ILoadShapes& UseActual(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LoadShapes_Set_UseActual(ctx, value);
            return *this;
        }

        /// 
        /// Fixed interval time value, in seconds.
        /// 
        /// Original COM help: https://opendss.epri.com/Sinterval.html
        /// 
        double sInterval() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LoadShapes_Get_SInterval(ctx);
        }
        ILoadShapes& sInterval(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LoadShapes_Set_SInterval(ctx, value);
            return *this;
        }
        /// 
        /// Converts the current LoadShape data to float32/single precision.
        /// If there is no data or the data is already represented using float32, nothing is done.
        /// 
        /// **(API Extension)**
        /// 
        void UseFloat32()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LoadShapes_UseFloat32(ctx);
        }
        /// 
        /// Converts the current LoadShape data to float64/double precision.
        /// If there is no data or the data is already represented using float64, nothing is done.
        /// 
        /// **(API Extension)**
        /// 
        void UseFloat64()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LoadShapes_UseFloat64(ctx);
        }
    };

    class IMonitors: public ContextState
    {
    public:

        IMonitors(altdss::APIUtil *util) :
            ContextState(util)
        {
        }
    
        // TODO: Implement AsMatrix someday

        /// Array of float64 for the specified channel (usage: MyArray = DSSMonitor.Channel(i)).
        /// A Save or SaveAll should be executed first. Done automatically by most standard solution modes.
        /// Channels start at index 1.
        template <typename VectorT=Eigen::VectorXd>
        VectorT Channel(int32_t Index)
        {
            //TODO: use the better implementation
            ctx_Monitors_Get_Channel_GR(ctx, Index);
            return api_util->get_float64_gr_array<VectorT>();
        }

        ///
        /// Array of strings with all Monitor names in the circuit.
        ///
        strings AllNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Monitors_Get_AllNames);
        }

        ///
        /// Number of Monitor objects in active circuit.
        ///
        int32_t Count() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Monitors_Get_Count(ctx);
        }

        ///
        /// Sets the first Monitor active. Returns 0 if no more.
        ///
        int32_t First() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Monitors_Get_First(ctx);
        }

        ///
        /// Get the name of the current active Monitor
        ///
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Monitors_Get_Name(ctx);
        }

        ///
        /// Sets the active Monitor by Name.
        ///
        IMonitors& Name(const char *value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Monitors_Set_Name(ctx, value);
            return *this;
        }
        IMonitors& Name(const string &value)
        {
            return Name(value.c_str());
        }

        ///
        /// Sets the next Monitor active. Returns 0 if no more.
        ///
        int32_t Next() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Monitors_Get_Next(ctx);
        }

        ///
        /// Get active Monitor by index; index is 1-based: 1..count
        ///
        int32_t idx() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Monitors_Get_idx(ctx);
        }

        ///
        /// Get active Monitor by index; index is 1-based: 1..count
        ///
        void idx(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Monitors_Set_idx(ctx, value);
        }
        /// 
        /// Post-process monitor samples taken so far, e.g., Pst for mode=4.
        /// 
        /// Original COM help: https://opendss.epri.com/Process.html
        /// 
        void Process()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Monitors_Process(ctx);
        }
        /// 
        /// Post-process all monitor samples taken so far, e.g., Pst for mode=4.
        /// 
        /// Original COM help: https://opendss.epri.com/ProcessAll.html
        /// 
        void ProcessAll()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Monitors_ProcessAll(ctx);
        }
        /// 
        /// Reset active Monitor object.
        /// 
        /// Original COM help: https://opendss.epri.com/Reset3.html
        /// 
        void Reset()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Monitors_Reset(ctx);
        }
        /// 
        /// Reset all Monitor objects.
        /// 
        /// Original COM help: https://opendss.epri.com/ResetAll1.html
        /// 
        void ResetAll()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Monitors_ResetAll(ctx);
        }
        /// 
        /// Instruct the active Monitor to take a sample of the present state.
        /// 
        /// Original COM help: https://opendss.epri.com/Sample2.html
        /// 
        void Sample()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Monitors_Sample(ctx);
        }
        /// 
        /// Instruct all Monitor objects to take a sample of the present state.
        /// 
        /// Original COM help: https://opendss.epri.com/SampleAll1.html
        /// 
        void SampleAll()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Monitors_SampleAll(ctx);
        }
        /// 
        /// Instructs the active monitor to save its current sample buffer to its monitor stream. 
        /// 
        /// After the data is on the stream, you can access the ByteStream or channel data. 
        /// 
        /// **Most standard solution modes do this automatically.**
        /// 
        /// Original COM help: https://opendss.epri.com/Save1.html
        /// 
        void Save()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Monitors_Save(ctx);
        }
        /// 
        /// Instructs the all monitor objects to save their current sample buffers to the respective monitor streams.
        /// 
        /// **Most standard solution modes do this automatically.**
        /// 
        /// Original COM help: https://opendss.epri.com/SaveAll1.html
        /// 
        void SaveAll()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Monitors_SaveAll(ctx);
        }
        /// 
        /// Convert the monitor data to text and displays it with the text editor.
        /// 
        /// Original COM help: https://opendss.epri.com/Show3.html
        /// 
        void Show()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Monitors_Show(ctx);
        }

        /// 
        /// Byte Array containing monitor stream values. Make sure a "save" is done first (standard solution modes do this automatically)
        /// 
        /// Original COM help: https://opendss.epri.com/ByteStream.html
        /// 
        template <typename VectorT=Eigen::Matrix<int8_t, Eigen::Dynamic, 1>>
        VectorT ByteStream() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Monitors_Get_ByteStream_GR(ctx);
            return api_util->get_int8_gr_array<VectorT>();
        }

        /// 
        /// Full object name of element being monitored.
        /// 
        /// Original COM help: https://opendss.epri.com/Element.html
        /// 
        string Element() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Monitors_Get_Element(ctx);
        }
        IMonitors& Element(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Monitors_Set_Element(ctx, value);
            return *this;
        }
        IMonitors& Element(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Monitors_Set_Element(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Name of CSV file associated with active Monitor.
        /// 
        /// Original COM help: https://opendss.epri.com/FileName.html
        /// 
        string FileName() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Monitors_Get_FileName(ctx);
        }

        /// 
        /// Monitor File Version (integer)
        /// 
        /// Original COM help: https://opendss.epri.com/FileVersion.html
        /// 
        int32_t FileVersion() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Monitors_Get_FileVersion(ctx);
        }

        /// 
        /// Header string;  Array of strings containing Channel names
        /// 
        /// Original COM help: https://opendss.epri.com/Header.html
        /// 
        strings Header() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Monitors_Get_Header);
        }

        /// 
        /// Monitor mode (bitmask integer - see DSS Help)
        /// 
        /// Original COM help: https://opendss.epri.com/Mode1.html
        /// 
        int32_t Mode() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Monitors_Get_Mode(ctx);
        }
        IMonitors& Mode(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Monitors_Set_Mode(ctx, value);
            return *this;
        }

        /// 
        /// Number of Channels in the active Monitor
        /// 
        /// Original COM help: https://opendss.epri.com/NumChannels.html
        /// 
        int32_t NumChannels() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Monitors_Get_NumChannels(ctx);
        }

        /// 
        /// Size of each record in ByteStream (Integer). Same as NumChannels.
        /// 
        /// Original COM help: https://opendss.epri.com/RecordSize.html
        /// 
        int32_t RecordSize() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Monitors_Get_RecordSize(ctx);
        }

        /// 
        /// Number of Samples in Monitor at Present
        /// 
        /// Original COM help: https://opendss.epri.com/SampleCount.html
        /// 
        int32_t SampleCount() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Monitors_Get_SampleCount(ctx);
        }

        /// 
        /// Terminal number of element being monitored.
        /// 
        /// Original COM help: https://opendss.epri.com/Terminal.html
        /// 
        int32_t Terminal() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Monitors_Get_Terminal(ctx);
        }
        IMonitors& Terminal(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Monitors_Set_Terminal(ctx, value);
            return *this;
        }

        /// 
        /// Array of doubles containing frequency values for harmonics mode solutions; Empty for time mode solutions (use dblHour)
        /// 
        /// Original COM help: https://opendss.epri.com/dblFreq.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT dblFreq() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Monitors_Get_dblFreq_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Array of doubles containing time value in hours for time-sampled monitor values; Empty if frequency-sampled values for harmonics solution (see dblFreq)
        /// 
        /// Original COM help: https://opendss.epri.com/dblHour.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT dblHour() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Monitors_Get_dblHour_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }
    };

    class IParser: public ContextState
    {
    public:

        IParser(altdss::APIUtil *util) :
            ContextState(util)
        {
        }
        /// 
        /// Use this property to parse a Matrix token in OpenDSS format.  Returns square matrix of order specified. Order same as default Fortran order: column by column.
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Matrix(int32_t ExpectedOrder)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Parser_Get_Matrix_GR(ctx, ExpectedOrder);
            return api_util->get_float64_gr_array<VectorT>();
        }
        /// 
        /// Use this property to parse a matrix token specified in lower triangle form. Symmetry is forced.
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT SymMatrix(int32_t ExpectedOrder)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Parser_Get_SymMatrix_GR(ctx, ExpectedOrder);
            return api_util->get_float64_gr_array<VectorT>();
        }
        /// 
        /// Returns token as array of doubles. For parsing quoted array syntax.
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Vector(int32_t ExpectedSize)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Parser_Get_Vector_GR(ctx, ExpectedSize);
            return api_util->get_float64_gr_array<VectorT>();
        }
        /// 
        /// Reset the delimiters to their default values.
        /// 
        /// Original COM help: https://opendss.epri.com/ResetDelimiters.html        
        /// 
        void ResetDelimiters()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Parser_ResetDelimiters(ctx);
        }

        /// 
        /// Default is FALSE. If TRUE, the parser automatically advances to next token after DblValue, IntValue, or StrValue. Simpler when you don't need to check for parameter names.
        /// 
        /// Original COM help: https://opendss.epri.com/AutoIncrement.html
        /// 
        bool AutoIncrement() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Parser_Get_AutoIncrement(ctx);
        }
        IParser& AutoIncrement(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Parser_Set_AutoIncrement(ctx, value);
            return *this;
        }

        /// 
        /// Get/Set String containing the the characters for Quoting in OpenDSS scripts. Matching pairs defined in EndQuote. Default is "'([{.
        /// 
        /// Original COM help: https://opendss.epri.com/BeginQuote.html
        /// 
        string BeginQuote() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Parser_Get_BeginQuote(ctx);
        }
        IParser& BeginQuote(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Parser_Set_BeginQuote(ctx, value);
            return *this;
        }
        IParser& BeginQuote(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Parser_Set_BeginQuote(ctx, value.c_str());
            return *this;
        }

        /// 
        /// String to be parsed. Loading this string resets the Parser to the beginning of the line. Then parse off the tokens in sequence.
        /// 
        /// Original COM help: https://opendss.epri.com/CmdString.html
        /// 
        string CmdString() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Parser_Get_CmdString(ctx);
        }
        IParser& CmdString(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Parser_Set_CmdString(ctx, value);
            return *this;
        }
        IParser& CmdString(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Parser_Set_CmdString(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Return next parameter as a double.
        /// 
        /// Original COM help: https://opendss.epri.com/DblValue.html
        /// 
        double DblValue() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Parser_Get_DblValue(ctx);
        }

        /// 
        /// String defining hard delimiters used to separate token on the command string. Default is , and =. The = separates token name from token value. These override whitespace to separate tokens.
        /// 
        /// Original COM help: https://opendss.epri.com/Delimiters.html
        /// 
        string Delimiters() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Parser_Get_Delimiters(ctx);
        }
        IParser& Delimiters(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Parser_Set_Delimiters(ctx, value);
            return *this;
        }
        IParser& Delimiters(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Parser_Set_Delimiters(ctx, value.c_str());
            return *this;
        }

        /// 
        /// String containing characters, in order, that match the beginning quote characters in BeginQuote. Default is "')]}
        /// 
        /// Original COM help: https://opendss.epri.com/EndQuote.html
        /// 
        string EndQuote() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Parser_Get_EndQuote(ctx);
        }
        IParser& EndQuote(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Parser_Set_EndQuote(ctx, value);
            return *this;
        }
        IParser& EndQuote(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Parser_Set_EndQuote(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Return next parameter as a long integer.
        /// 
        /// Original COM help: https://opendss.epri.com/IntValue.html
        /// 
        int32_t IntValue() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Parser_Get_IntValue(ctx);
        }

        /// 
        /// Get next token and return tag name (before = sign) if any. See AutoIncrement.
        /// 
        /// Original COM help: https://opendss.epri.com/NextParam.html
        /// 
        string NextParam() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Parser_Get_NextParam(ctx);
        }

        /// 
        /// Return next parameter as a string
        /// 
        /// Original COM help: https://opendss.epri.com/StrValue.html
        /// 
        string StrValue() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Parser_Get_StrValue(ctx);
        }

        /// 
        /// Get/set the characters used for White space in the command string.  Default is blank and Tab.
        /// 
        /// Original COM help: https://opendss.epri.com/WhiteSpace.html
        /// 
        string WhiteSpace() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Parser_Get_WhiteSpace(ctx);
        }
        IParser& WhiteSpace(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Parser_Set_WhiteSpace(ctx, value);
            return *this;
        }
        IParser& WhiteSpace(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Parser_Set_WhiteSpace(ctx, value.c_str());
            return *this;
        }
    };

    class IReduceCkt: public ContextState
    {
    public:

        IReduceCkt(altdss::APIUtil *util) :
            ContextState(util)
        {
        }

        /// 
        /// Zmag (ohms) for Reduce Option for Z of short lines
        /// 
        /// Original COM help: https://opendss.epri.com/Zmag.html
        /// 
        double Zmag() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_ReduceCkt_Get_Zmag(ctx);
        }
        IReduceCkt& Zmag(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ReduceCkt_Set_Zmag(ctx, value);
            return *this;
        }

        /// 
        /// Keep load flag for Reduction options that remove branches
        /// 
        /// Original COM help: https://opendss.epri.com/KeepLoad.html
        /// 
        bool KeepLoad() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_ReduceCkt_Get_KeepLoad(ctx);
        }
        IReduceCkt& KeepLoad(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ReduceCkt_Set_KeepLoad(ctx, value);
            return *this;
        }

        /// 
        /// Edit String for RemoveBranches functions
        /// 
        /// Original COM help: https://opendss.epri.com/EditString.html
        /// 
        string EditString() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_ReduceCkt_Get_EditString(ctx);
        }
        IReduceCkt& EditString(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ReduceCkt_Set_EditString(ctx, value);
            return *this;
        }
        IReduceCkt& EditString(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ReduceCkt_Set_EditString(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Start element for Remove Branch function
        /// 
        /// Original COM help: https://opendss.epri.com/StartPDElement.html
        /// 
        string StartPDElement() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_ReduceCkt_Get_StartPDElement(ctx);
        }
        IReduceCkt& StartPDElement(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ReduceCkt_Set_StartPDElement(ctx, value);
            return *this;
        }
        IReduceCkt& StartPDElement(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ReduceCkt_Set_StartPDElement(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Name of EnergyMeter to use for reduction
        /// 
        /// Original COM help: https://opendss.epri.com/EnergyMeter1.html
        /// 
        string EnergyMeter() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_ReduceCkt_Get_EnergyMeter(ctx);
        }
        IReduceCkt& EnergyMeter(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ReduceCkt_Set_EnergyMeter(ctx, value);
            return *this;
        }
        IReduceCkt& EnergyMeter(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ReduceCkt_Set_EnergyMeter(ctx, value.c_str());
            return *this;
        }
        /// 
        /// Save present (reduced) circuit
        /// Filename is listed in the Text Result interface
        /// 
        void SaveCircuit(const char *CktName)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ReduceCkt_SaveCircuit(ctx, CktName);
        }
        /// 
        /// Save present (reduced) circuit
        /// Filename is listed in the Text Result interface
        /// 
        void SaveCircuit(const string &CktName)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ReduceCkt_SaveCircuit(ctx, CktName.c_str());
        }
        /// 
        /// Do Default Reduction algorithm
        /// 
        /// Original COM help: https://opendss.epri.com/DoDefault.html
        /// 
        void DoDefault()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ReduceCkt_DoDefault(ctx);
        }
        /// 
        /// Do ShortLines algorithm: Set Zmag first if you don't want the default
        /// 
        /// Original COM help: https://opendss.epri.com/DoShortLines.html
        /// 
        void DoShortLines()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ReduceCkt_DoShortLines(ctx);
        }
        /// 
        /// Reduce Dangling Algorithm; branches with nothing connected
        /// 
        /// Original COM help: https://opendss.epri.com/DoDangling.html
        /// 
        void DoDangling()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ReduceCkt_DoDangling(ctx);
        }
        /// 
        /// Break (disable) all the loops found in the active circuit.
        /// 
        /// Disables one of the Line objects at the head of a loop to force the circuit to be radial.
        /// 
        void DoLoopBreak()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ReduceCkt_DoLoopBreak(ctx);
        }
        /// 
        /// Merge all parallel lines found in the circuit to facilitate its reduction.
        /// 
        void DoParallelLines()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ReduceCkt_DoParallelLines(ctx);
        }
        /// 
        /// Merge Line objects in which the IsSwitch property is true with the down-line Line object.
        /// 
        void DoSwitches()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ReduceCkt_DoSwitches(ctx);
        }
        /// 
        /// Remove all 1-phase laterals in the active EnergyMeter's zone.
        /// 
        /// Loads and other shunt elements are moved to the parent 3-phase bus.
        /// 
        void Do1phLaterals()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ReduceCkt_Do1phLaterals(ctx);
        }
        /// 
        /// Remove (disable) all branches down-line from the active PDElement. 
        /// 
        /// Circuit must have an EnergyMeter on this branch.
        /// If KeepLoad=Y (default), a new Load element is defined and kW, kvar are set to present power flow solution for the first element eliminated. 
        /// The EditString is applied to each new Load element defined. 
        /// 
        void DoBranchRemove()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ReduceCkt_DoBranchRemove(ctx);
        }
    };

    class IYMatrix: public ContextState
    {
    public:

        IYMatrix(altdss::APIUtil *util) :
            ContextState(util)
        {
        }
        void ZeroInjCurr()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_YMatrix_ZeroInjCurr(ctx);
        }
        void GetSourceInjCurrents()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_YMatrix_GetSourceInjCurrents(ctx);
        }
        void GetPCInjCurr()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_YMatrix_GetPCInjCurr(ctx);
        }
        void BuildYMatrixD(int32_t BuildOps, int32_t AllocateVI)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_YMatrix_BuildYMatrixD(ctx, BuildOps, AllocateVI);
        }
        void AddInAuxCurrents(int32_t SType)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_YMatrix_AddInAuxCurrents(ctx, SType);
        }

        bool SystemYChanged() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_YMatrix_Get_SystemYChanged(ctx);
        }
        IYMatrix& SystemYChanged(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_YMatrix_Set_SystemYChanged(ctx, value);
            return *this;
        }

        bool UseAuxCurrents() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_YMatrix_Get_UseAuxCurrents(ctx);
        }
        IYMatrix& UseAuxCurrents(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_YMatrix_Set_UseAuxCurrents(ctx, value);
            return *this;
        }

        /// 
        /// Sparse solver options. See the enumeration SparseSolverOptions
        /// 
        uint64_t SolverOptions() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_YMatrix_Get_SolverOptions(ctx);
        }
        IYMatrix& SolverOptions(uint64_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_YMatrix_Set_SolverOptions(ctx, value);
            return *this;
        }
        bool CheckConvergence()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_YMatrix_CheckConvergence(ctx);
        }
        void SetGeneratordQdV()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_YMatrix_SetGeneratordQdV(ctx);
        }

        bool LoadsNeedUpdating() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_YMatrix_Get_LoadsNeedUpdating(ctx);
        }
        IYMatrix& LoadsNeedUpdating(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_YMatrix_Set_LoadsNeedUpdating(ctx, value);
            return *this;
        }

        bool SolutionInitialized() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_YMatrix_Get_SolutionInitialized(ctx);
        }
        IYMatrix& SolutionInitialized(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_YMatrix_Set_SolutionInitialized(ctx, value);
            return *this;
        }

        int32_t Iteration() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_YMatrix_Get_Iteration(ctx);
        }
        IYMatrix& Iteration(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_YMatrix_Set_Iteration(ctx, value);
            return *this;
        }
    };

    class IMeters: public ContextState
    {
    public:

        IMeters(altdss::APIUtil *util) :
            ContextState(util)
        {
        }
    
        ///
        /// Returns the list of all PCE within the area covered by the energy meter
        ///
        strings ZonePCE() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Meters_Get_ZonePCE);
        }

        ///
        /// Array of strings with all Meter names in the circuit.
        ///
        strings AllNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Meters_Get_AllNames);
        }

        ///
        /// Number of Meter objects in active circuit.
        ///
        int32_t Count() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_Count(ctx);
        }

        ///
        /// Sets the first Meter active. Returns 0 if no more.
        ///
        int32_t First() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_First(ctx);
        }

        ///
        /// Get the name of the current active Meter
        ///
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_Name(ctx);
        }

        ///
        /// Sets the active Meter by Name.
        ///
        IMeters& Name(const char *value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Meters_Set_Name(ctx, value);
            return *this;
        }
        IMeters& Name(const string &value)
        {
            return Name(value.c_str());
        }

        ///
        /// Sets the next Meter active. Returns 0 if no more.
        ///
        int32_t Next() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_Next(ctx);
        }

        ///
        /// Get active Meter by index; index is 1-based: 1..count
        ///
        int32_t idx() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_idx(ctx);
        }

        ///
        /// Get active Meter by index; index is 1-based: 1..count
        ///
        void idx(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Meters_Set_idx(ctx, value);
        }
        /// 
        /// Close All Demand Interval Files. Users are required to close the DI files at the end of a run.
        /// 
        /// Original COM help: https://opendss.epri.com/CloseAllDIFiles.html
        /// 
        void CloseAllDIFiles()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Meters_CloseAllDIFiles(ctx);
        }
        /// 
        /// Calculate reliability indices
        /// 
        /// Original COM help: https://opendss.epri.com/DoReliabilityCalc.html
        /// 
        void DoReliabilityCalc(bool AssumeRestoration)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Meters_DoReliabilityCalc(ctx, AssumeRestoration);
        }
        /// 
        /// Open Demand Interval (DI) files
        /// 
        /// Original COM help: https://opendss.epri.com/OpenAllDIFiles.html
        /// 
        void OpenAllDIFiles()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Meters_OpenAllDIFiles(ctx);
        }
        /// 
        /// Resets registers of active meter.
        /// 
        /// Original COM help: https://opendss.epri.com/Reset2.html
        /// 
        void Reset()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Meters_Reset(ctx);
        }
        /// 
        /// Resets registers of all meter objects.
        /// 
        /// Original COM help: https://opendss.epri.com/ResetAll.html
        /// 
        void ResetAll()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Meters_ResetAll(ctx);
        }
        /// 
        /// Forces active Meter to take a sample.
        /// 
        /// Original COM help: https://opendss.epri.com/Sample1.html
        /// 
        void Sample()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Meters_Sample(ctx);
        }
        /// 
        /// Causes all EnergyMeter objects to take a sample at the present time.
        /// 
        /// Original COM help: https://opendss.epri.com/SampleAll.html
        /// 
        void SampleAll()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Meters_SampleAll(ctx);
        }
        /// 
        /// Saves meter register values.
        /// 
        /// Original COM help: https://opendss.epri.com/Save.html
        /// 
        void Save()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Meters_Save(ctx);
        }
        /// 
        /// Save All EnergyMeter objects
        /// 
        /// Original COM help: https://opendss.epri.com/SaveAll.html
        /// 
        void SaveAll()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Meters_SaveAll(ctx);
        }
        void SetActiveSection(int32_t SectIdx)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Meters_SetActiveSection(ctx, SectIdx);
        }

        /// 
        /// List (strings) of all branches in zone of the active EnergyMeter object.
        /// 
        /// Original COM help: https://opendss.epri.com/AllBranchesInZone.html
        /// 
        strings AllBranchesInZone() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Meters_Get_AllBranchesInZone);
        }

        /// 
        /// Array of names of all zone end elements.
        /// 
        /// Original COM help: https://opendss.epri.com/AllEndElements.html
        /// 
        strings AllEndElements() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Meters_Get_AllEndElements);
        }

        /// 
        /// Array of doubles: set the phase allocation factors for the active meter.
        /// 
        /// Original COM help: https://opendss.epri.com/AllocFactors.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT AllocFactors() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Meters_Get_AllocFactors_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        IMeters& AllocFactors(const VectorT &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Meters_Set_AllocFactors(ctx, &value[0], value.size());
            return *this;
        }

        /// 
        /// Average Repair time in this section of the meter zone
        /// 
        /// Original COM help: https://opendss.epri.com/AvgRepairTime.html
        /// 
        double AvgRepairTime() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_AvgRepairTime(ctx);
        }

        /// 
        /// Set the magnitude of the real part of the Calculated Current (normally determined by solution) for the Meter to force some behavior on Load Allocation
        /// 
        /// Original COM help: https://opendss.epri.com/CalcCurrent.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT CalcCurrent() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Meters_Get_CalcCurrent_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        IMeters& CalcCurrent(const VectorT &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Meters_Set_CalcCurrent(ctx, &value[0], value.size());
            return *this;
        }

        /// 
        /// Number of branches in Active EnergyMeter zone. (Same as sequence list size)
        /// 
        /// Original COM help: https://opendss.epri.com/CountBranches.html
        /// 
        int32_t CountBranches() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_CountBranches(ctx);
        }

        /// 
        /// Number of zone end elements in the active meter zone.
        /// 
        /// Original COM help: https://opendss.epri.com/CountEndElements.html
        /// 
        int32_t CountEndElements() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_CountEndElements(ctx);
        }

        /// 
        /// Total customer interruptions for this Meter zone based on reliability calcs.
        /// 
        /// Original COM help: https://opendss.epri.com/CustInterrupts.html
        /// 
        double CustInterrupts() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_CustInterrupts(ctx);
        }

        /// 
        /// Global Flag in the DSS to indicate if Demand Interval (DI) files have been properly opened.
        /// 
        /// Original COM help: https://opendss.epri.com/DIFilesAreOpen.html
        /// 
        bool DIFilesAreOpen() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_DIFilesAreOpen(ctx);
        }

        /// 
        /// Sum of Fault Rate time Repair Hrs in this section of the meter zone
        /// 
        /// Original COM help: https://opendss.epri.com/FaultRateXRepairHrs.html
        /// 
        double FaultRateXRepairHrs() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_FaultRateXRepairHrs(ctx);
        }

        /// 
        /// Name of metered element
        /// 
        /// Original COM help: https://opendss.epri.com/MeteredElement.html
        /// 
        string MeteredElement() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_MeteredElement(ctx);
        }
        IMeters& MeteredElement(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Meters_Set_MeteredElement(ctx, value);
            return *this;
        }
        IMeters& MeteredElement(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Meters_Set_MeteredElement(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Number of Metered Terminal
        /// 
        /// Original COM help: https://opendss.epri.com/MeteredTerminal.html
        /// 
        int32_t MeteredTerminal() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_MeteredTerminal(ctx);
        }
        IMeters& MeteredTerminal(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Meters_Set_MeteredTerminal(ctx, value);
            return *this;
        }

        /// 
        /// Number of branches (lines) in this section
        /// 
        /// Original COM help: https://opendss.epri.com/NumSectionBranches.html
        /// 
        int32_t NumSectionBranches() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_NumSectionBranches(ctx);
        }

        /// 
        /// Number of Customers in the active section.
        /// 
        /// Original COM help: https://opendss.epri.com/NumSectionCustomers.html
        /// 
        int32_t NumSectionCustomers() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_NumSectionCustomers(ctx);
        }

        /// 
        /// Number of feeder sections in this meter's zone
        /// 
        /// Original COM help: https://opendss.epri.com/NumSections.html
        /// 
        int32_t NumSections() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_NumSections(ctx);
        }

        /// 
        /// Type of OCP device. 1=Fuse; 2=Recloser; 3=Relay
        /// 
        /// Original COM help: https://opendss.epri.com/OCPDeviceType.html
        /// 
        OCPDevType OCPDeviceType() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return static_cast<OCPDevType>(ctx_Meters_Get_OCPDeviceType(ctx));
        }

        /// 
        /// Array of doubles to set values of Peak Current property
        /// 
        /// Original COM help: https://opendss.epri.com/Peakcurrent.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Peakcurrent() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Meters_Get_Peakcurrent_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        IMeters& Peakcurrent(const VectorT &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Meters_Set_Peakcurrent(ctx, &value[0], value.size());
            return *this;
        }

        /// 
        /// Array of strings containing the names of the registers.
        /// 
        /// See also the enum `EnergyMeterRegisters` for the standard register names.
        /// Besides those listed in the enumeration, users may need to check `RegisterNames`
        /// in order to find a specific register index at runtime.
        /// 
        /// Original COM help: https://opendss.epri.com/RegisterNames1.html
        /// 
        strings RegisterNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Meters_Get_RegisterNames);
        }

        /// 
        /// Array of all the values contained in the Meter registers for the active Meter.
        /// 
        /// Original COM help: https://opendss.epri.com/RegisterValues1.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT RegisterValues() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Meters_Get_RegisterValues_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// SAIDI for this meter's zone. Execute DoReliabilityCalc first.
        /// 
        /// Original COM help: https://opendss.epri.com/SAIDI.html
        /// 
        double SAIDI() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_SAIDI(ctx);
        }

        /// 
        /// Returns SAIFI for this meter's Zone. Execute Reliability Calc method first.
        /// 
        /// Original COM help: https://opendss.epri.com/SAIFI.html
        /// 
        double SAIFI() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_SAIFI(ctx);
        }

        /// 
        /// SAIFI based on kW rather than number of customers. Get after reliability calcs.
        /// 
        /// Original COM help: https://opendss.epri.com/SAIFIKW.html
        /// 
        double SAIFIKW() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_SAIFIKW(ctx);
        }

        /// 
        /// SequenceIndex of the branch at the head of this section
        /// 
        /// Original COM help: https://opendss.epri.com/SectSeqIdx.html
        /// 
        int32_t SectSeqIdx() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_SectSeqIdx(ctx);
        }

        /// 
        /// Total Customers downline from this section
        /// 
        /// Original COM help: https://opendss.epri.com/SectTotalCust.html
        /// 
        int32_t SectTotalCust() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_SectTotalCust(ctx);
        }

        /// 
        /// Size of the Sequence List
        /// 
        /// Original COM help: https://opendss.epri.com/SeqListSize.html
        /// 
        int32_t SeqListSize() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_SeqListSize(ctx);
        }

        /// 
        /// Get/set Index into Meter's SequenceList that contains branch pointers in lexical order. 
        /// Earlier index guaranteed to be upline from later index. Sets PDelement active.
        /// 
        /// Original COM help: https://opendss.epri.com/SequenceIndex.html
        /// 
        int32_t SequenceIndex() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_SequenceIndex(ctx);
        }
        IMeters& SequenceIndex(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Meters_Set_SequenceIndex(ctx, value);
            return *this;
        }

        /// 
        /// Sum of the branch fault rates in this section of the meter's zone
        /// 
        /// Original COM help: https://opendss.epri.com/SumBranchFltRates.html
        /// 
        double SumBranchFltRates() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_SumBranchFltRates(ctx);
        }

        /// 
        /// Total Number of customers in this zone (downline from the EnergyMeter)
        /// 
        /// Original COM help: https://opendss.epri.com/TotalCustomers.html
        /// 
        int32_t TotalCustomers() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_TotalCustomers(ctx);
        }

        /// 
        /// Totals of all registers of all meters
        /// 
        /// Original COM help: https://opendss.epri.com/Totals.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Totals() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Meters_Get_Totals_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

    };

    class IPDElements: public ContextState
    {
    public:

        IPDElements(altdss::APIUtil *util) :
            ContextState(util)
        {
        }

        /// 
        /// Accumulated failure rate for this branch on downline
        /// 
        /// *Requires a previous call to `RelCalc` command*
        /// 
        /// Original COM help: https://opendss.epri.com/AccumulatedL.html
        /// 
        double AccumulatedL() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PDElements_Get_AccumulatedL(ctx);
        }

        /// 
        /// Number of PD elements (including disabled elements)
        /// 
        /// Original COM help: https://opendss.epri.com/Count12.html
        /// 
        int32_t Count() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PDElements_Get_Count(ctx);
        }

        /// 
        /// Get/Set Number of failures per year. 
        /// For LINE elements: Number of failures per unit length per year.
        /// 
        double FaultRate() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PDElements_Get_FaultRate(ctx);
        }
        IPDElements& FaultRate(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PDElements_Set_FaultRate(ctx, value);
            return *this;
        }

        /// 
        /// Set the first enabled PD element to be the active element.
        /// Returns 0 if none found.
        /// 
        int32_t First() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PDElements_Get_First(ctx);
        }

        /// 
        /// Number of the terminal of active PD element that is on the "from" 
        /// side. This is set after the meter zone is determined.
        /// 
        /// *Requires an energy meter with an updated zone.*
        /// 
        int32_t FromTerminal() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PDElements_Get_FromTerminal(ctx);
        }

        /// 
        /// Boolean indicating of PD element should be treated as a shunt 
        /// element rather than a series element. Applies to Capacitor and Reactor 
        /// elements in particular.
        /// 
        bool IsShunt() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PDElements_Get_IsShunt(ctx);
        }

        /// 
        /// Failure rate for this branch. Faults per year including length of line.
        /// 
        /// *Requires a previous call to `RelCalc` command*
        /// 
        /// Original COM help: https://opendss.epri.com/Lambda1.html
        /// 
        double Lambda() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PDElements_Get_Lambda(ctx);
        }

        /// 
        /// Get/Set name of active PD Element. Returns null string if active element 
        /// is not PDElement type.
        /// 
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PDElements_Get_Name(ctx);
        }
        IPDElements& Name(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PDElements_Set_Name(ctx, value);
            return *this;
        }
        IPDElements& Name(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PDElements_Set_Name(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Advance to the next PD element in the circuit. Enabled elements 
        /// only. Returns 0 when no more elements.
        /// 
        int32_t Next() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PDElements_Get_Next(ctx);
        }

        /// 
        /// Number of customers, this branch
        /// 
        /// *Requires an energy meter with an updated zone.*
        /// 
        /// Original COM help: https://opendss.epri.com/Numcustomers.html
        /// 
        int32_t Numcustomers() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PDElements_Get_Numcustomers(ctx);
        }

        /// 
        /// Sets the parent PD element to be the active circuit element.
        /// Returns 0 if no more elements upline.
        /// 
        /// *Requires an energy meter with an updated zone.*
        /// 
        int32_t ParentPDElement() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PDElements_Get_ParentPDElement(ctx);
        }

        /// 
        /// Average repair time for this element in hours
        /// 
        /// Original COM help: https://opendss.epri.com/RepairTime.html
        /// 
        double RepairTime() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PDElements_Get_RepairTime(ctx);
        }
        IPDElements& RepairTime(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PDElements_Set_RepairTime(ctx, value);
            return *this;
        }

        /// 
        /// Integer ID of the feeder section that this PDElement branch is part of
        /// 
        /// *Requires a previous call to `RelCalc` command*
        /// 
        /// Original COM help: https://opendss.epri.com/SectionID1.html
        /// 
        int32_t SectionID() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PDElements_Get_SectionID(ctx);
        }

        /// 
        /// Total miles of line from this element to the end of the zone. For recloser siting algorithm.
        /// 
        /// *Requires a previous call to `RelCalc` command*
        /// 
        /// Original COM help: https://opendss.epri.com/TotalMiles1.html
        /// 
        double TotalMiles() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PDElements_Get_TotalMiles(ctx);
        }

        /// 
        /// Total number of customers from this branch to the end of the zone
        /// 
        /// *Requires a circuit with an energy meter with an updated zone.*
        /// 
        /// Original COM help: https://opendss.epri.com/TotalCustomers1.html
        /// 
        int32_t Totalcustomers() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PDElements_Get_Totalcustomers(ctx);
        }

        /// 
        /// Get/Set percent of faults that are permanent (require repair). Otherwise, fault is assumed to be transient/temporary.
        /// 
        /// Original COM help: https://opendss.epri.com/pctPermanent.html
        /// 
        double pctPermanent() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PDElements_Get_pctPermanent(ctx);
        }
        IPDElements& pctPermanent(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PDElements_Set_pctPermanent(ctx, value);
            return *this;
        }

        /// 
        /// Array of strings consisting of all PD element names.
        /// 
        /// **(API Extension)**
        /// 
        strings AllNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_PDElements_Get_AllNames);
        }
        /// 
        /// Array of doubles with the maximum current across the conductors, for each PD 
        /// element.
        /// 
        /// By default, only the *first terminal* is used for the maximum current, matching
        /// the behavior of the "export capacity" command. Pass `true` to 
        /// force the analysis to all terminals.
        /// 
        /// See also: 
        /// https://sourceforge.net/p/electricdss/discussion/beginners/thread/da5b93ca/
        /// 
        /// **(API Extension)**
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT AllMaxCurrents(bool AllNodes=false)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PDElements_Get_AllMaxCurrents_GR(ctx, AllNodes);
            return api_util->get_float64_gr_array<VectorT>();
        }
        /// 
        /// Array of doubles with the maximum current across the conductors as a percentage 
        /// of the Normal Ampere Rating, for each PD element.
        /// 
        /// By default, only the *first terminal* is used for the maximum current, matching
        /// the behavior of the "export capacity" command. Pass `true` to 
        /// force the analysis to all terminals.
        /// 
        /// See also: 
        /// https://sourceforge.net/p/electricdss/discussion/beginners/thread/da5b93ca/
        /// 
        /// **(API Extension)**
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT AllPctNorm(bool AllNodes=false)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PDElements_Get_AllPctNorm_GR(ctx, AllNodes);
            return api_util->get_float64_gr_array<VectorT>();
        }
        /// 
        /// Array of doubles with the maximum current across the conductors as a percentage
        /// of the Emergency Ampere Rating, for each PD element.
        /// 
        /// By default, only the *first terminal* is used for the maximum current, matching
        /// the behavior of the "export capacity" command. Pass `true` to 
        /// force the analysis to all terminals.
        /// 
        /// See also: 
        /// https://sourceforge.net/p/electricdss/discussion/beginners/thread/da5b93ca/
        /// 
        /// **(API Extension)**
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT AllPctEmerg(bool AllNodes=false)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PDElements_Get_AllPctEmerg_GR(ctx, AllNodes);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Complex array of currents for all conductors, all terminals, for each PD element.
        /// 
        /// **(API Extension)**
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT AllCurrents() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PDElements_Get_AllCurrents_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Complex array (magnitude and angle format) of currents for all conductors, all terminals, for each PD element.
        /// 
        /// **(API Extension)**
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT AllCurrentsMagAng() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PDElements_Get_AllCurrentsMagAng_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Complex double array of Sequence Currents for all conductors of all terminals, for each PD elements.
        /// 
        /// **(API Extension)**
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT AllCplxSeqCurrents() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PDElements_Get_AllCplxSeqCurrents_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Double array of the symmetrical component currents (magnitudes only) into each 3-phase terminal, for each PD element.
        /// 
        /// **(API Extension)**
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT AllSeqCurrents() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PDElements_Get_AllSeqCurrents_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Complex array of powers into each conductor of each terminal, for each PD element.
        /// 
        /// **(API Extension)**
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT AllPowers() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PDElements_Get_AllPowers_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Complex array of sequence powers into each 3-phase terminal, for each PD element
        /// 
        /// **(API Extension)**
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT AllSeqPowers() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PDElements_Get_AllSeqPowers_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Integer array listing the number of phases of all PD elements
        /// 
        /// **(API Extension)**
        /// 
        template <typename VectorT=Eigen::Matrix<int32_t, Eigen::Dynamic, 1>>
        VectorT AllNumPhases() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PDElements_Get_AllNumPhases_GR(ctx);
            return api_util->get_int32_gr_array<VectorT>();
        }

        /// 
        /// Integer array listing the number of conductors of all PD elements
        /// 
        /// **(API Extension)**
        /// 
        template <typename VectorT=Eigen::Matrix<int32_t, Eigen::Dynamic, 1>>
        VectorT AllNumConductors() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PDElements_Get_AllNumConductors_GR(ctx);
            return api_util->get_int32_gr_array<VectorT>();
        }

        /// 
        /// Integer array listing the number of terminals of all PD elements
        /// 
        /// **(API Extension)**
        /// 
        template <typename VectorT=Eigen::Matrix<int32_t, Eigen::Dynamic, 1>>
        VectorT AllNumTerminals() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PDElements_Get_AllNumTerminals_GR(ctx);
            return api_util->get_int32_gr_array<VectorT>();
        }
    };

    class IPVSystems: public ContextState
    {
    public:

        IPVSystems(altdss::APIUtil *util) :
            ContextState(util)
        {
        }

        ///
        /// Array of strings with all PVSystem names in the circuit.
        ///
        strings AllNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_PVSystems_Get_AllNames);
        }

        ///
        /// Number of PVSystem objects in active circuit.
        ///
        int32_t Count() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PVSystems_Get_Count(ctx);
        }

        ///
        /// Sets the first PVSystem active. Returns 0 if no more.
        ///
        int32_t First() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PVSystems_Get_First(ctx);
        }

        ///
        /// Get the name of the current active PVSystem
        ///
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PVSystems_Get_Name(ctx);
        }

        ///
        /// Sets the active PVSystem by Name.
        ///
        IPVSystems& Name(const char *value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PVSystems_Set_Name(ctx, value);
            return *this;
        }
        IPVSystems& Name(const string &value)
        {
            return Name(value.c_str());
        }

        ///
        /// Sets the next PVSystem active. Returns 0 if no more.
        ///
        int32_t Next() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PVSystems_Get_Next(ctx);
        }

        ///
        /// Get active PVSystem by index; index is 1-based: 1..count
        ///
        int32_t idx() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PVSystems_Get_idx(ctx);
        }

        ///
        /// Get active PVSystem by index; index is 1-based: 1..count
        ///
        void idx(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PVSystems_Set_idx(ctx, value);
        }

        /// 
        /// Get/set the present value of the Irradiance property in kW/m²
        /// 
        /// Original COM help: https://opendss.epri.com/Irradiance.html
        /// 
        double Irradiance() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PVSystems_Get_Irradiance(ctx);
        }
        IPVSystems& Irradiance(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PVSystems_Set_Irradiance(ctx, value);
            return *this;
        }

        /// 
        /// Get/set the power factor for the active PVSystem
        /// 
        /// Original COM help: https://opendss.epri.com/PF2.html
        /// 
        double PF() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PVSystems_Get_PF(ctx);
        }
        IPVSystems& PF(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PVSystems_Set_PF(ctx, value);
            return *this;
        }

        /// 
        /// Array of PVSystem energy meter register names
        /// 
        /// See also the enum `GeneratorRegisters`.
        /// 
        /// Original COM help: https://opendss.epri.com/RegisterNames2.html
        /// 
        strings RegisterNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_PVSystems_Get_RegisterNames);
        }

        /// 
        /// Array of doubles containing values in PVSystem registers.
        /// 
        /// Original COM help: https://opendss.epri.com/RegisterValues2.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT RegisterValues() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PVSystems_Get_RegisterValues_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Get/set Rated kVA of the PVSystem
        /// 
        /// Original COM help: https://opendss.epri.com/kVArated1.html
        /// 
        double kVArated() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PVSystems_Get_kVArated(ctx);
        }
        IPVSystems& kVArated(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PVSystems_Set_kVArated(ctx, value);
            return *this;
        }

        /// 
        /// Get kW output
        /// 
        /// Original COM help: https://opendss.epri.com/kW2.html
        /// 
        double kW() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PVSystems_Get_kW(ctx);
        }

        /// 
        /// Get/set kvar output value
        /// 
        /// Original COM help: https://opendss.epri.com/kvar2.html
        /// 
        double kvar() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PVSystems_Get_kvar(ctx);
        }
        IPVSystems& kvar(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PVSystems_Set_kvar(ctx, value);
            return *this;
        }

        /// 
        /// Name of the dispatch shape to use for daily simulations. Must be previously
        /// defined as a Loadshape object of 24 hrs, typically. In the default dispatch
        /// mode, the PVSystem element uses this loadshape to trigger State changes.
        /// 
        /// **(API Extension)**
        /// 
        string daily() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PVSystems_Get_daily(ctx);
        }
        IPVSystems& daily(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PVSystems_Set_daily(ctx, value);
            return *this;
        }
        IPVSystems& daily(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PVSystems_Set_daily(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Name of the load shape to use for duty cycle dispatch simulations such as
        /// for solar ramp rate studies. Must be previously defined as a Loadshape
        /// object. Typically would have time intervals of 1-5 seconds.
        /// 
        /// **(API Extension)**
        /// 
        string duty() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PVSystems_Get_duty(ctx);
        }
        IPVSystems& duty(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PVSystems_Set_duty(ctx, value);
            return *this;
        }
        IPVSystems& duty(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PVSystems_Set_duty(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Dispatch shape to use for yearly simulations. Must be previously defined
        /// as a Loadshape object. If this is not specified, the Daily dispatch shape,
        /// if any, is repeated during Yearly solution modes. In the default dispatch
        /// mode, the PVSystem element uses this loadshape to trigger State changes.
        /// 
        /// **(API Extension)**
        /// 
        string yearly() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PVSystems_Get_yearly(ctx);
        }
        IPVSystems& yearly(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PVSystems_Set_yearly(ctx, value);
            return *this;
        }
        IPVSystems& yearly(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PVSystems_Set_yearly(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Temperature shape to use for daily simulations. Must be previously defined
        /// as a TShape object of 24 hrs, typically. The PVSystem element uses this
        /// TShape to determine the Pmpp from the Pmpp vs T curve. Units must agree
        /// with the Pmpp vs T curve.
        /// 
        /// **(API Extension)**
        /// 
        string Tdaily() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PVSystems_Get_Tdaily(ctx);
        }
        IPVSystems& Tdaily(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PVSystems_Set_Tdaily(ctx, value);
            return *this;
        }
        IPVSystems& Tdaily(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PVSystems_Set_Tdaily(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Temperature shape to use for duty cycle dispatch simulations such as for
        /// solar ramp rate studies. Must be previously defined as a TShape object.
        /// Typically would have time intervals of 1-5 seconds. Designate the number
        /// of points to solve using the Set Number=xxxx command. If there are fewer
        /// points in the actual shape, the shape is assumed to repeat. The PVSystem
        /// model uses this TShape to determine the Pmpp from the Pmpp vs T curve.
        /// Units must agree with the Pmpp vs T curve.
        /// 
        /// **(API Extension)**
        /// 
        string Tduty() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PVSystems_Get_Tduty(ctx);
        }
        IPVSystems& Tduty(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PVSystems_Set_Tduty(ctx, value);
            return *this;
        }
        IPVSystems& Tduty(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PVSystems_Set_Tduty(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Temperature shape to use for yearly simulations. Must be previously defined
        /// as a TShape object. If this is not specified, the Daily dispatch shape, if
        /// any, is repeated during Yearly solution modes. The PVSystem element uses
        /// this TShape to determine the Pmpp from the Pmpp vs T curve. Units must
        /// agree with the Pmpp vs T curve.
        /// 
        /// **(API Extension)**
        /// 
        string Tyearly() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PVSystems_Get_Tyearly(ctx);
        }
        IPVSystems& Tyearly(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PVSystems_Set_Tyearly(ctx, value);
            return *this;
        }
        IPVSystems& Tyearly(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PVSystems_Set_Tyearly(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Returns the current irradiance value for the active PVSystem. Use it to 
        /// know what's the current irradiance value for the PV during a simulation.
        /// 
        /// Original COM help: https://opendss.epri.com/IrradianceNow.html
        /// 
        double IrradianceNow() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PVSystems_Get_IrradianceNow(ctx);
        }

        /// 
        /// Gets/sets the rated max power of the PV array for 1.0 kW/m² irradiance 
        /// and a user-selected array temperature of the active PVSystem.
        /// 
        /// Original COM help: https://opendss.epri.com/Pmpp.html
        /// 
        double Pmpp() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PVSystems_Get_Pmpp(ctx);
        }
        IPVSystems& Pmpp(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PVSystems_Set_Pmpp(ctx, value);
            return *this;
        }

        /// 
        /// Name of the sensor monitoring this element.
        /// 
        /// Original COM help: https://opendss.epri.com/Sensor1.html
        /// 
        string Sensor() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PVSystems_Get_Sensor(ctx);
        }
    };

    class IParallel: public ContextState
    {
    public:

        IParallel(altdss::APIUtil *util) :
            ContextState(util)
        {
        }
        /// 
        /// Create a new actor, if there are still cores available.
        /// 
        void CreateActor()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Parallel_CreateActor(ctx);
        }
        /// 
        /// Suspends the host's thread until all the OpenDSS running jobs finish.
        /// 
        /// Original COM help: https://opendss.epri.com/Wait.html
        /// 
        void Wait()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Parallel_Wait(ctx);
        }

        /// 
        /// Gets/sets the ID of the Active Actor
        /// 
        /// Original COM help: https://opendss.epri.com/ActiveActor.html
        /// 
        int32_t ActiveActor() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Parallel_Get_ActiveActor(ctx);
        }
        IParallel& ActiveActor(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Parallel_Set_ActiveActor(ctx, value);
            return *this;
        }

        /// 
        /// (read) Sets ON/OFF (1/0) Parallel features of the Engine
        /// (write) Delivers if the Parallel features of the Engine are Active
        /// 
        /// Original COM help: https://opendss.epri.com/ActiveParallel.html
        /// 
        int32_t ActiveParallel() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Parallel_Get_ActiveParallel(ctx);
        }
        IParallel& ActiveParallel(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Parallel_Set_ActiveParallel(ctx, value);
            return *this;
        }

        /// 
        /// Gets/sets the CPU of the Active Actor
        /// 
        /// Original COM help: https://opendss.epri.com/ActorCPU.html
        /// 
        int32_t ActorCPU() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Parallel_Get_ActorCPU(ctx);
        }
        IParallel& ActorCPU(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Parallel_Set_ActorCPU(ctx, value);
            return *this;
        }

        /// 
        /// Gets the progress of all existing actors in pct
        /// 
        /// Original COM help: https://opendss.epri.com/ActorProgress.html
        /// 
        template <typename VectorT=Eigen::Matrix<int32_t, Eigen::Dynamic, 1>>
        VectorT ActorProgress() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Parallel_Get_ActorProgress_GR(ctx);
            return api_util->get_int32_gr_array<VectorT>();
        }

        /// 
        /// Gets the status of each actor
        /// 
        /// Original COM help: https://opendss.epri.com/ActorStatus.html
        /// 
        template <typename VectorT=Eigen::Matrix<int32_t, Eigen::Dynamic, 1>>
        VectorT ActorStatus() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Parallel_Get_ActorStatus_GR(ctx);
            return api_util->get_int32_gr_array<VectorT>();
        }

        /// 
        /// (read) Reads the values of the ConcatenateReports option (1=enabled, 0=disabled)
        /// (write) Enable/Disable (1/0) the ConcatenateReports option for extracting monitors data
        /// 
        /// Original COM help: https://opendss.epri.com/ConcatenateReports.html
        /// 
        int32_t ConcatenateReports() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Parallel_Get_ConcatenateReports(ctx);
        }
        IParallel& ConcatenateReports(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Parallel_Set_ConcatenateReports(ctx, value);
            return *this;
        }

        /// 
        /// Delivers the number of CPUs on the current PC
        /// 
        /// Original COM help: https://opendss.epri.com/NumCPUs.html
        /// 
        int32_t NumCPUs() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Parallel_Get_NumCPUs(ctx);
        }

        /// 
        /// Delivers the number of Cores of the local PC
        /// 
        /// Original COM help: https://opendss.epri.com/NumCores.html
        /// 
        int32_t NumCores() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Parallel_Get_NumCores(ctx);
        }

        /// 
        /// Gets the number of Actors created
        /// 
        /// Original COM help: https://opendss.epri.com/NumOfActors.html
        /// 
        int32_t NumOfActors() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Parallel_Get_NumOfActors(ctx);
        }
    };

    class IReactors: public ContextState
    {
    public:

        IReactors(altdss::APIUtil *util) :
            ContextState(util)
        {
        }

        ///
        /// Array of strings with all Reactor names in the circuit.
        ///
        strings AllNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Reactors_Get_AllNames);
        }

        ///
        /// Number of Reactor objects in active circuit.
        ///
        int32_t Count() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reactors_Get_Count(ctx);
        }

        ///
        /// Sets the first Reactor active. Returns 0 if no more.
        ///
        int32_t First() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reactors_Get_First(ctx);
        }

        ///
        /// Get the name of the current active Reactor
        ///
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reactors_Get_Name(ctx);
        }

        ///
        /// Sets the active Reactor by Name.
        ///
        IReactors& Name(const char *value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reactors_Set_Name(ctx, value);
            return *this;
        }
        IReactors& Name(const string &value)
        {
            return Name(value.c_str());
        }

        ///
        /// Sets the next Reactor active. Returns 0 if no more.
        ///
        int32_t Next() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reactors_Get_Next(ctx);
        }

        ///
        /// Get active Reactor by index; index is 1-based: 1..count
        ///
        int32_t idx() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reactors_Get_idx(ctx);
        }

        ///
        /// Get active Reactor by index; index is 1-based: 1..count
        ///
        void idx(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reactors_Set_idx(ctx, value);
        }

        /// 
        /// How the reactor data was provided: 1=kvar, 2=R+jX, 3=R and X matrices, 4=sym components.
        /// Depending on this value, only some properties are filled or make sense in the context.
        /// 
        /// **(API Extension)**
        /// 
        int32_t SpecType() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reactors_Get_SpecType(ctx);
        }

        /// 
        /// Delta connection or wye?
        /// 
        /// **(API Extension)**
        /// 
        bool IsDelta() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reactors_Get_IsDelta(ctx);
        }
        IReactors& IsDelta(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reactors_Set_IsDelta(ctx, value);
            return *this;
        }

        /// 
        /// Indicates whether Rmatrix and Xmatrix are to be considered in parallel.
        /// 
        bool Parallel() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reactors_Get_Parallel(ctx);
        }
        IReactors& Parallel(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reactors_Set_Parallel(ctx, value);
            return *this;
        }

        /// 
        /// Inductance, mH. Alternate way to define the reactance, X, property.
        /// 
        double LmH() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reactors_Get_LmH(ctx);
        }
        IReactors& LmH(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reactors_Set_LmH(ctx, value);
            return *this;
        }

        /// 
        /// For 2, 3-phase, kV phase-phase. Otherwise specify actual coil rating.
        /// 
        double kV() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reactors_Get_kV(ctx);
        }
        IReactors& kV(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reactors_Set_kV(ctx, value);
            return *this;
        }

        /// 
        /// Total kvar, all phases.  Evenly divided among phases. Only determines X. Specify R separately
        /// 
        double kvar() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reactors_Get_kvar(ctx);
        }
        IReactors& kvar(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reactors_Set_kvar(ctx, value);
            return *this;
        }

        /// 
        /// Number of phases.
        /// 
        /// **(API Extension)**
        /// 
        int32_t Phases() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reactors_Get_Phases(ctx);
        }
        IReactors& Phases(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reactors_Set_Phases(ctx, value);
            return *this;
        }

        /// 
        /// Name of first bus.
        /// Bus2 property will default to this bus, node 0, unless previously specified.
        /// Only Bus1 need be specified for a Yg shunt reactor.
        /// 
        /// **(API Extension)**
        /// 
        string Bus1() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reactors_Get_Bus1(ctx);
        }
        IReactors& Bus1(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reactors_Set_Bus1(ctx, value);
            return *this;
        }
        IReactors& Bus1(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reactors_Set_Bus1(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Name of 2nd bus. Defaults to all phases connected to first bus, node 0, (Shunt Wye Connection) except when Bus2 is specifically defined.
        /// Not necessary to specify for delta (LL) connection.
        /// 
        /// **(API Extension)**
        /// 
        string Bus2() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reactors_Get_Bus2(ctx);
        }
        IReactors& Bus2(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reactors_Set_Bus2(ctx, value);
            return *this;
        }
        IReactors& Bus2(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reactors_Set_Bus2(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Name of XYCurve object, previously defined, describing per-unit variation of phase inductance, L=X/w, vs. frequency. Applies to reactance specified by X, LmH, Z, or kvar property. L generally decreases somewhat with frequency above the base frequency, approaching a limit at a few kHz.
        /// 
        string LCurve() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reactors_Get_LCurve(ctx);
        }
        IReactors& LCurve(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reactors_Set_LCurve(ctx, value);
            return *this;
        }
        IReactors& LCurve(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reactors_Set_LCurve(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Name of XYCurve object, previously defined, describing per-unit variation of phase resistance, R, vs. frequency. Applies to resistance specified by R or Z property. If actual values are not known, R often increases by approximately the square root of frequency.
        /// 
        string RCurve() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reactors_Get_RCurve(ctx);
        }
        IReactors& RCurve(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reactors_Set_RCurve(ctx, value);
            return *this;
        }
        IReactors& RCurve(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reactors_Set_RCurve(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Resistance (in series with reactance), each phase, ohms. This property applies to REACTOR specified by either kvar or X. See also help on Z.
        /// 
        double R() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reactors_Get_R(ctx);
        }
        IReactors& R(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reactors_Set_R(ctx, value);
            return *this;
        }

        /// 
        /// Reactance, each phase, ohms at base frequency. See also help on Z and LmH properties.
        /// 
        double X() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reactors_Get_X(ctx);
        }
        IReactors& X(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reactors_Set_X(ctx, value);
            return *this;
        }

        /// 
        /// Resistance in parallel with R and X (the entire branch). Assumed infinite if not specified.
        /// 
        double Rp() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reactors_Get_Rp(ctx);
        }
        IReactors& Rp(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reactors_Set_Rp(ctx, value);
            return *this;
        }

        /// 
        /// Resistance matrix, ohms at base frequency. Order of the matrix is the number of phases. Mutually exclusive to specifying parameters by kvar or X.
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Rmatrix() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reactors_Get_Rmatrix_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        IReactors& Rmatrix(const VectorT &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reactors_Set_Rmatrix(ctx, &value[0], value.size());
            return *this;
        }

        /// 
        /// Reactance matrix, ohms at base frequency. Order of the matrix is the number of phases. Mutually exclusive to specifying parameters by kvar or X.
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Xmatrix() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reactors_Get_Xmatrix_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        IReactors& Xmatrix(const VectorT &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reactors_Set_Xmatrix(ctx, &value[0], value.size());
            return *this;
        }

        /// 
        /// Alternative way of defining R and X properties. Enter a 2-element array representing R +jX in ohms.
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Z() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reactors_Get_Z_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        IReactors& Z(const VectorT &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reactors_Set_Z(ctx, &value[0], value.size());
            return *this;
        }

        /// 
        /// Positive-sequence impedance, ohms, as a 2-element array representing a complex number.
        /// 
        /// If defined, Z1, Z2, and Z0 are used to define the impedance matrix of the REACTOR.
        /// 
        /// Z1 MUST BE DEFINED TO USE THIS OPTION FOR DEFINING THE MATRIX.
        /// 
        /// Side Effect: Sets Z2 and Z0 to same values unless they were previously defined.
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Z1() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reactors_Get_Z1_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        IReactors& Z1(const VectorT &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reactors_Set_Z1(ctx, &value[0], value.size());
            return *this;
        }

        /// 
        /// Negative-sequence impedance, ohms, as a 2-element array representing a complex number.
        /// 
        /// Used to define the impedance matrix of the REACTOR if Z1 is also specified.
        /// 
        /// Note: Z2 defaults to Z1 if it is not specifically defined. If Z2 is not equal to Z1, the impedance matrix is asymmetrical.
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Z2() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reactors_Get_Z2_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        IReactors& Z2(const VectorT &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reactors_Set_Z2(ctx, &value[0], value.size());
            return *this;
        }

        /// 
        /// Zero-sequence impedance, ohms, as a 2-element array representing a complex number.
        /// 
        /// Used to define the impedance matrix of the REACTOR if Z1 is also specified.
        /// 
        /// Note: Z0 defaults to Z1 if it is not specifically defined.
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Z0() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reactors_Get_Z0_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        IReactors& Z0(const VectorT &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reactors_Set_Z0(ctx, &value[0], value.size());
            return *this;
        }
    };

    class IReclosers: public ContextState
    {
    public:

        IReclosers(altdss::APIUtil *util) :
            ContextState(util)
        {
        }

        ///
        /// Array of strings with all Recloser names in the circuit.
        ///
        strings AllNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Reclosers_Get_AllNames);
        }

        ///
        /// Number of Recloser objects in active circuit.
        ///
        int32_t Count() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reclosers_Get_Count(ctx);
        }

        ///
        /// Sets the first Recloser active. Returns 0 if no more.
        ///
        int32_t First() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reclosers_Get_First(ctx);
        }

        ///
        /// Get the name of the current active Recloser
        ///
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reclosers_Get_Name(ctx);
        }

        ///
        /// Sets the active Recloser by Name.
        ///
        IReclosers& Name(const char *value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reclosers_Set_Name(ctx, value);
            return *this;
        }
        IReclosers& Name(const string &value)
        {
            return Name(value.c_str());
        }

        ///
        /// Sets the next Recloser active. Returns 0 if no more.
        ///
        int32_t Next() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reclosers_Get_Next(ctx);
        }

        ///
        /// Get active Recloser by index; index is 1-based: 1..count
        ///
        int32_t idx() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reclosers_Get_idx(ctx);
        }

        ///
        /// Get active Recloser by index; index is 1-based: 1..count
        ///
        void idx(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reclosers_Set_idx(ctx, value);
        }
        void Close()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reclosers_Close(ctx);
        }
        void Open()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reclosers_Open(ctx);
        }

        /// 
        /// Ground (3I0) instantaneous trip setting - curve multiplier or actual amps.
        /// 
        /// Original COM help: https://opendss.epri.com/GroundInst.html
        /// 
        double GroundInst() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reclosers_Get_GroundInst(ctx);
        }
        IReclosers& GroundInst(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reclosers_Set_GroundInst(ctx, value);
            return *this;
        }

        /// 
        /// Ground (3I0) trip multiplier or actual amps
        /// 
        /// Original COM help: https://opendss.epri.com/GroundTrip.html
        /// 
        double GroundTrip() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reclosers_Get_GroundTrip(ctx);
        }
        IReclosers& GroundTrip(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reclosers_Set_GroundTrip(ctx, value);
            return *this;
        }

        /// 
        /// Full name of object this Recloser to be monitored.
        /// 
        /// Original COM help: https://opendss.epri.com/MonitoredObj2.html
        /// 
        string MonitoredObj() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reclosers_Get_MonitoredObj(ctx);
        }
        IReclosers& MonitoredObj(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reclosers_Set_MonitoredObj(ctx, value);
            return *this;
        }
        IReclosers& MonitoredObj(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reclosers_Set_MonitoredObj(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Terminal number of Monitored object for the Recloser 
        /// 
        /// Original COM help: https://opendss.epri.com/MonitoredTerm2.html
        /// 
        int32_t MonitoredTerm() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reclosers_Get_MonitoredTerm(ctx);
        }
        IReclosers& MonitoredTerm(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reclosers_Set_MonitoredTerm(ctx, value);
            return *this;
        }

        /// 
        /// Number of fast shots
        /// 
        /// Original COM help: https://opendss.epri.com/NumFast.html
        /// 
        int32_t NumFast() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reclosers_Get_NumFast(ctx);
        }
        IReclosers& NumFast(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reclosers_Set_NumFast(ctx, value);
            return *this;
        }

        /// 
        /// Phase instantaneous curve multiplier or actual amps
        /// 
        /// Original COM help: https://opendss.epri.com/PhaseInst.html
        /// 
        double PhaseInst() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reclosers_Get_PhaseInst(ctx);
        }
        IReclosers& PhaseInst(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reclosers_Set_PhaseInst(ctx, value);
            return *this;
        }

        /// 
        /// Phase trip curve multiplier or actual amps
        /// 
        /// Original COM help: https://opendss.epri.com/PhaseTrip.html
        /// 
        double PhaseTrip() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reclosers_Get_PhaseTrip(ctx);
        }
        IReclosers& PhaseTrip(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reclosers_Set_PhaseTrip(ctx, value);
            return *this;
        }

        /// 
        /// Array of Doubles: reclose intervals, s, between shots.
        /// 
        /// Original COM help: https://opendss.epri.com/RecloseIntervals.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT RecloseIntervals() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reclosers_Get_RecloseIntervals_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Number of shots to lockout (fast + delayed)
        /// 
        /// Original COM help: https://opendss.epri.com/Shots.html
        /// 
        int32_t Shots() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reclosers_Get_Shots(ctx);
        }
        IReclosers& Shots(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reclosers_Set_Shots(ctx, value);
            return *this;
        }

        /// 
        /// Full name of the circuit element that is being switched by the Recloser.
        /// 
        /// Original COM help: https://opendss.epri.com/SwitchedObj1.html
        /// 
        string SwitchedObj() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reclosers_Get_SwitchedObj(ctx);
        }
        IReclosers& SwitchedObj(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reclosers_Set_SwitchedObj(ctx, value);
            return *this;
        }
        IReclosers& SwitchedObj(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reclosers_Set_SwitchedObj(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Terminal number of the controlled device being switched by the Recloser
        /// 
        /// Original COM help: https://opendss.epri.com/SwitchedTerm1.html
        /// 
        int32_t SwitchedTerm() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reclosers_Get_SwitchedTerm(ctx);
        }
        IReclosers& SwitchedTerm(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reclosers_Set_SwitchedTerm(ctx, value);
            return *this;
        }
        /// 
        /// Reset recloser to normal state. 
        /// If open, lock out the recloser. 
        /// If closed, resets recloser to first operation.
        /// 
        void Reset()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reclosers_Reset(ctx);
        }

        /// 
        /// Present state of recloser. 
        /// If set to open (ActionCodes.Open=1), open recloser's controlled element and lock out the recloser. 
        /// If set to close (ActionCodes.Close=2), close recloser's controlled element and resets recloser to first operation.
        /// 
        int32_t State() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reclosers_Get_State(ctx);
        }
        IReclosers& State(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reclosers_Set_State(ctx, value);
            return *this;
        }

        /// 
        /// Get/set normal state (ActionCodes.Open=1, ActionCodes.Close=2) of the recloser.
        /// 
        /// Original COM help: https://opendss.epri.com/NormalState1.html
        /// 
        int32_t NormalState() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reclosers_Get_NormalState(ctx);
        }
        IReclosers& NormalState(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Reclosers_Set_NormalState(ctx, value);
            return *this;
        }
    };

    class IRegControls: public ContextState
    {
    public:

        IRegControls(altdss::APIUtil *util) :
            ContextState(util)
        {
        }

        ///
        /// Array of strings with all RegControl names in the circuit.
        ///
        strings AllNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_RegControls_Get_AllNames);
        }

        ///
        /// Number of RegControl objects in active circuit.
        ///
        int32_t Count() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_RegControls_Get_Count(ctx);
        }

        ///
        /// Sets the first RegControl active. Returns 0 if no more.
        ///
        int32_t First() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_RegControls_Get_First(ctx);
        }

        ///
        /// Get the name of the current active RegControl
        ///
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_RegControls_Get_Name(ctx);
        }

        ///
        /// Sets the active RegControl by Name.
        ///
        IRegControls& Name(const char *value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_RegControls_Set_Name(ctx, value);
            return *this;
        }
        IRegControls& Name(const string &value)
        {
            return Name(value.c_str());
        }

        ///
        /// Sets the next RegControl active. Returns 0 if no more.
        ///
        int32_t Next() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_RegControls_Get_Next(ctx);
        }

        ///
        /// Get active RegControl by index; index is 1-based: 1..count
        ///
        int32_t idx() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_RegControls_Get_idx(ctx);
        }

        ///
        /// Get active RegControl by index; index is 1-based: 1..count
        ///
        void idx(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_RegControls_Set_idx(ctx, value);
        }
        void Reset()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_RegControls_Reset(ctx);
        }

        /// 
        /// CT primary ampere rating (secondary is 0.2 amperes)
        /// 
        /// Original COM help: https://opendss.epri.com/CTPrimary.html
        /// 
        double CTPrimary() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_RegControls_Get_CTPrimary(ctx);
        }
        IRegControls& CTPrimary(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_RegControls_Set_CTPrimary(ctx, value);
            return *this;
        }

        /// 
        /// Time delay [s] after arming before the first tap change. Control may reset before actually changing taps.
        /// 
        /// Original COM help: https://opendss.epri.com/Delay2.html
        /// 
        double Delay() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_RegControls_Get_Delay(ctx);
        }
        IRegControls& Delay(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_RegControls_Set_Delay(ctx, value);
            return *this;
        }

        /// 
        /// Regulation bandwidth in forward direction, centered on Vreg
        /// 
        /// Original COM help: https://opendss.epri.com/ForwardBand.html
        /// 
        double ForwardBand() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_RegControls_Get_ForwardBand(ctx);
        }
        IRegControls& ForwardBand(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_RegControls_Set_ForwardBand(ctx, value);
            return *this;
        }

        /// 
        /// LDC R setting in Volts
        /// 
        /// Original COM help: https://opendss.epri.com/ForwardR.html
        /// 
        double ForwardR() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_RegControls_Get_ForwardR(ctx);
        }
        IRegControls& ForwardR(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_RegControls_Set_ForwardR(ctx, value);
            return *this;
        }

        /// 
        /// Target voltage in the forward direction, on PT secondary base.
        /// 
        /// Original COM help: https://opendss.epri.com/ForwardVreg.html
        /// 
        double ForwardVreg() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_RegControls_Get_ForwardVreg(ctx);
        }
        IRegControls& ForwardVreg(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_RegControls_Set_ForwardVreg(ctx, value);
            return *this;
        }

        /// 
        /// LDC X setting in Volts
        /// 
        /// Original COM help: https://opendss.epri.com/ForwardX.html
        /// 
        double ForwardX() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_RegControls_Get_ForwardX(ctx);
        }
        IRegControls& ForwardX(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_RegControls_Set_ForwardX(ctx, value);
            return *this;
        }

        /// 
        /// Time delay is inversely adjusted, proportional to the amount of voltage outside the regulating band.
        /// 
        /// Original COM help: https://opendss.epri.com/IsInverseTime.html
        /// 
        bool IsInverseTime() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_RegControls_Get_IsInverseTime(ctx);
        }
        IRegControls& IsInverseTime(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_RegControls_Set_IsInverseTime(ctx, value);
            return *this;
        }

        /// 
        /// Regulator can use different settings in the reverse direction.  Usually not applicable to substation transformers.
        /// 
        /// Original COM help: https://opendss.epri.com/IsReversible.html
        /// 
        bool IsReversible() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_RegControls_Get_IsReversible(ctx);
        }
        IRegControls& IsReversible(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_RegControls_Set_IsReversible(ctx, value);
            return *this;
        }

        /// 
        /// Maximum tap change per iteration in STATIC solution mode. 1 is more realistic, 16 is the default for a faster solution.
        /// 
        /// Original COM help: https://opendss.epri.com/MaxTapChange.html
        /// 
        int32_t MaxTapChange() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_RegControls_Get_MaxTapChange(ctx);
        }
        IRegControls& MaxTapChange(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_RegControls_Set_MaxTapChange(ctx, value);
            return *this;
        }

        /// 
        /// Name of a remote regulated bus, in lieu of LDC settings
        /// 
        /// Original COM help: https://opendss.epri.com/MonitoredBus.html
        /// 
        string MonitoredBus() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_RegControls_Get_MonitoredBus(ctx);
        }
        IRegControls& MonitoredBus(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_RegControls_Set_MonitoredBus(ctx, value);
            return *this;
        }
        IRegControls& MonitoredBus(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_RegControls_Set_MonitoredBus(ctx, value.c_str());
            return *this;
        }

        /// 
        /// PT ratio for voltage control settings
        /// 
        /// Original COM help: https://opendss.epri.com/PTratio1.html
        /// 
        double PTratio() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_RegControls_Get_PTratio(ctx);
        }
        IRegControls& PTratio(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_RegControls_Set_PTratio(ctx, value);
            return *this;
        }

        /// 
        /// Bandwidth in reverse direction, centered on reverse Vreg.
        /// 
        /// Original COM help: https://opendss.epri.com/ReverseBand.html
        /// 
        double ReverseBand() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_RegControls_Get_ReverseBand(ctx);
        }
        IRegControls& ReverseBand(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_RegControls_Set_ReverseBand(ctx, value);
            return *this;
        }

        /// 
        /// Reverse LDC R setting in Volts.
        /// 
        /// Original COM help: https://opendss.epri.com/ReverseR.html
        /// 
        double ReverseR() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_RegControls_Get_ReverseR(ctx);
        }
        IRegControls& ReverseR(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_RegControls_Set_ReverseR(ctx, value);
            return *this;
        }

        /// 
        /// Target voltage in the revese direction, on PT secondary base.
        /// 
        /// Original COM help: https://opendss.epri.com/ReverseVreg.html
        /// 
        double ReverseVreg() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_RegControls_Get_ReverseVreg(ctx);
        }
        IRegControls& ReverseVreg(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_RegControls_Set_ReverseVreg(ctx, value);
            return *this;
        }

        /// 
        /// Reverse LDC X setting in volts.
        /// 
        /// Original COM help: https://opendss.epri.com/ReverseX.html
        /// 
        double ReverseX() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_RegControls_Get_ReverseX(ctx);
        }
        IRegControls& ReverseX(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_RegControls_Set_ReverseX(ctx, value);
            return *this;
        }

        /// 
        /// Time delay [s] for subsequent tap changes in a set. Control may reset before actually changing taps.
        /// 
        /// Original COM help: https://opendss.epri.com/TapDelay.html
        /// 
        double TapDelay() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_RegControls_Get_TapDelay(ctx);
        }
        IRegControls& TapDelay(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_RegControls_Set_TapDelay(ctx, value);
            return *this;
        }

        /// 
        /// Integer number of the tap that the controlled transformer winding is currently on.
        /// 
        /// Original COM help: https://opendss.epri.com/TapNumber.html
        /// 
        int32_t TapNumber() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_RegControls_Get_TapNumber(ctx);
        }
        IRegControls& TapNumber(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_RegControls_Set_TapNumber(ctx, value);
            return *this;
        }

        /// 
        /// Tapped winding number
        /// 
        /// Original COM help: https://opendss.epri.com/TapWinding.html
        /// 
        int32_t TapWinding() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_RegControls_Get_TapWinding(ctx);
        }
        IRegControls& TapWinding(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_RegControls_Set_TapWinding(ctx, value);
            return *this;
        }

        /// 
        /// Name of the transformer this regulator controls
        /// 
        /// Original COM help: https://opendss.epri.com/Transformer.html
        /// 
        string Transformer() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_RegControls_Get_Transformer(ctx);
        }
        IRegControls& Transformer(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_RegControls_Set_Transformer(ctx, value);
            return *this;
        }
        IRegControls& Transformer(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_RegControls_Set_Transformer(ctx, value.c_str());
            return *this;
        }

        /// 
        /// First house voltage limit on PT secondary base.  Setting to 0 disables this function.
        /// 
        /// Original COM help: https://opendss.epri.com/VoltageLimit.html
        /// 
        double VoltageLimit() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_RegControls_Get_VoltageLimit(ctx);
        }
        IRegControls& VoltageLimit(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_RegControls_Set_VoltageLimit(ctx, value);
            return *this;
        }

        /// 
        /// Winding number for PT and CT connections
        /// 
        /// Original COM help: https://opendss.epri.com/Winding.html
        /// 
        int32_t Winding() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_RegControls_Get_Winding(ctx);
        }
        IRegControls& Winding(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_RegControls_Set_Winding(ctx, value);
            return *this;
        }
    };

    class IRelays: public ContextState
    {
    public:

        IRelays(altdss::APIUtil *util) :
            ContextState(util)
        {
        }

        ///
        /// Array of strings with all Relay names in the circuit.
        ///
        strings AllNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Relays_Get_AllNames);
        }

        ///
        /// Number of Relay objects in active circuit.
        ///
        int32_t Count() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Relays_Get_Count(ctx);
        }

        ///
        /// Sets the first Relay active. Returns 0 if no more.
        ///
        int32_t First() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Relays_Get_First(ctx);
        }

        ///
        /// Get the name of the current active Relay
        ///
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Relays_Get_Name(ctx);
        }

        ///
        /// Sets the active Relay by Name.
        ///
        IRelays& Name(const char *value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Relays_Set_Name(ctx, value);
            return *this;
        }
        IRelays& Name(const string &value)
        {
            return Name(value.c_str());
        }

        ///
        /// Sets the next Relay active. Returns 0 if no more.
        ///
        int32_t Next() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Relays_Get_Next(ctx);
        }

        ///
        /// Get active Relay by index; index is 1-based: 1..count
        ///
        int32_t idx() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Relays_Get_idx(ctx);
        }

        ///
        /// Get active Relay by index; index is 1-based: 1..count
        ///
        void idx(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Relays_Set_idx(ctx, value);
        }

        /// 
        /// Full name of object this Relay is monitoring.
        /// 
        /// Original COM help: https://opendss.epri.com/MonitoredObj3.html
        /// 
        string MonitoredObj() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Relays_Get_MonitoredObj(ctx);
        }
        IRelays& MonitoredObj(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Relays_Set_MonitoredObj(ctx, value);
            return *this;
        }
        IRelays& MonitoredObj(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Relays_Set_MonitoredObj(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Number of terminal of monitored element that this Relay is monitoring.
        /// 
        /// Original COM help: https://opendss.epri.com/MonitoredTerm3.html
        /// 
        int32_t MonitoredTerm() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Relays_Get_MonitoredTerm(ctx);
        }
        IRelays& MonitoredTerm(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Relays_Set_MonitoredTerm(ctx, value);
            return *this;
        }

        /// 
        /// Full name of element that will be switched when relay trips.
        /// 
        /// Original COM help: https://opendss.epri.com/SwitchedObj2.html
        /// 
        string SwitchedObj() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Relays_Get_SwitchedObj(ctx);
        }
        IRelays& SwitchedObj(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Relays_Set_SwitchedObj(ctx, value);
            return *this;
        }
        IRelays& SwitchedObj(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Relays_Set_SwitchedObj(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Terminal number of the switched object that will be opened when the relay trips.
        /// 
        /// Original COM help: https://opendss.epri.com/SwitchedTerm2.html
        /// 
        int32_t SwitchedTerm() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Relays_Get_SwitchedTerm(ctx);
        }
        IRelays& SwitchedTerm(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Relays_Set_SwitchedTerm(ctx, value);
            return *this;
        }
        /// 
        /// Open relay's controlled element and lock out the relay.
        /// 
        /// Original COM help: https://opendss.epri.com/Open4.html
        /// 
        void Open()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Relays_Open(ctx);
        }
        /// 
        /// Close the switched object controlled by the relay. Resets relay to first operation.
        /// 
        /// Original COM help: https://opendss.epri.com/Close5.html
        /// 
        void Close()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Relays_Close(ctx);
        }
        /// 
        /// Reset relay to normal state. 
        /// If open, lock out the relay. 
        /// If closed, resets relay to first operation.
        /// 
        void Reset()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Relays_Reset(ctx);
        }

        /// 
        /// Present state of relay. 
        /// If set to open, open relay's controlled element and lock out the relay. 
        /// If set to close, close relay's controlled element and resets relay to first operation.
        /// 
        int32_t State() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Relays_Get_State(ctx);
        }
        IRelays& State(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Relays_Set_State(ctx, value);
            return *this;
        }

        /// 
        /// Normal state of relay.
        /// 
        /// Original COM help: https://opendss.epri.com/NormalState3.html
        /// 
        int32_t NormalState() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Relays_Get_NormalState(ctx);
        }
        IRelays& NormalState(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Relays_Set_NormalState(ctx, value);
            return *this;
        }
    };

    class ISensors: public ContextState
    {
    public:

        ISensors(altdss::APIUtil *util) :
            ContextState(util)
        {
        }

        ///
        /// Array of strings with all Sensor names in the circuit.
        ///
        strings AllNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Sensors_Get_AllNames);
        }

        ///
        /// Number of Sensor objects in active circuit.
        ///
        int32_t Count() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Sensors_Get_Count(ctx);
        }

        ///
        /// Sets the first Sensor active. Returns 0 if no more.
        ///
        int32_t First() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Sensors_Get_First(ctx);
        }

        ///
        /// Get the name of the current active Sensor
        ///
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Sensors_Get_Name(ctx);
        }

        ///
        /// Sets the active Sensor by Name.
        ///
        ISensors& Name(const char *value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Sensors_Set_Name(ctx, value);
            return *this;
        }
        ISensors& Name(const string &value)
        {
            return Name(value.c_str());
        }

        ///
        /// Sets the next Sensor active. Returns 0 if no more.
        ///
        int32_t Next() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Sensors_Get_Next(ctx);
        }

        ///
        /// Get active Sensor by index; index is 1-based: 1..count
        ///
        int32_t idx() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Sensors_Get_idx(ctx);
        }

        ///
        /// Get active Sensor by index; index is 1-based: 1..count
        ///
        void idx(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Sensors_Set_idx(ctx, value);
        }
        void Reset()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Sensors_Reset(ctx);
        }
        void ResetAll()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Sensors_ResetAll(ctx);
        }

        /// 
        /// Array of doubles for the line current measurements; don't use with kWS and kVARS.
        /// 
        /// Original COM help: https://opendss.epri.com/Currents2.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Currents() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Sensors_Get_Currents_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        ISensors& Currents(const VectorT &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Sensors_Set_Currents(ctx, &value[0], value.size());
            return *this;
        }

        /// 
        /// True if measured voltages are line-line. Currents are always line currents.
        /// 
        /// Original COM help: https://opendss.epri.com/IsDelta2.html
        /// 
        bool IsDelta() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Sensors_Get_IsDelta(ctx);
        }
        ISensors& IsDelta(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Sensors_Set_IsDelta(ctx, value);
            return *this;
        }

        /// 
        /// Full Name of the measured element
        /// 
        /// Original COM help: https://opendss.epri.com/MeteredElement1.html
        /// 
        string MeteredElement() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Sensors_Get_MeteredElement(ctx);
        }
        ISensors& MeteredElement(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Sensors_Set_MeteredElement(ctx, value);
            return *this;
        }
        ISensors& MeteredElement(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Sensors_Set_MeteredElement(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Number of the measured terminal in the measured element.
        /// 
        /// Original COM help: https://opendss.epri.com/MeteredTerminal1.html
        /// 
        int32_t MeteredTerminal() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Sensors_Get_MeteredTerminal(ctx);
        }
        ISensors& MeteredTerminal(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Sensors_Set_MeteredTerminal(ctx, value);
            return *this;
        }

        /// 
        /// Assumed percent error in the Sensor measurement. Default is 1.
        /// 
        /// Original COM help: https://opendss.epri.com/PctError.html
        /// 
        double PctError() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Sensors_Get_PctError(ctx);
        }
        ISensors& PctError(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Sensors_Set_PctError(ctx, value);
            return *this;
        }

        /// 
        /// True if voltage measurements are 1-3, 3-2, 2-1.
        /// 
        /// Original COM help: https://opendss.epri.com/ReverseDelta.html
        /// 
        bool ReverseDelta() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Sensors_Get_ReverseDelta(ctx);
        }
        ISensors& ReverseDelta(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Sensors_Set_ReverseDelta(ctx, value);
            return *this;
        }

        /// 
        /// Weighting factor for this Sensor measurement with respect to other Sensors. Default is 1.
        /// 
        /// Original COM help: https://opendss.epri.com/Weight.html
        /// 
        double Weight() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Sensors_Get_Weight(ctx);
        }
        ISensors& Weight(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Sensors_Set_Weight(ctx, value);
            return *this;
        }

        /// 
        /// Array of doubles for Q measurements. Overwrites Currents with a new estimate using kWS.
        /// 
        /// Original COM help: https://opendss.epri.com/kVARS.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT kVARS() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Sensors_Get_kVARS_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        ISensors& kVARS(const VectorT &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Sensors_Set_kVARS(ctx, &value[0], value.size());
            return *this;
        }

        /// 
        /// Array of doubles for the LL or LN (depending on Delta connection) voltage measurements.
        /// 
        /// Original COM help: https://opendss.epri.com/kVS.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT kVS() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Sensors_Get_kVS_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        ISensors& kVS(const VectorT &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Sensors_Set_kVS(ctx, &value[0], value.size());
            return *this;
        }

        /// 
        /// Voltage base for the sensor measurements. LL for 2 and 3-phase sensors, LN for 1-phase sensors.
        /// 
        /// Original COM help: https://opendss.epri.com/kVBase1.html
        /// 
        double kVbase() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Sensors_Get_kVbase(ctx);
        }
        ISensors& kVbase(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Sensors_Set_kVbase(ctx, value);
            return *this;
        }

        /// 
        /// Array of doubles for P measurements. Overwrites Currents with a new estimate using kVARS.
        /// 
        /// Original COM help: https://opendss.epri.com/kWS.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT kWS() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Sensors_Get_kWS_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        ISensors& kWS(const VectorT &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Sensors_Set_kWS(ctx, &value[0], value.size());
            return *this;
        }

        /// 
        /// Array of doubles for the allocation factors for each phase.
        /// 
        /// Original COM help: https://opendss.epri.com/AllocationFactor1.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT AllocationFactor() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Sensors_Get_AllocationFactor_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }
    };

    class ISwtControls: public ContextState
    {
    public:

        ISwtControls(altdss::APIUtil *util) :
            ContextState(util)
        {
        }

        ///
        /// Array of strings with all SwtControl names in the circuit.
        ///
        strings AllNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_SwtControls_Get_AllNames);
        }

        ///
        /// Number of SwtControl objects in active circuit.
        ///
        int32_t Count() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_SwtControls_Get_Count(ctx);
        }

        ///
        /// Sets the first SwtControl active. Returns 0 if no more.
        ///
        int32_t First() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_SwtControls_Get_First(ctx);
        }

        ///
        /// Get the name of the current active SwtControl
        ///
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_SwtControls_Get_Name(ctx);
        }

        ///
        /// Sets the active SwtControl by Name.
        ///
        ISwtControls& Name(const char *value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_SwtControls_Set_Name(ctx, value);
            return *this;
        }
        ISwtControls& Name(const string &value)
        {
            return Name(value.c_str());
        }

        ///
        /// Sets the next SwtControl active. Returns 0 if no more.
        ///
        int32_t Next() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_SwtControls_Get_Next(ctx);
        }

        ///
        /// Get active SwtControl by index; index is 1-based: 1..count
        ///
        int32_t idx() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_SwtControls_Get_idx(ctx);
        }

        ///
        /// Get active SwtControl by index; index is 1-based: 1..count
        ///
        void idx(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_SwtControls_Set_idx(ctx, value);
        }
        void Reset()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_SwtControls_Reset(ctx);
        }

        /// 
        /// Open or Close the switch. No effect if switch is locked.  However, Reset removes any lock and then closes the switch (shelf state).
        /// 
        /// Original COM help: https://opendss.epri.com/Action1.html
        /// 
        int32_t Action() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_SwtControls_Get_Action(ctx);
        }
        ISwtControls& Action(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_SwtControls_Set_Action(ctx, value);
            return *this;
        }

        /// 
        /// Time delay [s] between arming and opening or closing the switch.  Control may reset before actually operating the switch.
        /// 
        /// Original COM help: https://opendss.epri.com/Delay3.html
        /// 
        double Delay() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_SwtControls_Get_Delay(ctx);
        }
        ISwtControls& Delay(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_SwtControls_Set_Delay(ctx, value);
            return *this;
        }

        /// 
        /// The lock prevents both manual and automatic switch operation.
        /// 
        /// Original COM help: https://opendss.epri.com/IsLocked.html
        /// 
        bool IsLocked() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_SwtControls_Get_IsLocked(ctx);
        }
        ISwtControls& IsLocked(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_SwtControls_Set_IsLocked(ctx, value);
            return *this;
        }

        /// 
        /// Get/set Normal state of switch (see ActionCodes) dssActionOpen or dssActionClose
        /// 
        ActionCodes NormalState() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return static_cast<ActionCodes>(ctx_SwtControls_Get_NormalState(ctx));
        }
        ISwtControls& NormalState(ActionCodes value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_SwtControls_Set_NormalState(ctx, static_cast<int32_t>(value));
            return *this;
        }

        /// 
        /// Set it to force the switch to a specified state, otherwise read its present state.
        /// 
        /// Original COM help: https://opendss.epri.com/State.html
        /// 
        int32_t State() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_SwtControls_Get_State(ctx);
        }
        ISwtControls& State(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_SwtControls_Set_State(ctx, value);
            return *this;
        }

        /// 
        /// Full name of the switched element.
        /// 
        /// Original COM help: https://opendss.epri.com/SwitchedObj3.html
        /// 
        string SwitchedObj() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_SwtControls_Get_SwitchedObj(ctx);
        }
        ISwtControls& SwitchedObj(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_SwtControls_Set_SwitchedObj(ctx, value);
            return *this;
        }
        ISwtControls& SwitchedObj(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_SwtControls_Set_SwitchedObj(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Terminal number where the switch is located on the SwitchedObj
        /// 
        /// Original COM help: https://opendss.epri.com/SwitchedTerm3.html
        /// 
        int32_t SwitchedTerm() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_SwtControls_Get_SwitchedTerm(ctx);
        }
        ISwtControls& SwitchedTerm(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_SwtControls_Set_SwitchedTerm(ctx, value);
            return *this;
        }
    };

    class ITSData: public ContextState
    {
    public:

        ITSData(altdss::APIUtil *util) :
            ContextState(util)
        {
        }

        ///
        /// Array of strings with all TSData names in the circuit.
        ///
        strings AllNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_TSData_Get_AllNames);
        }

        ///
        /// Number of TSData objects in active circuit.
        ///
        int32_t Count() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_TSData_Get_Count(ctx);
        }

        ///
        /// Sets the first TSData active. Returns 0 if no more.
        ///
        int32_t First() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_TSData_Get_First(ctx);
        }

        ///
        /// Get the name of the current active TSData
        ///
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_TSData_Get_Name(ctx);
        }

        ///
        /// Sets the active TSData by Name.
        ///
        ITSData& Name(const char *value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_TSData_Set_Name(ctx, value);
            return *this;
        }
        ITSData& Name(const string &value)
        {
            return Name(value.c_str());
        }

        ///
        /// Sets the next TSData active. Returns 0 if no more.
        ///
        int32_t Next() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_TSData_Get_Next(ctx);
        }

        ///
        /// Get active TSData by index; index is 1-based: 1..count
        ///
        int32_t idx() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_TSData_Get_idx(ctx);
        }

        ///
        /// Get active TSData by index; index is 1-based: 1..count
        ///
        void idx(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_TSData_Set_idx(ctx, value);
        }

        /// 
        /// Emergency ampere rating
        /// 
        double EmergAmps() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_TSData_Get_EmergAmps(ctx);
        }
        ITSData& EmergAmps(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_TSData_Set_EmergAmps(ctx, value);
            return *this;
        }

        /// 
        /// Normal Ampere rating
        /// 
        double NormAmps() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_TSData_Get_NormAmps(ctx);
        }
        ITSData& NormAmps(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_TSData_Set_NormAmps(ctx, value);
            return *this;
        }

        double Rdc() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_TSData_Get_Rdc(ctx);
        }
        ITSData& Rdc(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_TSData_Set_Rdc(ctx, value);
            return *this;
        }

        double Rac() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_TSData_Get_Rac(ctx);
        }
        ITSData& Rac(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_TSData_Set_Rac(ctx, value);
            return *this;
        }

        double GMRac() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_TSData_Get_GMRac(ctx);
        }
        ITSData& GMRac(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_TSData_Set_GMRac(ctx, value);
            return *this;
        }

        int32_t GMRUnits() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_TSData_Get_GMRUnits(ctx);
        }
        ITSData& GMRUnits(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_TSData_Set_GMRUnits(ctx, value);
            return *this;
        }

        double Radius() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_TSData_Get_Radius(ctx);
        }
        ITSData& Radius(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_TSData_Set_Radius(ctx, value);
            return *this;
        }

        int32_t RadiusUnits() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_TSData_Get_RadiusUnits(ctx);
        }
        ITSData& RadiusUnits(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_TSData_Set_RadiusUnits(ctx, value);
            return *this;
        }

        int32_t ResistanceUnits() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_TSData_Get_ResistanceUnits(ctx);
        }
        ITSData& ResistanceUnits(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_TSData_Set_ResistanceUnits(ctx, value);
            return *this;
        }

        double Diameter() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_TSData_Get_Diameter(ctx);
        }
        ITSData& Diameter(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_TSData_Set_Diameter(ctx, value);
            return *this;
        }

        double EpsR() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_TSData_Get_EpsR(ctx);
        }
        ITSData& EpsR(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_TSData_Set_EpsR(ctx, value);
            return *this;
        }

        double InsLayer() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_TSData_Get_InsLayer(ctx);
        }
        ITSData& InsLayer(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_TSData_Set_InsLayer(ctx, value);
            return *this;
        }

        double DiaIns() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_TSData_Get_DiaIns(ctx);
        }
        ITSData& DiaIns(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_TSData_Set_DiaIns(ctx, value);
            return *this;
        }

        double DiaCable() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_TSData_Get_DiaCable(ctx);
        }
        ITSData& DiaCable(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_TSData_Set_DiaCable(ctx, value);
            return *this;
        }

        double DiaShield() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_TSData_Get_DiaShield(ctx);
        }
        ITSData& DiaShield(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_TSData_Set_DiaShield(ctx, value);
            return *this;
        }

        double TapeLayer() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_TSData_Get_TapeLayer(ctx);
        }
        ITSData& TapeLayer(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_TSData_Set_TapeLayer(ctx, value);
            return *this;
        }

        double TapeLap() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_TSData_Get_TapeLap(ctx);
        }
        ITSData& TapeLap(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_TSData_Set_TapeLap(ctx, value);
            return *this;
        }
    };

    class IText: public ContextState
    {
    public:

        IText(altdss::APIUtil *util) :
            ContextState(util)
        {
        }

        /// Runs a large string as command lines directly in the DSS engine.
        /// Intermediate results are ignored.
        /// 
        /// (API Extension)
        void Commands(const char *value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Text_CommandBlock(ctx, value);
        }
        void Commands(const string &value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Text_CommandBlock(ctx, value.c_str());
        }

        /// Runs a list of strings as commands directly in the DSS engine.
        /// Intermediate results are ignored.
        /// 
        /// (API Extension)
        void Commands(const strings &value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            api_util->set_string_array(ctx_Text_CommandArray, value);
        }

        /// 
        /// Input command string for the DSS.
        /// 
        /// Original COM help: https://opendss.epri.com/Command1.html
        /// 
        string Command() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Text_Get_Command(ctx);
        }
        IText& Command(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Text_Set_Command(ctx, value);
            return *this;
        }
        IText& Command(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Text_Set_Command(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Result string for the last command.
        /// 
        /// Original COM help: https://opendss.epri.com/Result.html
        /// 
        string Result() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Text_Get_Result(ctx);
        }
    };

    class ITopology: public ContextState
    {
    public:

        ITopology(altdss::APIUtil *util) :
            ContextState(util)
        {
        }

        /// 
        /// Returns index of the active branch
        /// 
        /// Original COM help: https://opendss.epri.com/ActiveBranch.html
        /// 
        int32_t ActiveBranch() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Topology_Get_ActiveBranch(ctx);
        }

        /// 
        /// Topological depth of the active branch
        /// 
        /// Original COM help: https://opendss.epri.com/ActiveLevel.html
        /// 
        int32_t ActiveLevel() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Topology_Get_ActiveLevel(ctx);
        }

        /// 
        /// Array of all isolated branch names.
        /// 
        /// Original COM help: https://opendss.epri.com/AllIsolatedBranches.html
        /// 
        strings AllIsolatedBranches() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Topology_Get_AllIsolatedBranches);
        }

        /// 
        /// Array of all isolated load names.
        /// 
        /// Original COM help: https://opendss.epri.com/AllIsolatedLoads.html
        /// 
        strings AllIsolatedLoads() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Topology_Get_AllIsolatedLoads);
        }

        /// 
        /// Array of all looped element names, by pairs.
        /// 
        /// Original COM help: https://opendss.epri.com/AllLoopedPairs.html
        /// 
        strings AllLoopedPairs() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Topology_Get_AllLoopedPairs);
        }

        /// 
        /// Move back toward the source, return index of new active branch, or 0 if no more.
        /// 
        /// Original COM help: https://opendss.epri.com/BackwardBranch.html
        /// 
        int32_t BackwardBranch() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Topology_Get_BackwardBranch(ctx);
        }

        /// 
        /// Name of the active branch.
        /// 
        /// Original COM help: https://opendss.epri.com/BranchName.html
        /// 
        string BranchName() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Topology_Get_BranchName(ctx);
        }
        ITopology& BranchName(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Topology_Set_BranchName(ctx, value);
            return *this;
        }
        ITopology& BranchName(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Topology_Set_BranchName(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Set the active branch to one containing this bus, return index or 0 if not found
        /// 
        /// Original COM help: https://opendss.epri.com/BusName.html
        /// 
        string BusName() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Topology_Get_BusName(ctx);
        }
        ITopology& BusName(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Topology_Set_BusName(ctx, value);
            return *this;
        }
        ITopology& BusName(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Topology_Set_BusName(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Sets the first branch active, returns 0 if none.
        /// 
        /// Original COM help: https://opendss.epri.com/First19.html
        /// 
        int32_t First() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Topology_Get_First(ctx);
        }

        /// 
        /// First load at the active branch, return index or 0 if none.
        /// 
        /// Original COM help: https://opendss.epri.com/FirstLoad.html
        /// 
        int32_t FirstLoad() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Topology_Get_FirstLoad(ctx);
        }

        /// 
        /// Move forward in the tree, return index of new active branch or 0 if no more
        /// 
        /// Original COM help: https://opendss.epri.com/ForwardBranch.html
        /// 
        int32_t ForwardBranch() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Topology_Get_ForwardBranch(ctx);
        }

        /// 
        /// Move to looped branch, return index or 0 if none.
        /// 
        /// Original COM help: https://opendss.epri.com/LoopedBranch.html
        /// 
        int32_t LoopedBranch() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Topology_Get_LoopedBranch(ctx);
        }

        /// 
        /// Sets the next branch active, returns 0 if no more.
        /// 
        /// Original COM help: https://opendss.epri.com/Next18.html
        /// 
        int32_t Next() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Topology_Get_Next(ctx);
        }

        /// 
        /// Next load at the active branch, return index or 0 if no more.
        /// 
        /// Original COM help: https://opendss.epri.com/NextLoad.html
        /// 
        int32_t NextLoad() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Topology_Get_NextLoad(ctx);
        }

        /// 
        /// Number of isolated branches (PD elements and capacitors).
        /// 
        /// Original COM help: https://opendss.epri.com/NumIsolatedBranches.html
        /// 
        int32_t NumIsolatedBranches() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Topology_Get_NumIsolatedBranches(ctx);
        }

        /// 
        /// Number of isolated loads
        /// 
        /// Original COM help: https://opendss.epri.com/NumIsolatedLoads.html
        /// 
        int32_t NumIsolatedLoads() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Topology_Get_NumIsolatedLoads(ctx);
        }

        /// 
        /// Number of loops
        /// 
        /// Original COM help: https://opendss.epri.com/NumLoops.html
        /// 
        int32_t NumLoops() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Topology_Get_NumLoops(ctx);
        }

        /// 
        /// Move to directly parallel branch, return index or 0 if none.
        /// 
        /// Original COM help: https://opendss.epri.com/ParallelBranch.html
        /// 
        int32_t ParallelBranch() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Topology_Get_ParallelBranch(ctx);
        }
    };

    class ITransformers: public ContextState
    {
    public:

        ITransformers(altdss::APIUtil *util) :
            ContextState(util)
        {
        }

        ///
        /// Array of strings with all Transformer names in the circuit.
        ///
        strings AllNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Transformers_Get_AllNames);
        }

        ///
        /// Number of Transformer objects in active circuit.
        ///
        int32_t Count() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Transformers_Get_Count(ctx);
        }

        ///
        /// Sets the first Transformer active. Returns 0 if no more.
        ///
        int32_t First() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Transformers_Get_First(ctx);
        }

        ///
        /// Get the name of the current active Transformer
        ///
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Transformers_Get_Name(ctx);
        }

        ///
        /// Sets the active Transformer by Name.
        ///
        ITransformers& Name(const char *value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Transformers_Set_Name(ctx, value);
            return *this;
        }
        ITransformers& Name(const string &value)
        {
            return Name(value.c_str());
        }

        ///
        /// Sets the next Transformer active. Returns 0 if no more.
        ///
        int32_t Next() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Transformers_Get_Next(ctx);
        }

        ///
        /// Get active Transformer by index; index is 1-based: 1..count
        ///
        int32_t idx() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Transformers_Get_idx(ctx);
        }

        ///
        /// Get active Transformer by index; index is 1-based: 1..count
        ///
        void idx(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Transformers_Set_idx(ctx, value);
        }

        /// 
        /// Active Winding delta or wye connection?
        /// 
        /// Original COM help: https://opendss.epri.com/IsDelta3.html
        /// 
        bool IsDelta() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Transformers_Get_IsDelta(ctx);
        }
        ITransformers& IsDelta(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Transformers_Set_IsDelta(ctx, value);
            return *this;
        }

        /// 
        /// Active Winding maximum tap in per-unit.
        /// 
        /// Original COM help: https://opendss.epri.com/MaxTap.html
        /// 
        double MaxTap() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Transformers_Get_MaxTap(ctx);
        }
        ITransformers& MaxTap(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Transformers_Set_MaxTap(ctx, value);
            return *this;
        }

        /// 
        /// Active Winding minimum tap in per-unit.
        /// 
        /// Original COM help: https://opendss.epri.com/MinTap.html
        /// 
        double MinTap() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Transformers_Get_MinTap(ctx);
        }
        ITransformers& MinTap(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Transformers_Set_MinTap(ctx, value);
            return *this;
        }

        /// 
        /// Active Winding number of tap steps between MinTap and MaxTap.
        /// 
        /// Original COM help: https://opendss.epri.com/NumTaps.html
        /// 
        int32_t NumTaps() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Transformers_Get_NumTaps(ctx);
        }
        ITransformers& NumTaps(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Transformers_Set_NumTaps(ctx, value);
            return *this;
        }

        /// 
        /// Number of windings on this transformer. Allocates memory; set or change this property first.
        /// 
        /// Original COM help: https://opendss.epri.com/NumWindings.html
        /// 
        int32_t NumWindings() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Transformers_Get_NumWindings(ctx);
        }
        ITransformers& NumWindings(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Transformers_Set_NumWindings(ctx, value);
            return *this;
        }

        /// 
        /// Active Winding resistance in %
        /// 
        /// Original COM help: https://opendss.epri.com/R.html
        /// 
        double R() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Transformers_Get_R(ctx);
        }
        ITransformers& R(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Transformers_Set_R(ctx, value);
            return *this;
        }

        /// 
        /// Active Winding neutral resistance [ohms] for wye connections. Set less than zero for ungrounded wye.
        /// 
        /// Original COM help: https://opendss.epri.com/Rneut1.html
        /// 
        double Rneut() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Transformers_Get_Rneut(ctx);
        }
        ITransformers& Rneut(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Transformers_Set_Rneut(ctx, value);
            return *this;
        }

        /// 
        /// Active Winding tap in per-unit.
        /// 
        /// Original COM help: https://opendss.epri.com/Tap.html
        /// 
        double Tap() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Transformers_Get_Tap(ctx);
        }
        ITransformers& Tap(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Transformers_Set_Tap(ctx, value);
            return *this;
        }

        /// 
        /// Active Winding Number from 1..NumWindings. Update this before reading or setting a sequence of winding properties (R, Tap, kV, kVA, etc.)
        /// 
        /// Original COM help: https://opendss.epri.com/Wdg.html
        /// 
        int32_t Wdg() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Transformers_Get_Wdg(ctx);
        }
        ITransformers& Wdg(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Transformers_Set_Wdg(ctx, value);
            return *this;
        }

        /// 
        /// Name of an XfrmCode that supplies electrical parameters for this Transformer.
        /// 
        /// Original COM help: https://opendss.epri.com/XfmrCode1.html
        /// 
        string XfmrCode() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Transformers_Get_XfmrCode(ctx);
        }
        ITransformers& XfmrCode(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Transformers_Set_XfmrCode(ctx, value);
            return *this;
        }
        ITransformers& XfmrCode(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Transformers_Set_XfmrCode(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Percent reactance between windings 1 and 2, on winding 1 kVA base. Use for 2-winding or 3-winding transformers.
        /// 
        /// Original COM help: https://opendss.epri.com/Xhl.html
        /// 
        double Xhl() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Transformers_Get_Xhl(ctx);
        }
        ITransformers& Xhl(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Transformers_Set_Xhl(ctx, value);
            return *this;
        }

        /// 
        /// Percent reactance between windings 1 and 3, on winding 1 kVA base.  Use for 3-winding transformers only.
        /// 
        /// Original COM help: https://opendss.epri.com/Xht.html
        /// 
        double Xht() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Transformers_Get_Xht(ctx);
        }
        ITransformers& Xht(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Transformers_Set_Xht(ctx, value);
            return *this;
        }

        /// 
        /// Percent reactance between windings 2 and 3, on winding 1 kVA base. Use for 3-winding transformers only.
        /// 
        /// Original COM help: https://opendss.epri.com/Xlt.html
        /// 
        double Xlt() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Transformers_Get_Xlt(ctx);
        }
        ITransformers& Xlt(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Transformers_Set_Xlt(ctx, value);
            return *this;
        }

        /// 
        /// Active Winding neutral reactance [ohms] for wye connections.
        /// 
        /// Original COM help: https://opendss.epri.com/Xneut1.html
        /// 
        double Xneut() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Transformers_Get_Xneut(ctx);
        }
        ITransformers& Xneut(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Transformers_Set_Xneut(ctx, value);
            return *this;
        }

        /// 
        /// Active Winding kV rating.  Phase-phase for 2 or 3 phases, actual winding kV for 1 phase transformer.
        /// 
        /// Original COM help: https://opendss.epri.com/kV3.html
        /// 
        double kV() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Transformers_Get_kV(ctx);
        }
        ITransformers& kV(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Transformers_Set_kV(ctx, value);
            return *this;
        }

        /// 
        /// Active Winding kVA rating. On winding 1, this also determines normal and emergency current ratings for all windings.
        /// 
        /// Original COM help: https://opendss.epri.com/kva1.html
        /// 
        double kVA() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Transformers_Get_kVA(ctx);
        }
        ITransformers& kVA(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Transformers_Set_kVA(ctx, value);
            return *this;
        }

        /// 
        /// Complex array of voltages for active winding
        /// 
        /// **WARNING:** If the transformer has open terminal(s), results may be wrong, i.e. avoid using this
        /// in those situations. For more information, see https://github.com/dss-extensions/dss-extensions/issues/24
        /// 
        /// Original COM help: https://opendss.epri.com/WdgVoltages.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT WdgVoltages() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Transformers_Get_WdgVoltages_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// All Winding currents (ph1, wdg1, wdg2,... ph2, wdg1, wdg2 ...)
        /// 
        /// **WARNING:** If the transformer has open terminal(s), results may be wrong, i.e. avoid using this
        /// in those situations. For more information, see https://github.com/dss-extensions/dss-extensions/issues/24
        /// 
        /// Original COM help: https://opendss.epri.com/WdgCurrents.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT WdgCurrents() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Transformers_Get_WdgCurrents_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// All winding currents in CSV string form like the WdgCurrents property
        /// 
        /// **WARNING:** If the transformer has open terminal(s), results may be wrong, i.e. avoid using this
        /// in those situations. For more information, see https://github.com/dss-extensions/dss-extensions/issues/24
        /// 
        string strWdgCurrents() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Transformers_Get_strWdgCurrents(ctx);
        }

        /// 
        /// Transformer Core Type: 0=Shell; 1=1ph; 3-3leg; 4=4-Leg; 5=5-leg; 9=Core-1-phase
        /// 
        /// Original COM help: https://opendss.epri.com/CoreType.html
        /// 
        TransformerCoreType CoreType() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return TransformerCoreType(ctx_Transformers_Get_CoreType(ctx));
        }
        ITransformers& CoreType(TransformerCoreType value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Transformers_Set_CoreType(ctx, value);
            return *this;
        }

        /// 
        /// dc Resistance of active winding in ohms for GIC analysis
        /// 
        /// Original COM help: https://opendss.epri.com/RdcOhms.html
        /// 
        double RdcOhms() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Transformers_Get_RdcOhms(ctx);
        }
        ITransformers& RdcOhms(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Transformers_Set_RdcOhms(ctx, value);
            return *this;
        }

        /// 
        /// Complex array with the losses by type (total losses, load losses, no-load losses), in VA, for the current active transformer
        /// 
        /// **(API Extension)**
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT LossesByType() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Transformers_Get_LossesByType_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Complex array with the losses by type (total losses, load losses, no-load losses), in VA, concatenated for ALL transformers
        /// 
        /// **(API Extension)**
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT AllLossesByType() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Transformers_Get_AllLossesByType_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }
    };

    class IVsources: public ContextState
    {
    public:

        IVsources(altdss::APIUtil *util) :
            ContextState(util)
        {
        }

        ///
        /// Array of strings with all Vsource names in the circuit.
        ///
        strings AllNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Vsources_Get_AllNames);
        }

        ///
        /// Number of Vsource objects in active circuit.
        ///
        int32_t Count() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Vsources_Get_Count(ctx);
        }

        ///
        /// Sets the first Vsource active. Returns 0 if no more.
        ///
        int32_t First() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Vsources_Get_First(ctx);
        }

        ///
        /// Get the name of the current active Vsource
        ///
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Vsources_Get_Name(ctx);
        }

        ///
        /// Sets the active Vsource by Name.
        ///
        IVsources& Name(const char *value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Vsources_Set_Name(ctx, value);
            return *this;
        }
        IVsources& Name(const string &value)
        {
            return Name(value.c_str());
        }

        ///
        /// Sets the next Vsource active. Returns 0 if no more.
        ///
        int32_t Next() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Vsources_Get_Next(ctx);
        }

        ///
        /// Get active Vsource by index; index is 1-based: 1..count
        ///
        int32_t idx() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Vsources_Get_idx(ctx);
        }

        ///
        /// Get active Vsource by index; index is 1-based: 1..count
        ///
        void idx(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Vsources_Set_idx(ctx, value);
        }

        /// 
        /// Phase angle of first phase in degrees
        /// 
        /// Original COM help: https://opendss.epri.com/AngleDeg1.html
        /// 
        double AngleDeg() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Vsources_Get_AngleDeg(ctx);
        }
        IVsources& AngleDeg(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Vsources_Set_AngleDeg(ctx, value);
            return *this;
        }

        /// 
        /// Source voltage in kV
        /// 
        /// Original COM help: https://opendss.epri.com/BasekV.html
        /// 
        double BasekV() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Vsources_Get_BasekV(ctx);
        }
        IVsources& BasekV(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Vsources_Set_BasekV(ctx, value);
            return *this;
        }

        /// 
        /// Source frequency in Hz
        /// 
        /// Original COM help: https://opendss.epri.com/Frequency2.html
        /// 
        double Frequency() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Vsources_Get_Frequency(ctx);
        }
        IVsources& Frequency(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Vsources_Set_Frequency(ctx, value);
            return *this;
        }

        /// 
        /// Number of phases
        /// 
        /// Original COM help: https://opendss.epri.com/Phases3.html
        /// 
        int32_t Phases() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Vsources_Get_Phases(ctx);
        }
        IVsources& Phases(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Vsources_Set_Phases(ctx, value);
            return *this;
        }

        /// 
        /// Per-unit value of source voltage
        /// 
        /// Original COM help: https://opendss.epri.com/pu.html
        /// 
        double pu() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Vsources_Get_pu(ctx);
        }
        IVsources& pu(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Vsources_Set_pu(ctx, value);
            return *this;
        }
    };

    class IWireData: public ContextState
    {
    public:

        IWireData(altdss::APIUtil *util) :
            ContextState(util)
        {
        }

        ///
        /// Array of strings with all WireData names in the circuit.
        ///
        strings AllNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_WireData_Get_AllNames);
        }

        ///
        /// Number of WireData objects in active circuit.
        ///
        int32_t Count() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WireData_Get_Count(ctx);
        }

        ///
        /// Sets the first WireData active. Returns 0 if no more.
        ///
        int32_t First() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WireData_Get_First(ctx);
        }

        ///
        /// Get the name of the current active WireData
        ///
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WireData_Get_Name(ctx);
        }

        ///
        /// Sets the active WireData by Name.
        ///
        IWireData& Name(const char *value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WireData_Set_Name(ctx, value);
            return *this;
        }
        IWireData& Name(const string &value)
        {
            return Name(value.c_str());
        }

        ///
        /// Sets the next WireData active. Returns 0 if no more.
        ///
        int32_t Next() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WireData_Get_Next(ctx);
        }

        ///
        /// Get active WireData by index; index is 1-based: 1..count
        ///
        int32_t idx() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WireData_Get_idx(ctx);
        }

        ///
        /// Get active WireData by index; index is 1-based: 1..count
        ///
        void idx(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WireData_Set_idx(ctx, value);
        }

        /// 
        /// Emergency ampere rating
        /// 
        double EmergAmps() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WireData_Get_EmergAmps(ctx);
        }
        IWireData& EmergAmps(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WireData_Set_EmergAmps(ctx, value);
            return *this;
        }

        /// 
        /// Normal Ampere rating
        /// 
        double NormAmps() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WireData_Get_NormAmps(ctx);
        }
        IWireData& NormAmps(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WireData_Set_NormAmps(ctx, value);
            return *this;
        }

        double Rdc() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WireData_Get_Rdc(ctx);
        }
        IWireData& Rdc(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WireData_Set_Rdc(ctx, value);
            return *this;
        }

        double Rac() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WireData_Get_Rac(ctx);
        }
        IWireData& Rac(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WireData_Set_Rac(ctx, value);
            return *this;
        }

        double GMRac() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WireData_Get_GMRac(ctx);
        }
        IWireData& GMRac(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WireData_Set_GMRac(ctx, value);
            return *this;
        }

        LineUnits GMRUnits() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return static_cast<LineUnits>(ctx_WireData_Get_GMRUnits(ctx));
        }
        IWireData& GMRUnits(LineUnits value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WireData_Set_GMRUnits(ctx, static_cast<int32_t>(value));
            return *this;
        }

        double Radius() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WireData_Get_Radius(ctx);
        }
        IWireData& Radius(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WireData_Set_Radius(ctx, value);
            return *this;
        }

        int32_t RadiusUnits() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WireData_Get_RadiusUnits(ctx);
        }
        IWireData& RadiusUnits(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WireData_Set_RadiusUnits(ctx, value);
            return *this;
        }

        LineUnits ResistanceUnits() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return static_cast<LineUnits>(ctx_WireData_Get_ResistanceUnits(ctx));
        }
        IWireData& ResistanceUnits(LineUnits value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WireData_Set_ResistanceUnits(ctx, static_cast<int32_t>(value));
            return *this;
        }

        double Diameter() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WireData_Get_Diameter(ctx);
        }
        IWireData& Diameter(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WireData_Set_Diameter(ctx, value);
            return *this;
        }

        /// 
        /// Equivalent conductor radius for capacitance calcs. Specify this for bundled conductors. Defaults to same value as radius.
        /// 
        double CapRadius() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WireData_Get_CapRadius(ctx);
        }
        IWireData& CapRadius(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WireData_Set_CapRadius(ctx, value);
            return *this;
        }
    };

    class IXYCurves: public ContextState
    {
    public:

        IXYCurves(altdss::APIUtil *util) :
            ContextState(util)
        {
        }

        ///
        /// Array of strings with all XYCurve names in the circuit.
        ///
        strings AllNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_XYCurves_Get_AllNames);
        }

        ///
        /// Number of XYCurve objects in active circuit.
        ///
        int32_t Count() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_XYCurves_Get_Count(ctx);
        }

        ///
        /// Sets the first XYCurve active. Returns 0 if no more.
        ///
        int32_t First() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_XYCurves_Get_First(ctx);
        }

        ///
        /// Get the name of the current active XYCurve
        ///
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_XYCurves_Get_Name(ctx);
        }

        ///
        /// Sets the active XYCurve by Name.
        ///
        IXYCurves& Name(const char *value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_XYCurves_Set_Name(ctx, value);
            return *this;
        }
        IXYCurves& Name(const string &value)
        {
            return Name(value.c_str());
        }

        ///
        /// Sets the next XYCurve active. Returns 0 if no more.
        ///
        int32_t Next() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_XYCurves_Get_Next(ctx);
        }

        ///
        /// Get active XYCurve by index; index is 1-based: 1..count
        ///
        int32_t idx() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_XYCurves_Get_idx(ctx);
        }

        ///
        /// Get active XYCurve by index; index is 1-based: 1..count
        ///
        void idx(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_XYCurves_Set_idx(ctx, value);
        }

        /// 
        /// Get/Set Number of points in X-Y curve
        /// 
        /// Original COM help: https://opendss.epri.com/Npts1.html
        /// 
        int32_t Npts() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_XYCurves_Get_Npts(ctx);
        }
        IXYCurves& Npts(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_XYCurves_Set_Npts(ctx, value);
            return *this;
        }

        /// 
        /// Get/set X values as an array of doubles. When setting, remember to set Npts to max number expected values.
        /// 
        /// Original COM help: https://opendss.epri.com/Xarray.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Xarray() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_XYCurves_Get_Xarray_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        IXYCurves& Xarray(const VectorT &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_XYCurves_Set_Xarray(ctx, &value[0], value.size());
            return *this;
        }

        /// 
        /// Factor to scale X values from original curve
        /// 
        /// Original COM help: https://opendss.epri.com/Xscale.html
        /// 
        double Xscale() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_XYCurves_Get_Xscale(ctx);
        }
        IXYCurves& Xscale(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_XYCurves_Set_Xscale(ctx, value);
            return *this;
        }

        /// 
        /// Amount to shift X value from original curve
        /// 
        /// Original COM help: https://opendss.epri.com/Xshift.html
        /// 
        double Xshift() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_XYCurves_Get_Xshift(ctx);
        }
        IXYCurves& Xshift(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_XYCurves_Set_Xshift(ctx, value);
            return *this;
        }

        /// 
        /// Get/set Y values as an array of doubles. When setting, remember to set Npts to max number expected values.
        /// 
        /// Original COM help: https://opendss.epri.com/Yarray.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Yarray() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_XYCurves_Get_Yarray_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        IXYCurves& Yarray(const VectorT &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_XYCurves_Set_Yarray(ctx, &value[0], value.size());
            return *this;
        }

        /// 
        /// Factor to scale Y values from original curve
        /// 
        /// Original COM help: https://opendss.epri.com/Yscale.html
        /// 
        double Yscale() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_XYCurves_Get_Yscale(ctx);
        }
        IXYCurves& Yscale(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_XYCurves_Set_Yscale(ctx, value);
            return *this;
        }

        /// 
        /// Amount to shift Y value from original curve
        /// 
        /// Original COM help: https://opendss.epri.com/Yshift.html
        /// 
        double Yshift() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_XYCurves_Get_Yshift(ctx);
        }
        IXYCurves& Yshift(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_XYCurves_Set_Yshift(ctx, value);
            return *this;
        }

        /// 
        /// Set X value or get interpolated value after setting Y
        /// 
        /// Original COM help: https://opendss.epri.com/x4.html
        /// 
        double x() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_XYCurves_Get_x(ctx);
        }
        IXYCurves& x(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_XYCurves_Set_x(ctx, value);
            return *this;
        }

        /// 
        /// Set Y value or get interpolated Y value after setting X
        /// 
        /// Original COM help: https://opendss.epri.com/y1.html
        /// 
        double y() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_XYCurves_Get_y(ctx);
        }
        IXYCurves& y(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_XYCurves_Set_y(ctx, value);
            return *this;
        }
    };

    class IZIP: public ContextState
    {
    public:

        IZIP(altdss::APIUtil *util) :
            ContextState(util)
        {
        }

        /// List of strings consisting of all names match the regular expression provided in regexp.
        /// If no expression is provided, all names in the current open ZIP are returned.
        /// 
        /// See https://regex.sorokin.engineer/en/latest/regular_expressions.html for information on 
        /// the expression syntax and options.
        /// 
        /// (API Extension)
        strings List(const char *regexp="")
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_ZIP_List, regexp);
        }
        strings List(const string& regexp)
        {
            return List(regexp.c_str());
        }
        /// 
        /// Opens and prepares a ZIP file to be used by the DSS text parser.
        /// Currently, the ZIP format support is limited by what is provided in the Free Pascal distribution.
        /// Besides that, the full filenames inside the ZIP must be shorter than 256 characters.
        /// The limitations should be removed in a future revision.
        /// 
        /// **(API Extension)**
        /// 
        void Open(const char *FileName)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ZIP_Open(ctx, FileName);
        }
        /// 
        /// Opens and prepares a ZIP file to be used by the DSS text parser.
        /// Currently, the ZIP format support is limited by what is provided in the Free Pascal distribution.
        /// Besides that, the full filenames inside the ZIP must be shorter than 256 characters.
        /// The limitations should be removed in a future revision.
        /// 
        /// **(API Extension)**
        /// 
        void Open(const string &FileName)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ZIP_Open(ctx, FileName.c_str());
        }
        /// 
        /// Closes the current open ZIP file
        /// 
        /// **(API Extension)**
        /// 
        void Close()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ZIP_Close(ctx);
        }
        /// 
        /// Runs a "Redirect" command inside the current (open) ZIP file.
        /// In the current implementation, all files required by the script must
        /// be present inside the ZIP, using relative paths. The only exceptions are
        /// memory-mapped files.
        /// 
        /// **(API Extension)**
        /// 
        void Redirect(const char *FileInZip)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ZIP_Redirect(ctx, FileInZip);
        }
        /// 
        /// Runs a "Redirect" command inside the current (open) ZIP file.
        /// In the current implementation, all files required by the script must
        /// be present inside the ZIP, using relative paths. The only exceptions are
        /// memory-mapped files.
        /// 
        /// **(API Extension)**
        /// 
        void Redirect(const string &FileInZip)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ZIP_Redirect(ctx, FileInZip.c_str());
        }
        /// 
        /// Extracts the contents of the file "FileName" from the current (open) ZIP file.
        /// Returns a byte-string.
        /// 
        /// **(API Extension)**
        /// 
        template <typename VectorT=Eigen::Matrix<int8_t, Eigen::Dynamic, 1>>
        VectorT Extract(const char *FileName)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ZIP_Extract_GR(ctx, FileName);
            return api_util->get_int8_gr_array<VectorT>();
        }
        /// 
        /// Extracts the contents of the file "FileName" from the current (open) ZIP file.
        /// Returns a byte-string.
        /// 
        /// **(API Extension)**
        /// 
        template <typename VectorT=Eigen::Matrix<int8_t, Eigen::Dynamic, 1>>
        VectorT Extract(const string &FileName)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ZIP_Extract_GR(ctx, FileName.c_str());
            return api_util->get_int8_gr_array<VectorT>();
        }
        /// 
        /// Check if the given path name is present in the current ZIP file.
        /// 
        /// **(API Extension)**
        /// 
        bool Contains(const char *Name)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_ZIP_Contains(ctx, Name);
        }
        /// 
        /// Check if the given path name is present in the current ZIP file.
        /// 
        /// **(API Extension)**
        /// 
        bool Contains(const string &Name)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_ZIP_Contains(ctx, Name.c_str());
        }
    };

    class IActiveClass: public ContextState
    {
    public:

        IActiveClass(altdss::APIUtil *util) :
            ContextState(util)
        {
        }

        /// 
        /// Returns name of active class.
        /// 
        /// Original COM help: https://opendss.epri.com/ActiveClassName.html
        /// 
        string ActiveClassName() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_ActiveClass_Get_ActiveClassName(ctx);
        }

        /// 
        /// Array of strings consisting of all element names in the active class.
        /// 
        /// Original COM help: https://opendss.epri.com/AllNames.html
        /// 
        strings AllNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_ActiveClass_Get_AllNames);
        }

        /// 
        /// Number of elements in Active Class. Same as NumElements Property.
        /// 
        /// Original COM help: https://opendss.epri.com/Count.html
        /// 
        int32_t Count() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_ActiveClass_Get_Count(ctx);
        }

        /// 
        /// Sets first element in the active class to be the active DSS object. 
        /// If the object is a CktElement, ActiveCktElement also points to this element. 
        /// 
        /// Returns 0 if none.
        /// 
        /// Original COM help: https://opendss.epri.com/First.html
        /// 
        int32_t First() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_ActiveClass_Get_First(ctx);
        }

        /// 
        /// Name of the Active Element of the Active Class
        /// 
        /// Original COM help: https://opendss.epri.com/Name.html
        /// 
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_ActiveClass_Get_Name(ctx);
        }
        IActiveClass& Name(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ActiveClass_Set_Name(ctx, value);
            return *this;
        }
        IActiveClass& Name(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ActiveClass_Set_Name(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Sets next element in active class to be the active DSS object. 
        /// If the object is a CktElement, ActiveCktElement also points to this element.
        /// 
        /// Returns 0 if no more.
        /// 
        /// Original COM help: https://opendss.epri.com/Next.html
        /// 
        int32_t Next() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_ActiveClass_Get_Next(ctx);
        }

        /// 
        /// Number of elements in this class. Same as Count property.
        /// 
        /// Original COM help: https://opendss.epri.com/NumElements.html
        /// 
        int32_t NumElements() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_ActiveClass_Get_NumElements(ctx);
        }

        /// 
        /// Get the name of the parent class of the active class
        /// 
        /// Original COM help: https://opendss.epri.com/ActiveClassParent.html
        /// 
        string ActiveClassParent() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_ActiveClass_Get_ActiveClassParent(ctx);
        }
        /// 
        /// Returns the data (as a list) of all elements from the active class as a JSON-encoded string.
        /// 
        /// The `options` parameter contains bit-flags to toggle specific features.
        /// See `Obj_ToJSON` (C-API) for more, or `DSSObj.to_json` in Python.
        /// 
        /// Additionally, the `ExcludeDisabled` flag can be used to excluded disabled elements from the output.
        /// 
        /// **(API Extension)**
        /// 
        string ToJSON(int32_t options=0)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_ActiveClass_ToJSON(ctx, options);
        }
    };

    class IBus: public ContextState
    {
    public:

        IBus(altdss::APIUtil *util) :
            ContextState(util)
        {
        }
    
        IBus& operator[](int32_t key) // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_SetActiveBusi(ctx, key);
            return *this;
        }

        IBus& operator[](const char *key) // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_SetActiveBus(ctx, key);
            return *this;
        }
        IBus& operator[](const string &key) // getter
        {
            return (*this)[key.c_str()];
        }

        ///
        /// Returns an array with the names of all PCE connected to the active bus
        ///
        strings AllPCEatBus() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Bus_Get_AllPCEatBus);
        }

        ///
        /// Returns an array with the names of all PDE connected to the active bus
        ///
        strings AllPDEatBus() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Bus_Get_AllPDEatBus);
        }
        /// 
        /// Return a unique node number at the active bus to avoid node collisions and adds 
        /// it to the node list for the bus.
        /// 
        /// Original COM help: https://opendss.epri.com/GetUniqueNodeNumber.html
        /// 
        int32_t GetUniqueNodeNumber(int32_t StartNumber)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Bus_GetUniqueNodeNumber(ctx, StartNumber);
        }
        /// 
        /// Refreshes the Zsc matrix for the active bus.
        /// 
        /// Original COM help: https://opendss.epri.com/ZscRefresh.html
        /// 
        bool ZscRefresh()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Bus_ZscRefresh(ctx);
        }

        /// 
        /// Indicates whether a coordinate has been defined for this bus
        /// 
        /// Original COM help: https://opendss.epri.com/Coorddefined.html
        /// 
        bool Coorddefined() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Bus_Get_Coorddefined(ctx);
        }

        /// 
        /// Complex array of Sequence Voltages (0, 1, 2) at this Bus.
        /// 
        /// Original COM help: https://opendss.epri.com/CplxSeqVoltages.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT CplxSeqVoltages() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Bus_Get_CplxSeqVoltages_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Accumulated customer outage durations
        /// 
        /// *Requires a previous call to `RelCalc` command*
        /// 
        /// Original COM help: https://opendss.epri.com/Cust_Duration.html
        /// 
        double Cust_Duration() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Bus_Get_Cust_Duration(ctx);
        }

        /// 
        /// Annual number of customer-interruptions from this bus
        /// 
        /// *Requires a previous call to `RelCalc` command*
        /// 
        /// Original COM help: https://opendss.epri.com/Cust_Interrupts.html
        /// 
        double Cust_Interrupts() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Bus_Get_Cust_Interrupts(ctx);
        }

        /// 
        /// Distance from EnergyMeter (if non-zero)
        /// 
        /// *Requires an energy meter with an updated zone.*
        /// 
        /// Original COM help: https://opendss.epri.com/Distance.html
        /// 
        double Distance() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Bus_Get_Distance(ctx);
        }

        /// 
        /// Average interruption duration, hours.
        /// 
        /// *Requires a previous call to `RelCalc` command*
        /// 
        /// Original COM help: https://opendss.epri.com/Int_Duration.html
        /// 
        double Int_Duration() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Bus_Get_Int_Duration(ctx);
        }

        /// 
        /// Short circuit currents at bus; Complex Array.
        /// 
        /// *Requires a previous solution in `FaultStudy` mode.*
        /// 
        /// Original COM help: https://opendss.epri.com/Isc.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Isc() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Bus_Get_Isc_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Accumulated failure rate downstream from this bus; faults per year
        /// 
        /// *Requires a previous call to `RelCalc` command*
        /// 
        /// Original COM help: https://opendss.epri.com/Lambda.html
        /// 
        double Lambda() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Bus_Get_Lambda(ctx);
        }

        /// 
        /// Total numbers of customers served downline from this bus
        /// 
        /// *Requires a previous call to `RelCalc` command*
        /// 
        /// Original COM help: https://opendss.epri.com/N_Customers.html
        /// 
        int32_t N_Customers() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Bus_Get_N_Customers(ctx);
        }

        /// 
        /// Number of interruptions this bus per year
        /// 
        /// *Requires a previous call to `RelCalc` command*
        /// 
        /// Original COM help: https://opendss.epri.com/N_interrupts.html
        /// 
        double N_interrupts() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Bus_Get_N_interrupts(ctx);
        }

        /// 
        /// Name of the active Bus
        /// 
        /// Original COM help: https://opendss.epri.com/Name1.html
        /// 
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Bus_Get_Name(ctx);
        }

        /// 
        /// Integer Array of Node Numbers defined at the bus in same order as the voltages.
        /// 
        /// Original COM help: https://opendss.epri.com/Nodes.html
        /// 
        template <typename VectorT=Eigen::Matrix<int32_t, Eigen::Dynamic, 1>>
        VectorT Nodes() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Bus_Get_Nodes_GR(ctx);
            return api_util->get_int32_gr_array<VectorT>();
        }

        /// 
        /// Number of Nodes this bus.
        /// 
        /// Original COM help: https://opendss.epri.com/NumNodes.html
        /// 
        int32_t NumNodes() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Bus_Get_NumNodes(ctx);
        }

        /// 
        /// Integer ID of the feeder section in which this bus is located.
        /// 
        /// *Requires a previous call to `RelCalc` command*
        /// 
        /// Original COM help: https://opendss.epri.com/SectionID.html
        /// 
        int32_t SectionID() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Bus_Get_SectionID(ctx);
        }

        /// 
        /// Double Array of sequence voltages at this bus. Magnitudes only.
        /// 
        /// Original COM help: https://opendss.epri.com/SeqVoltages.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT SeqVoltages() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Bus_Get_SeqVoltages_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Total length of line downline from this bus, in miles. For recloser siting algorithm.
        /// 
        /// *Requires a previous call to `RelCalc` command*
        /// 
        /// Original COM help: https://opendss.epri.com/TotalMiles.html
        /// 
        double TotalMiles() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Bus_Get_TotalMiles(ctx);
        }

        /// 
        /// For 2- and 3-phase buses, returns array of complex numbers representing L-L voltages in volts. Returns -1.0 for 1-phase bus. If more than 3 phases, returns only first 3.
        /// 
        /// Original COM help: https://opendss.epri.com/VLL.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT VLL() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Bus_Get_VLL_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Array of doubles containing voltages in Magnitude (VLN), angle (degrees) 
        /// 
        /// Original COM help: https://opendss.epri.com/VMagAngle.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT VMagAngle() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Bus_Get_VMagAngle_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Open circuit voltage; Complex array.
        /// 
        /// *Requires a previous solution in `FaultStudy` mode.*
        /// 
        /// Original COM help: https://opendss.epri.com/Voc.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Voc() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Bus_Get_Voc_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Complex array of voltages at this bus.
        /// 
        /// Original COM help: https://opendss.epri.com/Voltages.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Voltages() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Bus_Get_Voltages_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Complex array of Ysc matrix at bus. Column by column.
        /// 
        /// *Requires a previous solution in `FaultStudy` mode or a call to `ZSCRefresh`.*
        /// 
        /// Original COM help: https://opendss.epri.com/YscMatrix.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT YscMatrix() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Bus_Get_YscMatrix_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Complex Zero-Sequence short circuit impedance at bus.
        /// 
        /// *Requires a previous solution in `FaultStudy` mode or a call to `ZSCRefresh`.*
        /// 
        /// Original COM help: https://opendss.epri.com/Zsc0.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Zsc0() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Bus_Get_Zsc0_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Complex Positive-Sequence short circuit impedance at bus.
        /// 
        /// *Requires a previous solution in `FaultStudy` mode or a call to `ZSCRefresh`.*
        /// 
        /// Original COM help: https://opendss.epri.com/Zsc1.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Zsc1() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Bus_Get_Zsc1_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Complex array of Zsc matrix at bus. Column by column.
        /// 
        /// *Requires a previous solution in `FaultStudy` mode or a call to `ZSCRefresh`.*
        /// 
        /// Original COM help: https://opendss.epri.com/ZscMatrix.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT ZscMatrix() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Bus_Get_ZscMatrix_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Base voltage at bus in kV
        /// 
        /// Original COM help: https://opendss.epri.com/kVBase.html
        /// 
        double kVBase() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Bus_Get_kVBase(ctx);
        }

        /// 
        /// Returns Complex array of pu L-L voltages for 2- and 3-phase buses. Returns -1.0 for 1-phase bus. If more than 3 phases, returns only 3 phases.
        /// 
        /// Original COM help: https://opendss.epri.com/puVLL.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT puVLL() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Bus_Get_puVLL_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Array of doubles containing voltage magnitude, angle (degrees) pairs in per unit
        /// 
        /// Original COM help: https://opendss.epri.com/puVmagAngle.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT puVmagAngle() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Bus_Get_puVmagAngle_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Complex Array of pu voltages at the bus.
        /// 
        /// Original COM help: https://opendss.epri.com/puVoltages.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT puVoltages() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Bus_Get_puVoltages_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Complex array containing the complete 012 Zsc matrix. 
        /// Only available after Zsc is computed, either through the "ZscRefresh" command, or running a "FaultStudy" solution.
        /// Only available for buses with 3 nodes.
        /// 
        /// *Requires a previous solution in `FaultStudy` mode or a call to `ZSCRefresh`.*
        /// 
        /// Original COM help: https://opendss.epri.com/ZSC012Matrix.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT ZSC012Matrix() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Bus_Get_ZSC012Matrix_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// X Coordinate for bus
        /// 
        /// Original COM help: https://opendss.epri.com/x.html
        /// 
        double x() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Bus_Get_x(ctx);
        }
        IBus& x(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Bus_Set_x(ctx, value);
            return *this;
        }

        /// 
        /// Y coordinate for bus
        /// 
        /// Original COM help: https://opendss.epri.com/y.html
        /// 
        double y() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Bus_Get_y(ctx);
        }
        IBus& y(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Bus_Set_y(ctx, value);
            return *this;
        }

        /// 
        /// List of strings: Full Names of LOAD elements connected to the active bus.
        /// 
        /// Original COM help: https://opendss.epri.com/LoadList.html
        /// 
        strings LoadList() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Bus_Get_LoadList);
        }

        /// 
        /// List of strings: Full Names of LINE elements connected to the active bus.
        /// 
        /// Original COM help: https://opendss.epri.com/LineList.html
        /// 
        strings LineList() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Bus_Get_LineList);
        }

    };

    class ICNData: public ContextState
    {
    public:

        ICNData(altdss::APIUtil *util) :
            ContextState(util)
        {
        }

        ///
        /// Array of strings with all CNData names in the circuit.
        ///
        strings AllNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_CNData_Get_AllNames);
        }

        ///
        /// Number of CNData objects in active circuit.
        ///
        int32_t Count() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CNData_Get_Count(ctx);
        }

        ///
        /// Sets the first CNData active. Returns 0 if no more.
        ///
        int32_t First() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CNData_Get_First(ctx);
        }

        ///
        /// Get the name of the current active CNData
        ///
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CNData_Get_Name(ctx);
        }

        ///
        /// Sets the active CNData by Name.
        ///
        ICNData& Name(const char *value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CNData_Set_Name(ctx, value);
            return *this;
        }
        ICNData& Name(const string &value)
        {
            return Name(value.c_str());
        }

        ///
        /// Sets the next CNData active. Returns 0 if no more.
        ///
        int32_t Next() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CNData_Get_Next(ctx);
        }

        ///
        /// Get active CNData by index; index is 1-based: 1..count
        ///
        int32_t idx() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CNData_Get_idx(ctx);
        }

        ///
        /// Get active CNData by index; index is 1-based: 1..count
        ///
        void idx(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CNData_Set_idx(ctx, value);
        }

        /// 
        /// Emergency ampere rating
        /// 
        double EmergAmps() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CNData_Get_EmergAmps(ctx);
        }
        ICNData& EmergAmps(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CNData_Set_EmergAmps(ctx, value);
            return *this;
        }

        /// 
        /// Normal Ampere rating
        /// 
        double NormAmps() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CNData_Get_NormAmps(ctx);
        }
        ICNData& NormAmps(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CNData_Set_NormAmps(ctx, value);
            return *this;
        }

        double Rdc() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CNData_Get_Rdc(ctx);
        }
        ICNData& Rdc(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CNData_Set_Rdc(ctx, value);
            return *this;
        }

        double Rac() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CNData_Get_Rac(ctx);
        }
        ICNData& Rac(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CNData_Set_Rac(ctx, value);
            return *this;
        }

        double GMRac() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CNData_Get_GMRac(ctx);
        }
        ICNData& GMRac(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CNData_Set_GMRac(ctx, value);
            return *this;
        }

        LineUnits GMRUnits() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return static_cast<LineUnits>(ctx_CNData_Get_GMRUnits(ctx));
        }
        ICNData& GMRUnits(LineUnits value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CNData_Set_GMRUnits(ctx, static_cast<int32_t>(value));
            return *this;
        }

        double Radius() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CNData_Get_Radius(ctx);
        }
        ICNData& Radius(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CNData_Set_Radius(ctx, value);
            return *this;
        }

        LineUnits RadiusUnits() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return static_cast<LineUnits>(ctx_CNData_Get_RadiusUnits(ctx));
        }
        ICNData& RadiusUnits(LineUnits value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CNData_Set_RadiusUnits(ctx, static_cast<int32_t>(value));
            return *this;
        }

        LineUnits ResistanceUnits() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return static_cast<LineUnits>(ctx_CNData_Get_ResistanceUnits(ctx));
        }
        ICNData& ResistanceUnits(LineUnits value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CNData_Set_ResistanceUnits(ctx, static_cast<int32_t>(value));
            return *this;
        }

        double Diameter() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CNData_Get_Diameter(ctx);
        }
        ICNData& Diameter(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CNData_Set_Diameter(ctx, value);
            return *this;
        }

        double EpsR() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CNData_Get_EpsR(ctx);
        }
        ICNData& EpsR(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CNData_Set_EpsR(ctx, value);
            return *this;
        }

        double InsLayer() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CNData_Get_InsLayer(ctx);
        }
        ICNData& InsLayer(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CNData_Set_InsLayer(ctx, value);
            return *this;
        }

        double DiaIns() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CNData_Get_DiaIns(ctx);
        }
        ICNData& DiaIns(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CNData_Set_DiaIns(ctx, value);
            return *this;
        }

        double DiaCable() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CNData_Get_DiaCable(ctx);
        }
        ICNData& DiaCable(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CNData_Set_DiaCable(ctx, value);
            return *this;
        }

        int32_t k() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CNData_Get_k(ctx);
        }
        ICNData& k(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CNData_Set_k(ctx, value);
            return *this;
        }

        double DiaStrand() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CNData_Get_DiaStrand(ctx);
        }
        ICNData& DiaStrand(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CNData_Set_DiaStrand(ctx, value);
            return *this;
        }

        double GmrStrand() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CNData_Get_GmrStrand(ctx);
        }
        ICNData& GmrStrand(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CNData_Set_GmrStrand(ctx, value);
            return *this;
        }

        double RStrand() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CNData_Get_RStrand(ctx);
        }
        ICNData& RStrand(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CNData_Set_RStrand(ctx, value);
            return *this;
        }
    };

    class ICapControls: public ContextState
    {
    public:

        ICapControls(altdss::APIUtil *util) :
            ContextState(util)
        {
        }

        ///
        /// Array of strings with all CapControl names in the circuit.
        ///
        strings AllNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_CapControls_Get_AllNames);
        }

        ///
        /// Number of CapControl objects in active circuit.
        ///
        int32_t Count() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CapControls_Get_Count(ctx);
        }

        ///
        /// Sets the first CapControl active. Returns 0 if no more.
        ///
        int32_t First() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CapControls_Get_First(ctx);
        }

        ///
        /// Get the name of the current active CapControl
        ///
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CapControls_Get_Name(ctx);
        }

        ///
        /// Sets the active CapControl by Name.
        ///
        ICapControls& Name(const char *value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CapControls_Set_Name(ctx, value);
            return *this;
        }
        ICapControls& Name(const string &value)
        {
            return Name(value.c_str());
        }

        ///
        /// Sets the next CapControl active. Returns 0 if no more.
        ///
        int32_t Next() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CapControls_Get_Next(ctx);
        }

        ///
        /// Get active CapControl by index; index is 1-based: 1..count
        ///
        int32_t idx() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CapControls_Get_idx(ctx);
        }

        ///
        /// Get active CapControl by index; index is 1-based: 1..count
        ///
        void idx(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CapControls_Set_idx(ctx, value);
        }
        /// 
        /// Force a reset of this CapControl.
        /// 
        /// Original COM help: https://opendss.epri.com/Reset.html
        /// 
        void Reset()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CapControls_Reset(ctx);
        }

        /// 
        /// Transducer ratio from primary current to control current.
        /// 
        /// Original COM help: https://opendss.epri.com/CTratio.html
        /// 
        double CTratio() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CapControls_Get_CTratio(ctx);
        }
        ICapControls& CTratio(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CapControls_Set_CTratio(ctx, value);
            return *this;
        }

        /// 
        /// Name of the Capacitor that is controlled.
        /// 
        /// Original COM help: https://opendss.epri.com/Capacitor.html
        /// 
        string Capacitor() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CapControls_Get_Capacitor(ctx);
        }
        ICapControls& Capacitor(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CapControls_Set_Capacitor(ctx, value);
            return *this;
        }
        ICapControls& Capacitor(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CapControls_Set_Capacitor(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Dead time after capacitor is turned OFF before it can be turned back ON for the active CapControl.
        /// 
        /// Default is 300 sec.
        /// 
        /// Original COM help: https://opendss.epri.com/DeadTime.html
        /// 
        double DeadTime() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CapControls_Get_DeadTime(ctx);
        }
        ICapControls& DeadTime(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CapControls_Set_DeadTime(ctx, value);
            return *this;
        }

        /// 
        /// Time delay [s] to switch on after arming.  Control may reset before actually switching.
        /// 
        /// Original COM help: https://opendss.epri.com/Delay.html
        /// 
        double Delay() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CapControls_Get_Delay(ctx);
        }
        ICapControls& Delay(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CapControls_Set_Delay(ctx, value);
            return *this;
        }

        /// 
        /// Time delay [s] before switching off a step. Control may reset before actually switching.
        /// 
        /// Original COM help: https://opendss.epri.com/DelayOff.html
        /// 
        double DelayOff() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CapControls_Get_DelayOff(ctx);
        }
        ICapControls& DelayOff(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CapControls_Set_DelayOff(ctx, value);
            return *this;
        }

        /// 
        /// Type of automatic controller.
        /// 
        /// Original COM help: https://opendss.epri.com/Mode.html
        /// 
        CapControlModes Mode() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return static_cast<CapControlModes>(ctx_CapControls_Get_Mode(ctx));
        }
        ICapControls& Mode(CapControlModes value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CapControls_Set_Mode(ctx, static_cast<int32_t>(value));
            return *this;
        }

        /// 
        /// Full name of the element that PT and CT are connected to.
        /// 
        /// Original COM help: https://opendss.epri.com/MonitoredObj.html
        /// 
        string MonitoredObj() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CapControls_Get_MonitoredObj(ctx);
        }
        ICapControls& MonitoredObj(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CapControls_Set_MonitoredObj(ctx, value);
            return *this;
        }
        ICapControls& MonitoredObj(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CapControls_Set_MonitoredObj(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Terminal number on the element that PT and CT are connected to.
        /// 
        /// Original COM help: https://opendss.epri.com/MonitoredTerm.html
        /// 
        int32_t MonitoredTerm() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CapControls_Get_MonitoredTerm(ctx);
        }
        ICapControls& MonitoredTerm(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CapControls_Set_MonitoredTerm(ctx, value);
            return *this;
        }

        /// 
        /// Threshold to switch off a step. See Mode for units.
        /// 
        /// Original COM help: https://opendss.epri.com/OFFSetting.html
        /// 
        double OFFSetting() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CapControls_Get_OFFSetting(ctx);
        }
        ICapControls& OFFSetting(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CapControls_Set_OFFSetting(ctx, value);
            return *this;
        }

        /// 
        /// Threshold to arm or switch on a step.  See Mode for units.
        /// 
        /// Original COM help: https://opendss.epri.com/ONSetting.html
        /// 
        double ONSetting() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CapControls_Get_ONSetting(ctx);
        }
        ICapControls& ONSetting(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CapControls_Set_ONSetting(ctx, value);
            return *this;
        }

        /// 
        /// Transducer ratio from primary feeder to control voltage.
        /// 
        /// Original COM help: https://opendss.epri.com/PTratio.html
        /// 
        double PTratio() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CapControls_Get_PTratio(ctx);
        }
        ICapControls& PTratio(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CapControls_Set_PTratio(ctx, value);
            return *this;
        }

        /// 
        /// Enables Vmin and Vmax to override the control Mode
        /// 
        /// Original COM help: https://opendss.epri.com/UseVoltOverride.html
        /// 
        bool UseVoltOverride() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CapControls_Get_UseVoltOverride(ctx);
        }
        ICapControls& UseVoltOverride(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CapControls_Set_UseVoltOverride(ctx, value);
            return *this;
        }

        /// 
        /// With VoltOverride, swtich off whenever PT voltage exceeds this level.
        /// 
        /// Original COM help: https://opendss.epri.com/Vmax.html
        /// 
        double Vmax() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CapControls_Get_Vmax(ctx);
        }
        ICapControls& Vmax(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CapControls_Set_Vmax(ctx, value);
            return *this;
        }

        /// 
        /// With VoltOverride, switch ON whenever PT voltage drops below this level.
        /// 
        /// Original COM help: https://opendss.epri.com/Vmin.html
        /// 
        double Vmin() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CapControls_Get_Vmin(ctx);
        }
        ICapControls& Vmin(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CapControls_Set_Vmin(ctx, value);
            return *this;
        }
    };

    class ICapacitors: public ContextState
    {
    public:

        ICapacitors(altdss::APIUtil *util) :
            ContextState(util)
        {
        }

        ///
        /// Array of strings with all Capacitor names in the circuit.
        ///
        strings AllNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Capacitors_Get_AllNames);
        }

        ///
        /// Number of Capacitor objects in active circuit.
        ///
        int32_t Count() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Capacitors_Get_Count(ctx);
        }

        ///
        /// Sets the first Capacitor active. Returns 0 if no more.
        ///
        int32_t First() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Capacitors_Get_First(ctx);
        }

        ///
        /// Get the name of the current active Capacitor
        ///
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Capacitors_Get_Name(ctx);
        }

        ///
        /// Sets the active Capacitor by Name.
        ///
        ICapacitors& Name(const char *value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Capacitors_Set_Name(ctx, value);
            return *this;
        }
        ICapacitors& Name(const string &value)
        {
            return Name(value.c_str());
        }

        ///
        /// Sets the next Capacitor active. Returns 0 if no more.
        ///
        int32_t Next() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Capacitors_Get_Next(ctx);
        }

        ///
        /// Get active Capacitor by index; index is 1-based: 1..count
        ///
        int32_t idx() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Capacitors_Get_idx(ctx);
        }

        ///
        /// Get active Capacitor by index; index is 1-based: 1..count
        ///
        void idx(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Capacitors_Set_idx(ctx, value);
        }
        bool AddStep()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Capacitors_AddStep(ctx);
        }
        void Close()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Capacitors_Close(ctx);
        }
        void Open()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Capacitors_Open(ctx);
        }
        bool SubtractStep()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Capacitors_SubtractStep(ctx);
        }

        /// 
        /// Number of Steps available in cap bank to be switched ON.
        /// 
        /// Original COM help: https://opendss.epri.com/AvailableSteps.html
        /// 
        int32_t AvailableSteps() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Capacitors_Get_AvailableSteps(ctx);
        }

        /// 
        /// Delta connection or wye?
        /// 
        /// Original COM help: https://opendss.epri.com/IsDelta.html
        /// 
        bool IsDelta() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Capacitors_Get_IsDelta(ctx);
        }
        ICapacitors& IsDelta(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Capacitors_Set_IsDelta(ctx, value);
            return *this;
        }

        /// 
        /// Number of steps (default 1) for distributing and switching the total bank kVAR.
        /// 
        /// Original COM help: https://opendss.epri.com/NumSteps.html
        /// 
        int32_t NumSteps() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Capacitors_Get_NumSteps(ctx);
        }
        ICapacitors& NumSteps(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Capacitors_Set_NumSteps(ctx, value);
            return *this;
        }

        /// 
        /// An array of integers [0..NumSteps-1] indicating state of each step. If the read value is -1 an error has occurred.
        /// 
        /// Original COM help: https://opendss.epri.com/States.html
        /// 
        template <typename VectorT=Eigen::Matrix<int32_t, Eigen::Dynamic, 1>>
        VectorT States() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Capacitors_Get_States_GR(ctx);
            return api_util->get_int32_gr_array<VectorT>();
        }
        template <typename VectorT=Eigen::Matrix<int32_t, Eigen::Dynamic, 1>>
        ICapacitors& States(const VectorT &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Capacitors_Set_States(ctx, &value[0], value.size());
            return *this;
        }

        /// 
        /// Bank kV rating. Use LL for 2 or 3 phases, or actual can rating for 1 phase.
        /// 
        /// Original COM help: https://opendss.epri.com/kV.html
        /// 
        double kV() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Capacitors_Get_kV(ctx);
        }
        ICapacitors& kV(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Capacitors_Set_kV(ctx, value);
            return *this;
        }

        /// 
        /// Total bank KVAR, distributed equally among phases and steps.
        /// 
        double kvar() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Capacitors_Get_kvar(ctx);
        }
        ICapacitors& kvar(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Capacitors_Set_kvar(ctx, value);
            return *this;
        }
    };

    class ICtrlQueue: public ContextState
    {
    public:

        ICtrlQueue(altdss::APIUtil *util) :
            ContextState(util)
        {
        }
        /// 
        /// Clear all actions from the Control Proxy's Action List (they are popped off the list). 
        /// 
        /// Original COM help: https://opendss.epri.com/ClearActions.html
        /// 
        void ClearActions()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CtrlQueue_ClearActions(ctx);
        }
        /// 
        /// Clear the control queue.
        /// 
        /// Original COM help: https://opendss.epri.com/ClearQueue.html
        /// 
        void ClearQueue()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CtrlQueue_ClearQueue(ctx);
        }
        /// 
        /// Delete an Action from the DSS Control Queue by the handle that is returned when the action is added.
        /// 
        /// (The Push function returns the handle.)
        /// 
        /// Original COM help: https://opendss.epri.com/Delete.html
        /// 
        void Delete(int32_t ActionHandle)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CtrlQueue_Delete(ctx, ActionHandle);
        }
        /// 
        /// Execute all actions currently on the Control Queue. 
        /// 
        /// Side effect: clears the queue.
        /// 
        /// Original COM help: https://opendss.epri.com/DoAllQueue.html
        /// 
        void DoAllQueue()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CtrlQueue_DoAllQueue(ctx);
        }
        /// 
        /// Export the queue to a CSV table and show it.
        /// 
        /// Original COM help: https://opendss.epri.com/Show.html
        /// 
        void Show()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CtrlQueue_Show(ctx);
        }

        /// 
        /// Code for the active action. Integer code to tell the control device what to do.
        /// 
        /// Use this to determine what the user-defined controls are supposed to do.
        /// It can be any 32-bit integer of the user's choosing and is the same value that the control pushed onto the control queue earlier.
        /// 
        /// Original COM help: https://opendss.epri.com/ActionCode.html
        /// 
        int32_t ActionCode() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CtrlQueue_Get_ActionCode(ctx);
        }

        /// 
        /// Handle (User defined) to device that must act on the pending action.
        /// 
        /// The user-written code driving the interface may support more than one 
        /// control element as necessary to perform the simulation. This handle is
        /// an index returned to the user program that lets the program know which
        /// control is to perform the active action.
        /// 
        /// Original COM help: https://opendss.epri.com/DeviceHandle.html   
        /// 
        int32_t DeviceHandle() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CtrlQueue_Get_DeviceHandle(ctx);
        }

        /// 
        /// Number of Actions on the current action list (that have been popped off the control queue by CheckControlActions)
        /// 
        /// Original COM help: https://opendss.epri.com/NumActions.html
        /// 
        int32_t NumActions() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CtrlQueue_Get_NumActions(ctx);
        }
        /// 
        /// Push a control action onto the DSS control queue by time, action code, and device handle (user defined). Returns Control Queue handle.
        /// 
        /// Original COM help: https://opendss.epri.com/Push.html
        /// 
        int32_t Push(int32_t Hour, double Seconds, int32_t ActionCode, int32_t DeviceHandle)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CtrlQueue_Push(ctx, Hour, Seconds, ActionCode, DeviceHandle);
        }

        /// 
        /// Pops next action off the action list and makes it the active action. Returns zero if none.
        /// 
        /// Original COM help: https://opendss.epri.com/PopAction.html
        /// 
        int32_t PopAction() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CtrlQueue_Get_PopAction(ctx);
        }

        /// 
        /// Array of strings containing the entire queue in CSV format
        /// 
        /// Original COM help: https://opendss.epri.com/Queue.html
        /// 
        strings Queue() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_CtrlQueue_Get_Queue);
        }

        /// 
        /// Number of items on the OpenDSS control Queue
        /// 
        /// Original COM help: https://opendss.epri.com/QueueSize.html
        /// 
        int32_t QueueSize() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CtrlQueue_Get_QueueSize(ctx);
        }

        /// 
        /// (write-only) Set the active action by index
        /// 
        /// Original COM help: https://opendss.epri.com/Action.html
        /// 
        ICtrlQueue& Action(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CtrlQueue_Set_Action(ctx, value);
            return *this;
        }
    };

    class IDSSElement: public ContextState
    {
    public:
        IDSSProperty Properties;

        IDSSElement(altdss::APIUtil *util) :
            ContextState(util),
            Properties(util)
        {
        }

        /// 
        /// Array of strings containing the names of all properties for the active DSS object.
        /// 
        /// Original COM help: https://opendss.epri.com/AllPropertyNames1.html
        /// 
        strings AllPropertyNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_DSSElement_Get_AllPropertyNames);
        }

        /// 
        /// Full Name of Active DSS Object (general element or circuit element).
        /// 
        /// Original COM help: https://opendss.epri.com/Name5.html
        /// 
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSSElement_Get_Name(ctx);
        }

        /// 
        /// Number of Properties for the active DSS object.
        /// 
        /// Original COM help: https://opendss.epri.com/NumProperties1.html
        /// 
        int32_t NumProperties() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSSElement_Get_NumProperties(ctx);
        }
        /// 
        /// Returns the properties of the active DSS object as a JSON-encoded string.
        /// 
        /// The `options` parameter contains bit-flags to toggle specific features.
        /// See `Obj_ToJSON` (C-API) for more, or `DSSObj.to_json` in Python.
        /// 
        /// **(API Extension)**
        /// 
        string ToJSON(int32_t options=0)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSSElement_ToJSON(ctx, options);
        }
    };

    class ILineGeometries: public ContextState
    {
    public:

        ILineGeometries(altdss::APIUtil *util) :
            ContextState(util)
        {
        }

        ///
        /// Array of strings with all LineGeometrie names in the circuit.
        ///
        strings AllNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_LineGeometries_Get_AllNames);
        }

        ///
        /// Number of LineGeometrie objects in active circuit.
        ///
        int32_t Count() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LineGeometries_Get_Count(ctx);
        }

        ///
        /// Sets the first LineGeometrie active. Returns 0 if no more.
        ///
        int32_t First() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LineGeometries_Get_First(ctx);
        }

        ///
        /// Get the name of the current active LineGeometrie
        ///
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LineGeometries_Get_Name(ctx);
        }

        ///
        /// Sets the active LineGeometrie by Name.
        ///
        ILineGeometries& Name(const char *value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineGeometries_Set_Name(ctx, value);
            return *this;
        }
        ILineGeometries& Name(const string &value)
        {
            return Name(value.c_str());
        }

        ///
        /// Sets the next LineGeometrie active. Returns 0 if no more.
        ///
        int32_t Next() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LineGeometries_Get_Next(ctx);
        }

        ///
        /// Get active LineGeometrie by index; index is 1-based: 1..count
        ///
        int32_t idx() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LineGeometries_Get_idx(ctx);
        }

        ///
        /// Get active LineGeometrie by index; index is 1-based: 1..count
        ///
        void idx(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineGeometries_Set_idx(ctx, value);
        }

        /// 
        /// Array of strings with names of all conductors in the active LineGeometry object
        /// 
        strings Conductors() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_LineGeometries_Get_Conductors);
        }

        /// 
        /// Emergency ampere rating
        /// 
        double EmergAmps() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LineGeometries_Get_EmergAmps(ctx);
        }
        ILineGeometries& EmergAmps(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineGeometries_Set_EmergAmps(ctx, value);
            return *this;
        }

        /// 
        /// Normal ampere rating
        /// 
        double NormAmps() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LineGeometries_Get_NormAmps(ctx);
        }
        ILineGeometries& NormAmps(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineGeometries_Set_NormAmps(ctx, value);
            return *this;
        }

        double RhoEarth() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LineGeometries_Get_RhoEarth(ctx);
        }
        ILineGeometries& RhoEarth(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineGeometries_Set_RhoEarth(ctx, value);
            return *this;
        }

        bool Reduce() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LineGeometries_Get_Reduce(ctx);
        }
        ILineGeometries& Reduce(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineGeometries_Set_Reduce(ctx, value);
            return *this;
        }

        /// 
        /// Number of Phases
        /// 
        int32_t Phases() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LineGeometries_Get_Phases(ctx);
        }
        ILineGeometries& Phases(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineGeometries_Set_Phases(ctx, value);
            return *this;
        }
        /// 
        /// Resistance matrix, ohms
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Rmatrix(double Frequency, double Length, int32_t Units)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineGeometries_Get_Rmatrix_GR(ctx, Frequency, Length, Units);
            return api_util->get_float64_gr_array<VectorT>();
        }
        /// 
        /// Reactance matrix, ohms
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Xmatrix(double Frequency, double Length, int32_t Units)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineGeometries_Get_Xmatrix_GR(ctx, Frequency, Length, Units);
            return api_util->get_float64_gr_array<VectorT>();
        }
        /// 
        /// Complex impedance matrix, ohms
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Zmatrix(double Frequency, double Length, int32_t Units)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineGeometries_Get_Zmatrix_GR(ctx, Frequency, Length, Units);
            return api_util->get_float64_gr_array<VectorT>();
        }
        /// 
        /// Capacitance matrix, nF
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Cmatrix(double Frequency, double Length, int32_t Units)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineGeometries_Get_Cmatrix_GR(ctx, Frequency, Length, Units);
            return api_util->get_float64_gr_array<VectorT>();
        }

        template <typename VectorT=Eigen::Matrix<int32_t, Eigen::Dynamic, 1>>
        VectorT Units() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineGeometries_Get_Units_GR(ctx);
            return api_util->get_int32_gr_array<VectorT>();
        }
        template <typename VectorT=Eigen::Matrix<int32_t, Eigen::Dynamic, 1>>
        ILineGeometries& Units(const VectorT &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineGeometries_Set_Units(ctx, &value[0], value.size());
            return *this;
        }

        /// 
        /// Get/Set the X (horizontal) coordinates of the conductors
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Xcoords() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineGeometries_Get_Xcoords_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        ILineGeometries& Xcoords(const VectorT &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineGeometries_Set_Xcoords(ctx, &value[0], value.size());
            return *this;
        }

        /// 
        /// Get/Set the Y (vertical/height) coordinates of the conductors
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Ycoords() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineGeometries_Get_Ycoords_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        ILineGeometries& Ycoords(const VectorT &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineGeometries_Set_Ycoords(ctx, &value[0], value.size());
            return *this;
        }

        /// 
        /// Number of conductors in this geometry. Default is 3. Triggers memory allocations. Define first!
        /// 
        int32_t Nconds() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LineGeometries_Get_Nconds(ctx);
        }
        ILineGeometries& Nconds(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineGeometries_Set_Nconds(ctx, value);
            return *this;
        }
    };

    class ILines: public ContextState
    {
    public:

        ILines(altdss::APIUtil *util) :
            ContextState(util)
        {
        }

        ///
        /// Array of strings with all Line names in the circuit.
        ///
        strings AllNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Lines_Get_AllNames);
        }

        ///
        /// Number of Line objects in active circuit.
        ///
        int32_t Count() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_Get_Count(ctx);
        }

        ///
        /// Sets the first Line active. Returns 0 if no more.
        ///
        int32_t First() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_Get_First(ctx);
        }

        ///
        /// Get the name of the current active Line
        ///
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_Get_Name(ctx);
        }

        ///
        /// Sets the active Line by Name.
        ///
        ILines& Name(const char *value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Lines_Set_Name(ctx, value);
            return *this;
        }
        ILines& Name(const string &value)
        {
            return Name(value.c_str());
        }

        ///
        /// Sets the next Line active. Returns 0 if no more.
        ///
        int32_t Next() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_Get_Next(ctx);
        }

        ///
        /// Get active Line by index; index is 1-based: 1..count
        ///
        int32_t idx() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_Get_idx(ctx);
        }

        ///
        /// Get active Line by index; index is 1-based: 1..count
        ///
        void idx(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Lines_Set_idx(ctx, value);
        }
        /// 
        /// Create new Line object with the given `Name`
        /// 
        int32_t New(const char *Name)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_New(ctx, Name);
        }
        /// 
        /// Create new Line object with the given `Name`
        /// 
        int32_t New(const string &Name)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_New(ctx, Name.c_str());
        }

        /// 
        /// Name of bus for terminal 1.
        /// 
        /// Original COM help: https://opendss.epri.com/Bus1.html
        /// 
        string Bus1() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_Get_Bus1(ctx);
        }
        ILines& Bus1(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Lines_Set_Bus1(ctx, value);
            return *this;
        }
        ILines& Bus1(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Lines_Set_Bus1(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Name of bus for terminal 2.
        /// 
        /// Original COM help: https://opendss.epri.com/Bus2.html
        /// 
        string Bus2() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_Get_Bus2(ctx);
        }
        ILines& Bus2(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Lines_Set_Bus2(ctx, value);
            return *this;
        }
        ILines& Bus2(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Lines_Set_Bus2(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Zero Sequence capacitance, nanofarads per unit length.
        /// 
        /// Original COM help: https://opendss.epri.com/C0.html
        /// 
        double C0() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_Get_C0(ctx);
        }
        ILines& C0(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Lines_Set_C0(ctx, value);
            return *this;
        }

        /// 
        /// Positive Sequence capacitance, nanofarads per unit length.
        /// 
        /// Original COM help: https://opendss.epri.com/C1.html
        /// 
        double C1() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_Get_C1(ctx);
        }
        ILines& C1(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Lines_Set_C1(ctx, value);
            return *this;
        }

        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Cmatrix() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Lines_Get_Cmatrix_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        ILines& Cmatrix(const VectorT &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Lines_Set_Cmatrix(ctx, &value[0], value.size());
            return *this;
        }

        /// 
        /// Emergency (maximum) ampere rating of Line.
        /// 
        /// Original COM help: https://opendss.epri.com/EmergAmps1.html
        /// 
        double EmergAmps() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_Get_EmergAmps(ctx);
        }
        ILines& EmergAmps(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Lines_Set_EmergAmps(ctx, value);
            return *this;
        }

        /// 
        /// Line geometry code
        /// 
        /// Original COM help: https://opendss.epri.com/Geometry.html
        /// 
        string Geometry() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_Get_Geometry(ctx);
        }
        ILines& Geometry(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Lines_Set_Geometry(ctx, value);
            return *this;
        }
        ILines& Geometry(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Lines_Set_Geometry(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Length of line section in units compatible with the LineCode definition.
        /// 
        /// Original COM help: https://opendss.epri.com/Length.html
        /// 
        double Length() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_Get_Length(ctx);
        }
        ILines& Length(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Lines_Set_Length(ctx, value);
            return *this;
        }

        /// 
        /// Name of LineCode object that defines the impedances.
        /// 
        /// Original COM help: https://opendss.epri.com/LineCode.html
        /// 
        string LineCode() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_Get_LineCode(ctx);
        }
        ILines& LineCode(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Lines_Set_LineCode(ctx, value);
            return *this;
        }
        ILines& LineCode(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Lines_Set_LineCode(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Normal ampere rating of Line.
        /// 
        /// Original COM help: https://opendss.epri.com/NormAmps.html
        /// 
        double NormAmps() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_Get_NormAmps(ctx);
        }
        ILines& NormAmps(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Lines_Set_NormAmps(ctx, value);
            return *this;
        }

        /// 
        /// Number of customers on this line section.
        /// 
        /// *Requires an energy meter with an updated zone.*
        /// 
        /// Original COM help: https://opendss.epri.com/NumCust.html
        /// 
        int32_t NumCust() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_Get_NumCust(ctx);
        }

        /// 
        /// Sets Parent of the active Line to be the active line. Returns 0 if no parent or action fails.
        /// 
        /// *Requires an energy meter with an updated zone.*
        /// 
        /// Original COM help: https://opendss.epri.com/Parent.html
        /// 
        int32_t Parent() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_Get_Parent(ctx);
        }

        /// 
        /// Number of Phases, this Line element.
        /// 
        /// Original COM help: https://opendss.epri.com/Phases1.html
        /// 
        int32_t Phases() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_Get_Phases(ctx);
        }
        ILines& Phases(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Lines_Set_Phases(ctx, value);
            return *this;
        }

        /// 
        /// Zero Sequence resistance, ohms per unit length.
        /// 
        /// Original COM help: https://opendss.epri.com/R0.html
        /// 
        double R0() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_Get_R0(ctx);
        }
        ILines& R0(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Lines_Set_R0(ctx, value);
            return *this;
        }

        /// 
        /// Positive Sequence resistance, ohms per unit length.
        /// 
        /// Original COM help: https://opendss.epri.com/R1.html
        /// 
        double R1() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_Get_R1(ctx);
        }
        ILines& R1(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Lines_Set_R1(ctx, value);
            return *this;
        }

        /// 
        /// Earth return resistance value used to compute line impedances at power frequency
        /// 
        /// Original COM help: https://opendss.epri.com/Rg.html
        /// 
        double Rg() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_Get_Rg(ctx);
        }
        ILines& Rg(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Lines_Set_Rg(ctx, value);
            return *this;
        }

        /// 
        /// Earth Resistivity, m-ohms
        /// 
        /// Original COM help: https://opendss.epri.com/Rho.html
        /// 
        double Rho() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_Get_Rho(ctx);
        }
        ILines& Rho(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Lines_Set_Rho(ctx, value);
            return *this;
        }

        /// 
        /// Resistance matrix (full), ohms per unit length. Array of doubles.
        /// 
        /// Original COM help: https://opendss.epri.com/Rmatrix.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Rmatrix() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Lines_Get_Rmatrix_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        ILines& Rmatrix(const VectorT &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Lines_Set_Rmatrix(ctx, &value[0], value.size());
            return *this;
        }

        /// 
        /// Line spacing code
        /// 
        /// Original COM help: https://opendss.epri.com/Spacing.html
        /// 
        string Spacing() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_Get_Spacing(ctx);
        }
        ILines& Spacing(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Lines_Set_Spacing(ctx, value);
            return *this;
        }
        ILines& Spacing(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Lines_Set_Spacing(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Total Number of customers served from this line section.
        /// 
        /// Original COM help: https://opendss.epri.com/TotalCust.html
        /// 
        int32_t TotalCust() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_Get_TotalCust(ctx);
        }

        int32_t Units() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return static_cast<LineUnits>(ctx_Lines_Get_Units(ctx));
        }
        ILines& Units(LineUnits value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Lines_Set_Units(ctx, static_cast<int32_t>(value));
            return *this;
        }

        /// 
        /// Zero Sequence reactance ohms per unit length.
        /// 
        /// Original COM help: https://opendss.epri.com/X0.html
        /// 
        double X0() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_Get_X0(ctx);
        }
        ILines& X0(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Lines_Set_X0(ctx, value);
            return *this;
        }

        /// 
        /// Positive Sequence reactance, ohms per unit length.
        /// 
        /// Original COM help: https://opendss.epri.com/X1.html
        /// 
        double X1() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_Get_X1(ctx);
        }
        ILines& X1(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Lines_Set_X1(ctx, value);
            return *this;
        }

        /// 
        /// Earth return reactance value used to compute line impedances at power frequency
        /// 
        /// Original COM help: https://opendss.epri.com/Xg.html
        /// 
        double Xg() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_Get_Xg(ctx);
        }
        ILines& Xg(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Lines_Set_Xg(ctx, value);
            return *this;
        }

        /// 
        /// Reactance matrix (full), ohms per unit length. Array of doubles.
        /// 
        /// Original COM help: https://opendss.epri.com/Xmatrix.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Xmatrix() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Lines_Get_Xmatrix_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        ILines& Xmatrix(const VectorT &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Lines_Set_Xmatrix(ctx, &value[0], value.size());
            return *this;
        }

        /// 
        /// Yprimitive for the active line object (complex array).
        /// 
        /// Original COM help: https://opendss.epri.com/Yprim1.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Yprim() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Lines_Get_Yprim_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        ILines& Yprim(const VectorT &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Lines_Set_Yprim(ctx, &value[0], value.size());
            return *this;
        }

        /// 
        /// Delivers the rating for the current season (in Amps)  if the "SeasonalRatings" option is active
        /// 
        /// Original COM help: https://opendss.epri.com/SeasonRating.html
        /// 
        double SeasonRating() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_Get_SeasonRating(ctx);
        }

        /// 
        /// Line element switch status. Setting it has side-effects to the line parameters.
        /// 
        /// **(API Extension)**
        /// 
        bool IsSwitch() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_Get_IsSwitch(ctx);
        }
        ILines& IsSwitch(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Lines_Set_IsSwitch(ctx, value);
            return *this;
        }
    };

    class ILoads: public ContextState
    {
    public:

        ILoads(altdss::APIUtil *util) :
            ContextState(util)
        {
        }

        ///
        /// Array of strings with all Load names in the circuit.
        ///
        strings AllNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Loads_Get_AllNames);
        }

        ///
        /// Number of Load objects in active circuit.
        ///
        int32_t Count() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_Count(ctx);
        }

        ///
        /// Sets the first Load active. Returns 0 if no more.
        ///
        int32_t First() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_First(ctx);
        }

        ///
        /// Get the name of the current active Load
        ///
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_Name(ctx);
        }

        ///
        /// Sets the active Load by Name.
        ///
        ILoads& Name(const char *value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_Name(ctx, value);
            return *this;
        }
        ILoads& Name(const string &value)
        {
            return Name(value.c_str());
        }

        ///
        /// Sets the next Load active. Returns 0 if no more.
        ///
        int32_t Next() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_Next(ctx);
        }

        ///
        /// Get active Load by index; index is 1-based: 1..count
        ///
        int32_t idx() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_idx(ctx);
        }

        ///
        /// Get active Load by index; index is 1-based: 1..count
        ///
        void idx(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_idx(ctx, value);
        }

        /// 
        /// Factor for allocating loads by connected xfkva
        /// 
        /// Original COM help: https://opendss.epri.com/AllocationFactor.html
        /// 
        double AllocationFactor() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_AllocationFactor(ctx);
        }
        ILoads& AllocationFactor(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_AllocationFactor(ctx, value);
            return *this;
        }

        /// 
        /// Name of a loadshape with both Mult and Qmult, for CVR factors as a function of time.
        /// 
        /// Original COM help: https://opendss.epri.com/CVRcurve.html
        /// 
        string CVRcurve() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_CVRcurve(ctx);
        }
        ILoads& CVRcurve(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_CVRcurve(ctx, value);
            return *this;
        }
        ILoads& CVRcurve(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_CVRcurve(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Percent reduction in Q for percent reduction in V. Must be used with dssLoadModelCVR.
        /// 
        /// Original COM help: https://opendss.epri.com/CVRvars.html
        /// 
        double CVRvars() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_CVRvars(ctx);
        }
        ILoads& CVRvars(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_CVRvars(ctx, value);
            return *this;
        }

        /// 
        /// Percent reduction in P for percent reduction in V. Must be used with dssLoadModelCVR.
        /// 
        /// Original COM help: https://opendss.epri.com/CVRwatts.html
        /// 
        double CVRwatts() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_CVRwatts(ctx);
        }
        ILoads& CVRwatts(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_CVRwatts(ctx, value);
            return *this;
        }

        /// 
        /// CFactor relates average to peak kw.  Used for allocation with kwh and kwhdays
        /// 
        /// Original COM help: https://opendss.epri.com/Cfactor.html
        /// 
        double Cfactor() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_Cfactor(ctx);
        }
        ILoads& Cfactor(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_Cfactor(ctx, value);
            return *this;
        }

        /// 
        /// Code number used to separate loads by class or group. No effect on the solution.
        /// 
        /// Original COM help: https://opendss.epri.com/Class.html
        /// 
        int32_t Class() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_Class_(ctx);
        }
        ILoads& Class(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_Class_(ctx, value);
            return *this;
        }

        /// 
        /// Name of the growthshape curve for yearly load growth factors.
        /// 
        /// Original COM help: https://opendss.epri.com/Growth.html
        /// 
        string Growth() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_Growth(ctx);
        }
        ILoads& Growth(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_Growth(ctx, value);
            return *this;
        }
        ILoads& Growth(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_Growth(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Delta loads are connected line-to-line.
        /// 
        /// Original COM help: https://opendss.epri.com/IsDelta1.html
        /// 
        bool IsDelta() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_IsDelta(ctx);
        }
        ILoads& IsDelta(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_IsDelta(ctx, value);
            return *this;
        }

        /// 
        /// The Load Model defines variation of P and Q with voltage.
        /// 
        /// Original COM help: https://opendss.epri.com/Model1.html
        /// 
        LoadModels Model() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return static_cast<LoadModels>(ctx_Loads_Get_Model(ctx));
        }
        ILoads& Model(LoadModels value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_Model(ctx, static_cast<int32_t>(value));
            return *this;
        }

        /// 
        /// Number of customers in this load, defaults to one.
        /// 
        /// Original COM help: https://opendss.epri.com/NumCust1.html
        /// 
        int32_t NumCust() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_NumCust(ctx);
        }
        ILoads& NumCust(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_NumCust(ctx, value);
            return *this;
        }

        /// 
        /// Get or set Power Factor for Active Load. Specify leading PF as negative. Updates kvar based on present value of kW
        /// 
        /// Original COM help: https://opendss.epri.com/PF1.html
        /// 
        double PF() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_PF(ctx);
        }
        ILoads& PF(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_PF(ctx, value);
            return *this;
        }

        /// 
        /// Average percent of nominal load in Monte Carlo studies; only if no loadshape defined for this load.
        /// 
        /// Original COM help: https://opendss.epri.com/PctMean.html
        /// 
        double PctMean() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_PctMean(ctx);
        }
        ILoads& PctMean(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_PctMean(ctx, value);
            return *this;
        }

        /// 
        /// Percent standard deviation for Monte Carlo load studies; if there is no loadshape assigned to this load.
        /// 
        /// Original COM help: https://opendss.epri.com/PctStdDev.html
        /// 
        double PctStdDev() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_PctStdDev(ctx);
        }
        ILoads& PctStdDev(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_PctStdDev(ctx, value);
            return *this;
        }

        /// 
        /// Relative Weighting factor for the active load.
        /// 
        /// This value is used in reliability methods.
        /// 
        /// Original COM help: https://opendss.epri.com/RelWeight.html
        /// 
        double RelWeight() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_RelWeight(ctx);
        }
        ILoads& RelWeight(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_RelWeight(ctx, value);
            return *this;
        }

        /// 
        /// Neutral resistance for wye-connected loads.
        /// 
        /// Original COM help: https://opendss.epri.com/Rneut.html
        /// 
        double Rneut() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_Rneut(ctx);
        }
        ILoads& Rneut(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_Rneut(ctx, value);
            return *this;
        }

        /// 
        /// Name of harmonic current spectrum shape.
        /// 
        /// Original COM help: https://opendss.epri.com/Spectrum.html
        /// 
        string Spectrum() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_Spectrum(ctx);
        }
        ILoads& Spectrum(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_Spectrum(ctx, value);
            return *this;
        }
        ILoads& Spectrum(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_Spectrum(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Response to load multipliers: Fixed (growth only), Exempt (no LD curve), Variable (all).
        /// 
        /// Original COM help: https://opendss.epri.com/Status.html
        /// 
        LoadStatus Status() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return static_cast<LoadStatus>(ctx_Loads_Get_Status(ctx));
        }
        ILoads& Status(LoadStatus value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_Status(ctx, static_cast<int32_t>(value));
            return *this;
        }

        /// 
        /// Maximum per-unit voltage to use the load model. Above this, constant Z applies.
        /// 
        /// Original COM help: https://opendss.epri.com/Vmaxpu1.html
        /// 
        double Vmaxpu() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_Vmaxpu(ctx);
        }
        ILoads& Vmaxpu(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_Vmaxpu(ctx, value);
            return *this;
        }

        /// 
        /// Minimum voltage for unserved energy (UE) evaluation.
        /// 
        /// Original COM help: https://opendss.epri.com/Vminemerg.html
        /// 
        double Vminemerg() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_Vminemerg(ctx);
        }
        ILoads& Vminemerg(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_Vminemerg(ctx, value);
            return *this;
        }

        /// 
        /// Minimum voltage for energy exceeding normal (EEN) evaluations.
        /// 
        /// Original COM help: https://opendss.epri.com/Vminnorm.html
        /// 
        double Vminnorm() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_Vminnorm(ctx);
        }
        ILoads& Vminnorm(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_Vminnorm(ctx, value);
            return *this;
        }

        /// 
        /// Minimum voltage to apply the load model. Below this, constant Z is used.
        /// 
        /// Original COM help: https://opendss.epri.com/Vminpu1.html
        /// 
        double Vminpu() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_Vminpu(ctx);
        }
        ILoads& Vminpu(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_Vminpu(ctx, value);
            return *this;
        }

        /// 
        /// Neutral reactance for wye-connected loads.
        /// 
        /// Original COM help: https://opendss.epri.com/Xneut.html
        /// 
        double Xneut() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_Xneut(ctx);
        }
        ILoads& Xneut(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_Xneut(ctx, value);
            return *this;
        }

        /// 
        /// Name of yearly duration loadshape
        /// 
        /// Original COM help: https://opendss.epri.com/Yearly.html
        /// 
        string Yearly() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_Yearly(ctx);
        }
        ILoads& Yearly(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_Yearly(ctx, value);
            return *this;
        }
        ILoads& Yearly(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_Yearly(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Array of 7 doubles with values for ZIPV property of the load object
        /// 
        /// Original COM help: https://opendss.epri.com/ZIPV.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT ZIPV() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Get_ZIPV_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        ILoads& ZIPV(const VectorT &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_ZIPV(ctx, &value[0], value.size());
            return *this;
        }

        /// 
        /// Name of the loadshape for a daily load profile.
        /// 
        /// Original COM help: https://opendss.epri.com/daily.html
        /// 
        string daily() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_daily(ctx);
        }
        ILoads& daily(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_daily(ctx, value);
            return *this;
        }
        ILoads& daily(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_daily(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Name of the loadshape for a duty cycle simulation.
        /// 
        /// Original COM help: https://opendss.epri.com/duty.html
        /// 
        string duty() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_duty(ctx);
        }
        ILoads& duty(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_duty(ctx, value);
            return *this;
        }
        ILoads& duty(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_duty(ctx, value.c_str());
            return *this;
        }

        /// 
        /// kV rating for active Load. For 2 or more phases set Line-Line kV. Else actual kV across terminals.
        /// 
        /// Original COM help: https://opendss.epri.com/kV2.html
        /// 
        double kV() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_kV(ctx);
        }
        ILoads& kV(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_kV(ctx, value);
            return *this;
        }

        /// 
        /// Set kW for active Load. Updates kvar based on present PF.
        /// 
        /// Original COM help: https://opendss.epri.com/kW1.html
        /// 
        double kW() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_kW(ctx);
        }
        ILoads& kW(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_kW(ctx, value);
            return *this;
        }

        /// 
        /// Base load kva. Also defined kw and kvar or pf input, or load allocation by kwh or xfkva.
        /// 
        /// Original COM help: https://opendss.epri.com/kva.html
        /// 
        double kva() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_kva(ctx);
        }
        ILoads& kva(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_kva(ctx, value);
            return *this;
        }

        /// 
        /// Reactive power in kvar for active Load. If set, updates PF based on present kW.
        /// 
        /// Original COM help: https://opendss.epri.com/kvar1.html
        /// 
        double kvar() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_kvar(ctx);
        }
        ILoads& kvar(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_kvar(ctx, value);
            return *this;
        }

        /// 
        /// kWh billed for this period. Can be used with Cfactor for load allocation.
        /// 
        /// Original COM help: https://opendss.epri.com/kwh.html
        /// 
        double kwh() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_kwh(ctx);
        }
        ILoads& kwh(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_kwh(ctx, value);
            return *this;
        }

        /// 
        /// Length of kWh billing period for average demand calculation. Default 30.
        /// 
        /// Original COM help: https://opendss.epri.com/kwhdays.html
        /// 
        double kwhdays() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_kwhdays(ctx);
        }
        ILoads& kwhdays(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_kwhdays(ctx, value);
            return *this;
        }

        /// 
        /// Percent of Load that is modeled as series R-L for harmonics studies
        /// 
        /// Original COM help: https://opendss.epri.com/pctSeriesRL.html
        /// 
        double pctSeriesRL() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_pctSeriesRL(ctx);
        }
        ILoads& pctSeriesRL(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_pctSeriesRL(ctx, value);
            return *this;
        }

        /// 
        /// Rated service transformer kVA for load allocation, using AllocationFactor. Affects kW, kvar, and pf.
        /// 
        /// Original COM help: https://opendss.epri.com/xfkVA.html
        /// 
        double xfkVA() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_xfkVA(ctx);
        }
        ILoads& xfkVA(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_xfkVA(ctx, value);
            return *this;
        }

        /// 
        /// Name of the sensor monitoring this load.
        /// 
        /// Original COM help: https://opendss.epri.com/Sensor.html
        /// 
        string Sensor() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_Sensor(ctx);
        }

        /// 
        /// Number of phases
        /// 
        /// **(API Extension)**
        /// 
        int32_t Phases() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_Phases(ctx);
        }
        ILoads& Phases(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_Phases(ctx, value);
            return *this;
        }
    };

    class ISettings: public ContextState
    {
    public:

        ISettings(altdss::APIUtil *util) :
            ContextState(util)
        {
        }
        /// 
        /// Returns a Settings context manager. 
        /// The context manager saves the values of the tracker settings on enter, 
        /// restoring them on exit. This allows code to change the settings within 
        /// the context block and they are restored to the initial values automatically.
        /// 
        /// Note: this context manager target DSS-Python settings. Use the equivalent for OpenDSSDirect.py.
        /// A few settings are shared at engine level.
        /// 
        /// Settings tracked:
        /// - AdvancedTypes
        /// - CompatFlags
        /// - IterateDisabled
        /// - PreferLists
        /// - SkipCommands
        /// - SkipFileRegExp
        /// 
        SettingsContext Context()
        {
            return SettingsContext(*this);
        }

        /// 
        /// Designates whether to allow duplicate names of objects
        /// 
        /// False by default.
        /// 
        /// **NOTE**: for DSS-Extensions, we are considering removing this option in a future 
        /// release since it has performance impacts even when not used.
        /// 
        bool AllowDuplicates() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Settings_Get_AllowDuplicates(ctx);
        }
        ISettings& AllowDuplicates(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Settings_Set_AllowDuplicates(ctx, value);
            return *this;
        }

        /// 
        /// List of Buses or (File=xxxx) syntax for the AutoAdd solution mode.
        /// 
        /// Original COM help: https://opendss.epri.com/AutoBusList.html
        /// 
        string AutoBusList() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Settings_Get_AutoBusList(ctx);
        }
        ISettings& AutoBusList(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Settings_Set_AutoBusList(ctx, value);
            return *this;
        }
        ISettings& AutoBusList(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Settings_Set_AutoBusList(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Indicate if the circuit model is positive sequence.
        /// 
        /// Original COM help: https://opendss.epri.com/CktModel.html
        /// 
        CktModels CktModel() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return static_cast<CktModels>(ctx_Settings_Get_CktModel(ctx));
        }
        ISettings& CktModel(CktModels value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Settings_Set_CktModel(ctx, static_cast<int32_t>(value));
            return *this;
        }

        /// 
        /// Denotes whether to trace the control actions to a file.
        /// 
        /// Original COM help: https://opendss.epri.com/ControlTrace.html
        /// 
        bool ControlTrace() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Settings_Get_ControlTrace(ctx);
        }
        ISettings& ControlTrace(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Settings_Set_ControlTrace(ctx, value);
            return *this;
        }

        /// 
        /// Per Unit maximum voltage for Emergency conditions.
        /// 
        /// Original COM help: https://opendss.epri.com/EmergVmaxpu.html
        /// 
        double EmergVmaxpu() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Settings_Get_EmergVmaxpu(ctx);
        }
        ISettings& EmergVmaxpu(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Settings_Set_EmergVmaxpu(ctx, value);
            return *this;
        }

        /// 
        /// Per Unit minimum voltage for Emergency conditions.
        /// 
        /// Original COM help: https://opendss.epri.com/EmergVminpu.html
        /// 
        double EmergVminpu() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Settings_Get_EmergVminpu(ctx);
        }
        ISettings& EmergVminpu(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Settings_Set_EmergVminpu(ctx, value);
            return *this;
        }

        /// 
        /// Integer array defining which energy meter registers to use for computing losses
        /// 
        /// Original COM help: https://opendss.epri.com/LossRegs.html
        /// 
        template <typename VectorT=Eigen::Matrix<int32_t, Eigen::Dynamic, 1>>
        VectorT LossRegs() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Settings_Get_LossRegs_GR(ctx);
            return api_util->get_int32_gr_array<VectorT>();
        }
        template <typename VectorT=Eigen::Matrix<int32_t, Eigen::Dynamic, 1>>
        ISettings& LossRegs(const VectorT &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Settings_Set_LossRegs(ctx, &value[0], value.size());
            return *this;
        }

        /// 
        /// Weighting factor applied to Loss register values.
        /// 
        /// Original COM help: https://opendss.epri.com/LossWeight.html
        /// 
        double LossWeight() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Settings_Get_LossWeight(ctx);
        }
        ISettings& LossWeight(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Settings_Set_LossWeight(ctx, value);
            return *this;
        }

        /// 
        /// Per Unit maximum voltage for Normal conditions.
        /// 
        /// Original COM help: https://opendss.epri.com/NormVmaxpu.html
        /// 
        double NormVmaxpu() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Settings_Get_NormVmaxpu(ctx);
        }
        ISettings& NormVmaxpu(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Settings_Set_NormVmaxpu(ctx, value);
            return *this;
        }

        /// 
        /// Per Unit minimum voltage for Normal conditions.
        /// 
        /// Original COM help: https://opendss.epri.com/NormVminpu.html
        /// 
        double NormVminpu() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Settings_Get_NormVminpu(ctx);
        }
        ISettings& NormVminpu(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Settings_Set_NormVminpu(ctx, value);
            return *this;
        }

        /// 
        /// Name of LoadShape object that serves as the source of price signal data for yearly simulations, etc.
        /// 
        /// Original COM help: https://opendss.epri.com/PriceCurve.html
        /// 
        string PriceCurve() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Settings_Get_PriceCurve(ctx);
        }
        ISettings& PriceCurve(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Settings_Set_PriceCurve(ctx, value);
            return *this;
        }
        ISettings& PriceCurve(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Settings_Set_PriceCurve(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Price Signal for the Circuit
        /// 
        /// Original COM help: https://opendss.epri.com/PriceSignal.html
        /// 
        double PriceSignal() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Settings_Get_PriceSignal(ctx);
        }
        ISettings& PriceSignal(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Settings_Set_PriceSignal(ctx, value);
            return *this;
        }

        /// 
        /// Gets value of trapezoidal integration flag in energy meters. Defaults to `False`.
        /// 
        /// Original COM help: https://opendss.epri.com/Trapezoidal.html
        /// 
        bool Trapezoidal() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Settings_Get_Trapezoidal(ctx);
        }
        ISettings& Trapezoidal(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Settings_Set_Trapezoidal(ctx, value);
            return *this;
        }

        /// 
        /// Array of Integers defining energy meter registers to use for computing UE
        /// 
        /// Original COM help: https://opendss.epri.com/UEregs.html
        /// 
        template <typename VectorT=Eigen::Matrix<int32_t, Eigen::Dynamic, 1>>
        VectorT UEregs() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Settings_Get_UEregs_GR(ctx);
            return api_util->get_int32_gr_array<VectorT>();
        }
        template <typename VectorT=Eigen::Matrix<int32_t, Eigen::Dynamic, 1>>
        ISettings& UEregs(const VectorT &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Settings_Set_UEregs(ctx, &value[0], value.size());
            return *this;
        }

        /// 
        /// Weighting factor applied to UE register values.
        /// 
        /// Original COM help: https://opendss.epri.com/UEweight.html
        /// 
        double UEweight() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Settings_Get_UEweight(ctx);
        }
        ISettings& UEweight(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Settings_Set_UEweight(ctx, value);
            return *this;
        }

        /// 
        /// Array of doubles defining the legal voltage bases in kV L-L
        /// 
        /// Original COM help: https://opendss.epri.com/VoltageBases.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT VoltageBases() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Settings_Get_VoltageBases_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        ISettings& VoltageBases(const VectorT &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Settings_Set_VoltageBases(ctx, &value[0], value.size());
            return *this;
        }

        /// 
        /// Locks Zones on energy meters to prevent rebuilding if a circuit change occurs.
        /// 
        /// Original COM help: https://opendss.epri.com/ZoneLock.html
        /// 
        bool ZoneLock() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Settings_Get_ZoneLock(ctx);
        }
        ISettings& ZoneLock(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Settings_Set_ZoneLock(ctx, value);
            return *this;
        }

        /// 
        /// (write-only) Sets all load allocation factors for all loads defined by XFKVA property to this value.
        /// 
        ISettings& AllocationFactors(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Settings_Set_AllocationFactors(ctx, value);
            return *this;
        }

        /// 
        /// Controls whether the terminals are checked when updating the currents in Load component. Defaults to True.
        /// If the loads are guaranteed to have their terminals closed throughout the simulation, this can be set to False to save some time.
        /// 
        /// **(API Extension)**
        /// 
        bool LoadsTerminalCheck() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Settings_Get_LoadsTerminalCheck(ctx);
        }
        ISettings& LoadsTerminalCheck(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Settings_Set_LoadsTerminalCheck(ctx, value);
            return *this;
        }

        /// 
        /// Controls whether `First`/`Next` iteration includes or skips disabled circuit elements.
        /// The default behavior from OpenDSS is to skip those. The user can still activate the element by name or index.
        /// 
        /// The default value for IterateDisabled is 0, keeping the original behavior.
        /// Set it to 1 (or `True`) to include disabled elements.
        /// Other numeric values are reserved for other potential behaviors.
        /// 
        /// **(API Extension)**
        /// 
        int32_t IterateDisabled() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Settings_Get_IterateDisabled(ctx);
        }
        ISettings& IterateDisabled(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Settings_Set_IterateDisabled(ctx, value);
            return *this;
        }
        /// 
        /// Switch the property names according to the target style.
        /// 
        /// Use this method for compatibility with code that doesn't consider that
        /// OpenDSS is case insensitive. Check the enumeration for more:
        /// [DSSPropertyNameStyle](
        /// 
        /// **(API Extension)**
        /// 
        void SetPropertyNameStyle(DSSPropertyNameStyle value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Settings_SetPropertyNameStyle(ctx, static_cast<int32_t>(value));
        }

        /// 
        /// Regular expression pattern to skip files.
        /// 
        /// If a file name as provided in the input for the `Redirect` and `Compile` commands
        /// matches the regular expression pattern, it is skipped (the file is not read nor
        /// commands contained in the file are executed).
        /// 
        /// Set to an empty string to reset/disable the filter.
        /// 
        /// Case-insensitive.
        /// See https://regex.sorokin.engineer/en/latest/regular_expressions.html for information on 
        /// the expression syntax and options.
        /// 
        /// Even if the `clear` command is included in `Settings.SkipCommands`, the `DSS.ClearAll()` method can 
        /// still be called. It resets both skip settings, `SkipCommands` and `SkipFileRegExp`.
        /// 
        /// **(API Extension)**
        /// 
        string SkipFileRegExp() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Settings_Get_SkipFileRegExp(ctx);
        }
        ISettings& SkipFileRegExp(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Settings_Set_SkipFileRegExp(ctx, (value || ""));
            return *this;
        }
        ISettings& SkipFileRegExp(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Settings_Set_SkipFileRegExp(ctx, (value.c_str() || ""));
            return *this;
        }

        /// 
        /// Controls some compatibility flags introduced to toggle some behavior from EPRI's OpenDSS.
        /// 
        /// **THE FLAGS ARE GLOBAL, affecting all AltDSS engines in the process.**  
        /// CompatFlags for Oddie-loaded instances (OpenDSS and OpenDSS-C engines) are handled by the Oddie code itself,
        /// so it is global for each Oddie library.
        /// 
        /// These flags may change for each version of DSS C-API, but the same value will not be reused. That is,
        /// when we remove a compatibility flag, it will have no effect but will also not affect anything else
        /// besides raising an error if the user tries to toggle a flag that was available in a previous version.
        /// 
        /// We expect to keep a very limited number of flags. Since the flags are more transient than the other
        /// options/flags, it was preferred to add this generic function instead of a separate function per
        /// flag.
        /// 
        /// See the enumeration `DSSCompatFlags` for available flags, including description.
        /// 
        /// **(API Extension)**
        /// 
        DSSCompatFlags CompatFlags() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSS_Get_CompatFlags(ctx);
        }
        ISettings& CompatFlags(DSSCompatFlags value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_DSS_Set_CompatFlags(ctx, static_cast<uint32_t>(value));
            return *this;
        }

        /// 
        /// If enabled, in case of errors or empty arrays, the API returns arrays with values compatible with
        /// EPRI's OpenDSS COM interface. 
        /// 
        /// For example, consider the property `Loads.ZIPV`. If there is no active circuit or active load element:
        /// 
        /// - In the disabled state (COMErrorResults=False), the function will return "[]", an array with 0 elements.
        /// - In the enabled state (COMErrorResults=True), the function will return "[0.0]" instead. This should
        /// be compatible with the return value of EPRI's COM interface.
        /// 
        /// Defaults to false (disabled state) in AltDSS since the v0.15.x series.
        /// 
        /// This does not affect the results when using EPRI's OpenDSS distribution through Oddie.
        /// 
        /// This can also be set through the environment variable `DSS_CAPI_COM_DEFAULTS`. Setting it to 1 enables
        /// the legacy/COM behavior. The value can be toggled through the API at any time.
        /// 
        /// **(API Extension)**
        /// 
        bool COMErrorResults() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSS_Get_COMErrorResults(ctx);
        }
        ISettings& COMErrorResults(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_DSS_Set_COMErrorResults(ctx, value);
            return *this;
        }

        /// 
        /// If enabled, the `DOScmd` command is allowed. Otherwise, an error is reported if the user tries to use it.
        /// 
        /// Defaults to False/0 (disabled state). Users should consider DOScmd deprecated on DSS-Extensions.
        /// 
        /// This can also be set through the environment variable DSS_CAPI_ALLOW_DOSCMD. Setting it to 1 enables
        /// the command.
        /// 
        /// **(API Extension)**
        /// 
        bool AllowDOScmd() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSS_Get_AllowDOScmd(ctx);
        }
        ISettings& AllowDOScmd(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_DSS_Set_AllowDOScmd(ctx, value);
            return *this;
        }

        /// 
        /// If disabled, the engine will not change the active working directory during execution. E.g. a "compile"
        /// command will not "chdir" to the file path.
        /// 
        /// If you have issues with long paths, enabling this might help in some scenarios.
        /// 
        /// Defaults to True (allow changes, backwards compatible) in the 0.10.x versions of DSS C-API. 
        /// This might change to False in future versions.
        /// 
        /// This can also be set through the environment variable DSS_CAPI_ALLOW_CHANGE_DIR. Set it to 0 to
        /// disallow changing the active working directory.
        /// 
        /// **(API Extension)**
        /// 
        bool AllowChangeDir() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSS_Get_AllowChangeDir(ctx);
        }
        ISettings& AllowChangeDir(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_DSS_Set_AllowChangeDir(ctx, value);
            return *this;
        }

        /// 
        /// Gets/sets whether running the external editor for "Show" is allowed
        /// 
        /// AllowEditor controls whether the external editor is used in commands like "Show".
        /// If you set to 0 (false), the editor is not executed. Note that other side effects,
        /// such as the creation of files, are not affected.
        /// 
        /// **(API Extension)**
        /// 
        bool AllowEditor() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSS_Get_AllowEditor(ctx);
        }
        ISettings& AllowEditor(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_DSS_Set_AllowEditor(ctx, value);
            return *this;
        }
    };

    class ICktElement: public ContextState
    {
    public:
        IDSSProperty Properties;

        ICktElement(altdss::APIUtil *util) :
            ContextState(util),
            Properties(util)
        {
        }
    
        ///
        /// Value as return and error code in Code parameter. For PCElement, get the value of a variable by name. If Code>0 then no variable by this name or not a PCelement.
        ///
        double Variable(const char *MyVarName, int32_t &Code)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CktElement_Get_Variable(ctx, MyVarName, &Code);
        }
        double Variable(const string &MyVarName, int32_t &Code)
        {
            return Variable(MyVarName.c_str(), Code);
        }

        ///
        /// Value as return and error code in Code parameter. For PCElement, get the value of a variable by integer index. If Code>0 then no variable by this index or not a PCelement.
        ///
        double Variablei(int32_t Idx, int32_t &Code)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CktElement_Get_Variablei(ctx, Idx, &Code);
        }

        int32_t setVariableByIndex(int32_t Idx, double Value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            int32_t Code = 0;
            ctx_CktElement_Set_Variablei(ctx, Idx, &Code, Value);
            return Code;
        }

        int32_t setVariableByName(const char *name, double Value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            int32_t Code = 0;
            ctx_CktElement_Set_Variable(ctx, name, &Code, Value);
            return Code;
        }
        int32_t setVariableByName(const string &name, double Value)
        {
            return setVariableByName(name.c_str(), Value);
        }
        /// 
        /// Close the specified terminal and phase, if non-zero, or all conductors at the terminal.
        /// 
        /// Original COM help: https://opendss.epri.com/Close1.html
        /// 
        void Close(int32_t Term, int32_t Phs)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Close(ctx, Term, Phs);
        }
        /// 
        /// Full name of the i-th controller attached to this element. Ex: str = Controller(2).  See NumControls to determine valid index range
        /// 
        string Controller(int32_t idx)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CktElement_Get_Controller(ctx, idx);
        }
        /// 
        /// Indicates if the specified terminal and, optionally, a specific phase conductor is open.
        /// 
        /// Provide zero in the `Phs` argument to check if any conductor of the terminal `Term` is open.
        /// 
        /// Provide a non-zero phase number in `Phs` to check if a specific phase conductor is open.
        /// 
        /// Original COM help: https://opendss.epri.com/IsOpen.html
        /// 
        bool IsOpen(int32_t Term, int32_t Phs=0)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CktElement_IsOpen(ctx, Term, Phs);
        }
        /// 
        /// Open the specified terminal and phase, if non-zero, or all conductors at the terminal.
        /// 
        /// Original COM help: https://opendss.epri.com/Open1.html
        /// 
        void Open(int32_t Term, int32_t Phs)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Open(ctx, Term, Phs);
        }

        /// 
        /// Array containing all property names of the active device.
        /// 
        /// Original COM help: https://opendss.epri.com/AllPropertyNames.html
        /// 
        strings AllPropertyNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_CktElement_Get_AllPropertyNames);
        }

        /// 
        /// Array of strings listing all the published state variable names.
        /// Valid only for PCElements.
        /// 
        /// Original COM help: https://opendss.epri.com/AllVariableNames.html
        /// 
        strings AllVariableNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_CktElement_Get_AllVariableNames);
        }

        /// 
        /// Array of doubles. Values of state variables of active element if PC element.
        /// Valid only for PCElements.
        /// 
        /// Original COM help: https://opendss.epri.com/AllVariableValues.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT AllVariableValues() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Get_AllVariableValues_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Bus definitions to which each terminal is connected.
        /// 
        /// The `removeNodes` argument is an **API Extension**. Use it to get only the bus names, 
        /// without the connection/node specification, if present.
        /// 
        /// Original COM help: https://opendss.epri.com/BusNames.html
        /// 
        strings BusNames(bool removeNodes=false)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_CktElement_Get_BusNames, removeNodes);
        }
        ICktElement& BusNames(const strings &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            api_util->set_string_array(ctx_CktElement_Set_BusNames, value);
            return *this;
        }

        /// 
        /// Complex double array of Sequence Currents for all conductors of all terminals of active circuit element.
        /// 
        /// Original COM help: https://opendss.epri.com/CplxSeqCurrents.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT CplxSeqCurrents() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Get_CplxSeqCurrents_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Complex double array of Sequence Voltage for all terminals of active circuit element.
        /// 
        /// Original COM help: https://opendss.epri.com/CplxSeqVoltages1.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT CplxSeqVoltages() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Get_CplxSeqVoltages_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Complex array of currents into each conductor of each terminal
        /// 
        /// Original COM help: https://opendss.epri.com/Currents1.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Currents() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Get_Currents_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Currents in magnitude, angle (degrees) format as an array of doubles.
        /// 
        /// Original COM help: https://opendss.epri.com/CurrentsMagAng.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT CurrentsMagAng() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Get_CurrentsMagAng_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Display name of the object (not necessarily unique)
        /// 
        /// Original COM help: https://opendss.epri.com/DisplayName.html
        /// 
        string DisplayName() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CktElement_Get_DisplayName(ctx);
        }
        ICktElement& DisplayName(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Set_DisplayName(ctx, value);
            return *this;
        }
        ICktElement& DisplayName(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Set_DisplayName(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Emergency Ampere Rating for PD elements
        /// 
        /// Original COM help: https://opendss.epri.com/EmergAmps.html
        /// 
        double EmergAmps() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CktElement_Get_EmergAmps(ctx);
        }
        ICktElement& EmergAmps(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Set_EmergAmps(ctx, value);
            return *this;
        }

        /// 
        /// Boolean indicating that element is currently in the circuit.
        /// 
        /// Original COM help: https://opendss.epri.com/Enabled.html
        /// 
        bool Enabled() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CktElement_Get_Enabled(ctx);
        }
        ICktElement& Enabled(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Set_Enabled(ctx, value);
            return *this;
        }

        /// 
        /// Name of the Energy Meter this element is assigned to.
        /// 
        /// *Requires an energy meter with an updated zone.*
        /// 
        /// Original COM help: https://opendss.epri.com/EnergyMeter.html
        /// 
        string EnergyMeter() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CktElement_Get_EnergyMeter(ctx);
        }

        /// 
        /// GUID/UUID for this object.
        /// 
        /// Original COM help: https://opendss.epri.com/GUID.html
        /// 
        string GUID() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CktElement_Get_GUID(ctx);
        }

        /// 
        /// Index of this element into the circuit's element list.
        /// 
        /// Original COM help: https://opendss.epri.com/Handle.html
        /// 
        int32_t Handle() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CktElement_Get_Handle(ctx);
        }

        /// 
        /// True if a recloser, relay, or fuse controlling this ckt element. OCP = Overcurrent Protection 
        /// 
        /// Original COM help: https://opendss.epri.com/HasOCPDevice.html
        /// 
        bool HasOCPDevice() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CktElement_Get_HasOCPDevice(ctx);
        }

        /// 
        /// True if this element has a SwtControl attached.
        /// 
        /// Original COM help: https://opendss.epri.com/HasSwitchControl.html
        /// 
        bool HasSwitchControl() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CktElement_Get_HasSwitchControl(ctx);
        }

        /// 
        /// True if this element has a CapControl or RegControl attached.
        /// 
        /// Original COM help: https://opendss.epri.com/HasVoltControl.html
        /// 
        bool HasVoltControl() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CktElement_Get_HasVoltControl(ctx);
        }

        /// 
        /// Total losses in the element: two-element double array (complex), in VA (watts, vars)
        /// 
        /// Original COM help: https://opendss.epri.com/Losses1.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Losses() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Get_Losses_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Complex array with the losses by type (total losses, load losses, no-load losses), in VA, for the active circuit element.
        /// 
        /// Added in May 2025. Same as `LossesByType` introduced for Transformers in AltDSS/DSS C-API in May 2019.
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT AllLosses() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Get_AllLosses_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Full Name of Active Circuit Element
        /// 
        /// Original COM help: https://opendss.epri.com/Name4.html
        /// 
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CktElement_Get_Name(ctx);
        }

        /// 
        /// Array of integer containing the node numbers (representing phases, for example) for each conductor of each terminal. 
        /// 
        /// Be sure to run a solution to initialize the values after the circuit is created or modified.
        /// 
        /// Original COM help: https://opendss.epri.com/NodeOrder.html
        /// 
        template <typename VectorT=Eigen::Matrix<int32_t, Eigen::Dynamic, 1>>
        VectorT NodeOrder() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Get_NodeOrder_GR(ctx);
            return api_util->get_int32_gr_array<VectorT>();
        }

        /// 
        /// Normal ampere rating for PD Elements
        /// 
        /// Original COM help: https://opendss.epri.com/NormalAmps.html
        /// 
        double NormalAmps() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CktElement_Get_NormalAmps(ctx);
        }
        ICktElement& NormalAmps(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Set_NormalAmps(ctx, value);
            return *this;
        }

        /// 
        /// Number of Conductors per Terminal
        /// 
        /// Original COM help: https://opendss.epri.com/NumConductors.html
        /// 
        int32_t NumConductors() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CktElement_Get_NumConductors(ctx);
        }

        /// 
        /// Number of controls connected to this device. 
        /// Use to determine valid range for index into Controller array.
        /// 
        /// Original COM help: https://opendss.epri.com/NumControls.html
        /// 
        int32_t NumControls() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CktElement_Get_NumControls(ctx);
        }

        /// 
        /// Number of Phases
        /// 
        /// Original COM help: https://opendss.epri.com/NumPhases.html
        /// 
        int32_t NumPhases() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CktElement_Get_NumPhases(ctx);
        }

        /// 
        /// Number of Properties this Circuit Element.
        /// 
        /// Original COM help: https://opendss.epri.com/NumProperties.html
        /// 
        int32_t NumProperties() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CktElement_Get_NumProperties(ctx);
        }

        /// 
        /// Number of terminals in this Circuit Element
        /// 
        /// Original COM help: https://opendss.epri.com/NumTerminals.html
        /// 
        int32_t NumTerminals() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CktElement_Get_NumTerminals(ctx);
        }

        /// 
        /// Index into Controller list of OCP Device controlling this CktElement
        /// 
        /// Original COM help: https://opendss.epri.com/OCPDevIndex.html
        /// 
        int32_t OCPDevIndex() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CktElement_Get_OCPDevIndex(ctx);
        }

        /// 
        /// 0=None; 1=Fuse; 2=Recloser; 3=Relay;  Type of OCP controller device
        /// 
        /// Original COM help: https://opendss.epri.com/OCPDevType.html
        /// 
        OCPDevType OCPDevType() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return static_cast<OCPDevType>(ctx_CktElement_Get_OCPDevType(ctx));
        }

        /// 
        /// Complex array of losses (kVA) by phase
        /// 
        /// Original COM help: https://opendss.epri.com/PhaseLosses.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT PhaseLosses() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Get_PhaseLosses_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Complex array of powers (kVA) into each conductor of each terminal
        /// 
        /// Original COM help: https://opendss.epri.com/Powers.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Powers() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Get_Powers_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Residual currents for each terminal: (magnitude, angle in degrees)
        /// 
        /// Original COM help: https://opendss.epri.com/Residuals.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Residuals() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Get_Residuals_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Double array of symmetrical component currents (magnitudes only) into each 3-phase terminal
        /// 
        /// Original COM help: https://opendss.epri.com/SeqCurrents.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT SeqCurrents() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Get_SeqCurrents_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Complex array of sequence powers (kW, kvar) into each 3-phase terminal
        /// 
        /// Original COM help: https://opendss.epri.com/SeqPowers.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT SeqPowers() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Get_SeqPowers_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Double array of symmetrical component voltages (magnitudes only) at each 3-phase terminal
        /// 
        /// Original COM help: https://opendss.epri.com/SeqVoltages1.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT SeqVoltages() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Get_SeqVoltages_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Complex array of voltages at terminals
        /// 
        /// Original COM help: https://opendss.epri.com/Voltages1.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Voltages() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Get_Voltages_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Voltages at each conductor in magnitude, angle form as array of doubles.
        /// 
        /// Original COM help: https://opendss.epri.com/VoltagesMagAng.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT VoltagesMagAng() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Get_VoltagesMagAng_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// YPrim matrix, column order, complex numbers
        /// 
        /// Original COM help: https://opendss.epri.com/Yprim.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Yprim() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Get_Yprim_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Order (size) of the active circuit element's primite Y matrix (Yprim), typically `NumConductors * NumTerminals`
        /// 
        /// **(API Extension)**
        /// 
        int32_t YprimOrder() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CktElement_Get_YprimOrder(ctx);
        }

        /// 
        /// Returns true if the current active element is isolated.
        /// Note that this only fetches the current value. See also the Topology interface.
        /// 
        /// **(API Extension)**
        /// 
        bool IsIsolated() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CktElement_Get_IsIsolated(ctx);
        }

        /// 
        /// Returns an array with the total powers (complex, kVA) at ALL terminals of the active circuit element.
        /// 
        /// Original COM help: https://opendss.epri.com/TotalPowers.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT TotalPowers() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Get_TotalPowers_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Array of integers, a copy of the internal NodeRef of the CktElement.
        /// 
        /// Be sure to run a solution to initialize the values after the circuit is created or modified.
        /// 
        /// **(API Extension)**
        /// 
        template <typename VectorT=Eigen::Matrix<int32_t, Eigen::Dynamic, 1>>
        VectorT NodeRef() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Get_NodeRef_GR(ctx);
            return api_util->get_int32_gr_array<VectorT>();
        }
    };

    class IGICSources: public ContextState
    {
    public:

        IGICSources(altdss::APIUtil *util) :
            ContextState(util)
        {
        }

        ///
        /// Array of strings with all GICSource names in the circuit.
        ///
        strings AllNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_GICSources_Get_AllNames);
        }

        ///
        /// Number of GICSource objects in active circuit.
        ///
        int32_t Count() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_GICSources_Get_Count(ctx);
        }

        ///
        /// Sets the first GICSource active. Returns 0 if no more.
        ///
        int32_t First() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_GICSources_Get_First(ctx);
        }

        ///
        /// Get the name of the current active GICSource
        ///
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_GICSources_Get_Name(ctx);
        }

        ///
        /// Sets the active GICSource by Name.
        ///
        IGICSources& Name(const char *value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_GICSources_Set_Name(ctx, value);
            return *this;
        }
        IGICSources& Name(const string &value)
        {
            return Name(value.c_str());
        }

        ///
        /// Sets the next GICSource active. Returns 0 if no more.
        ///
        int32_t Next() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_GICSources_Get_Next(ctx);
        }

        ///
        /// Get active GICSource by index; index is 1-based: 1..count
        ///
        int32_t idx() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_GICSources_Get_idx(ctx);
        }

        ///
        /// Get active GICSource by index; index is 1-based: 1..count
        ///
        void idx(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_GICSources_Set_idx(ctx, value);
        }

        /// 
        /// First bus name of GICSource (Created name)
        /// 
        string Bus1() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_GICSources_Get_Bus1(ctx);
        }

        /// 
        /// Second bus name
        /// 
        string Bus2() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_GICSources_Get_Bus2(ctx);
        }

        /// 
        /// Number of Phases, this GICSource element.
        /// 
        int32_t Phases() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_GICSources_Get_Phases(ctx);
        }
        IGICSources& Phases(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_GICSources_Set_Phases(ctx, value);
            return *this;
        }

        /// 
        /// Northward E Field V/km
        /// 
        double EN() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_GICSources_Get_EN(ctx);
        }
        IGICSources& EN(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_GICSources_Set_EN(ctx, value);
            return *this;
        }

        /// 
        /// Eastward E Field, V/km
        /// 
        double EE() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_GICSources_Get_EE(ctx);
        }
        IGICSources& EE(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_GICSources_Set_EE(ctx, value);
            return *this;
        }

        /// 
        /// Latitude of Bus1 (degrees)
        /// 
        double Lat1() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_GICSources_Get_Lat1(ctx);
        }
        IGICSources& Lat1(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_GICSources_Set_Lat1(ctx, value);
            return *this;
        }

        /// 
        /// Latitude of Bus2 (degrees)
        /// 
        double Lat2() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_GICSources_Get_Lat2(ctx);
        }
        IGICSources& Lat2(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_GICSources_Set_Lat2(ctx, value);
            return *this;
        }

        /// 
        /// Longitude of Bus1 (Degrees)
        /// 
        double Lon1() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_GICSources_Get_Lon1(ctx);
        }
        IGICSources& Lon1(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_GICSources_Set_Lon1(ctx, value);
            return *this;
        }

        /// 
        /// Longitude of Bus2 (Degrees)
        /// 
        double Lon2() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_GICSources_Get_Lon2(ctx);
        }
        IGICSources& Lon2(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_GICSources_Set_Lon2(ctx, value);
            return *this;
        }

        /// 
        /// Specify dc voltage directly
        /// 
        double Volts() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_GICSources_Get_Volts(ctx);
        }
        IGICSources& Volts(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_GICSources_Set_Volts(ctx, value);
            return *this;
        }
    };

    class IStorages: public ContextState
    {
    public:

        IStorages(altdss::APIUtil *util) :
            ContextState(util)
        {
        }

        ///
        /// Array of strings with all Storage names in the circuit.
        ///
        strings AllNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Storages_Get_AllNames);
        }

        ///
        /// Number of Storage objects in active circuit.
        ///
        int32_t Count() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Storages_Get_Count(ctx);
        }

        ///
        /// Sets the first Storage active. Returns 0 if no more.
        ///
        int32_t First() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Storages_Get_First(ctx);
        }

        ///
        /// Get the name of the current active Storage
        ///
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Storages_Get_Name(ctx);
        }

        ///
        /// Sets the active Storage by Name.
        ///
        IStorages& Name(const char *value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Storages_Set_Name(ctx, value);
            return *this;
        }
        IStorages& Name(const string &value)
        {
            return Name(value.c_str());
        }

        ///
        /// Sets the next Storage active. Returns 0 if no more.
        ///
        int32_t Next() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Storages_Get_Next(ctx);
        }

        ///
        /// Get active Storage by index; index is 1-based: 1..count
        ///
        int32_t idx() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Storages_Get_idx(ctx);
        }

        ///
        /// Get active Storage by index; index is 1-based: 1..count
        ///
        void idx(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Storages_Set_idx(ctx, value);
        }

        /// 
        /// Per unit state of charge
        /// 
        double puSOC() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Storages_Get_puSOC(ctx);
        }
        IStorages& puSOC(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Storages_Set_puSOC(ctx, value);
            return *this;
        }

        /// 
        /// Get/set state: 0=Idling; 1=Discharging; -1=Charging;
        /// 
        StorageStates State() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return static_cast<StorageStates>(ctx_Storages_Get_State(ctx));
        }
        IStorages& State(StorageStates value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Storages_Set_State(ctx, static_cast<int32_t>(value));
            return *this;
        }

        /// 
        /// Array of Storage energy meter register names
        /// 
        /// See also the enum `GeneratorRegisters`.
        /// 
        strings RegisterNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Storages_Get_RegisterNames);
        }

        /// 
        /// Array of values in Storage registers.
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT RegisterValues() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Storages_Get_RegisterValues_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Current limit per phase for the IBR when operating in GFM mode.
        /// 
        double AmpLimit() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Storages_Get_AmpLimit(ctx);
        }
        IStorages& AmpLimit(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Storages_Set_AmpLimit(ctx, value);
            return *this;
        }

        /// 
        /// Use it for fine tuning the current limiter when active.
        /// 
        double AmpLimitGain() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Storages_Get_AmpLimitGain(ctx);
        }
        IStorages& AmpLimitGain(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Storages_Set_AmpLimitGain(ctx, value);
            return *this;
        }

        /// 
        /// Dispatch trigger value for charging the Storage.
        /// 
        double ChargeTrigger() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Storages_Get_ChargeTrigger(ctx);
        }
        IStorages& ChargeTrigger(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Storages_Set_ChargeTrigger(ctx, value);
            return *this;
        }

        /// 
        /// Control mode for the inverter. It can be one of {GFM = 1 | GFL* = 0}.
        /// 
        int32_t ControlMode() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Storages_Get_ControlMode(ctx);
        }
        IStorages& ControlMode(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Storages_Set_ControlMode(ctx, value);
            return *this;
        }

        /// 
        /// Dispatch trigger value for discharging the Storage.
        /// 
        double DischargeTrigger() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Storages_Get_DischargeTrigger(ctx);
        }
        IStorages& DischargeTrigger(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Storages_Set_DischargeTrigger(ctx, value);
            return *this;
        }

        /// 
        /// Percentage efficiency for CHARGING the Storage element.
        /// 
        double EffCharge() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Storages_Get_EffCharge(ctx);
        }
        IStorages& EffCharge(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Storages_Set_EffCharge(ctx, value);
            return *this;
        }

        /// 
        /// Percentage efficiency for DISCHARGING the Storage element.
        /// 
        double EffDischarge() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Storages_Get_EffDischarge(ctx);
        }
        IStorages& EffDischarge(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Storages_Set_EffDischarge(ctx, value);
            return *this;
        }

        /// 
        /// Proportional gain for the PI controller within the inverter.
        /// Use it to modify the controller response in dynamics simulation mode.
        /// 
        double Kp() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Storages_Get_Kp(ctx);
        }
        IStorages& Kp(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Storages_Set_Kp(ctx, value);
            return *this;
        }

        /// 
        /// Nominal rated (1.0 per unit) voltage, kV, for Storage element.
        /// 
        double kV() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Storages_Get_kV(ctx);
        }
        IStorages& kV(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Storages_Set_kV(ctx, value);
            return *this;
        }

        /// 
        /// Inverter nameplate capability (in kVA). Used as the base for Dynamics mode and Harmonics mode values.
        /// 
        double kVA() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Storages_Get_kVA(ctx);
        }
        IStorages& kVA(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Storages_Set_kVA(ctx, value);
            return *this;
        }

        /// 
        /// Get/set the requested kvar value. Final kvar is subjected to the inverter ratings. Sets inverter to operate in constant kvar mode.
        /// 
        double kvar() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Storages_Get_kvar(ctx);
        }
        IStorages& kvar(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Storages_Set_kvar(ctx, value);
            return *this;
        }

        /// 
        /// Rated voltage (kV) at the input of the inverter while the storage is discharging
        /// 
        double kVDC() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Storages_Get_kVDC(ctx);
        }
        IStorages& kVDC(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Storages_Set_kVDC(ctx, value);
            return *this;
        }

        /// 
        /// Get/set the requested kW value. Final kW is subjected to the inverter ratings.
        /// 
        double kW() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Storages_Get_kW(ctx);
        }
        IStorages& kW(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Storages_Set_kW(ctx, value);
            return *this;
        }

        /// 
        /// Rated Storage capacity in kWh.
        /// 
        double kWhRated() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Storages_Get_kWhRated(ctx);
        }
        IStorages& kWhRated(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Storages_Set_kWhRated(ctx, value);
            return *this;
        }

        /// 
        /// kW rating of power output. Base for Loadshapes when DispMode=Follow. Sets kVA property if it has not been specified yet.
        /// 
        double kWRated() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Storages_Get_kWRated(ctx);
        }
        IStorages& kWRated(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Storages_Set_kWRated(ctx, value);
            return *this;
        }

        /// 
        /// Limits current magnitude to Vminpu value for both 1-phase and 3-phase Storage similar to Generator Model 7.
        /// For 3-phase, limits the positive-sequence current but not the negative-sequence."
        /// 
        bool LimitCurrent() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Storages_Get_LimitCurrent(ctx);
        }
        IStorages& LimitCurrent(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Storages_Set_LimitCurrent(ctx, value);
            return *this;
        }

        /// 
        /// Get/set the requested PF value.
        /// 
        double PF() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Storages_Get_PF(ctx);
        }
        IStorages& PF(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Storages_Set_PF(ctx, value);
            return *this;
        }

        /// 
        /// Tolerance (%) for the closed loop controller of the inverter
        /// 
        double PITol() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Storages_Get_PITol(ctx);
        }
        IStorages& PITol(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Storages_Set_PITol(ctx, value);
            return *this;
        }

        /// 
        /// (Read only) Indicates whether the inverter entered (Yes) or not (No) into Safe Mode.
        /// 
        int32_t SafeMode() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Storages_Get_SafeMode(ctx);
        }

        /// 
        /// Indicates the voltage level (%) respect to the base voltage level for which the Inverter will operate.
        /// 
        double SafeVoltage() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Storages_Get_SafeVoltage(ctx);
        }
        IStorages& SafeVoltage(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Storages_Set_SafeVoltage(ctx, value);
            return *this;
        }

        /// 
        /// Time of day in fractional hours (0230 = 2.5) at which Storage element will automatically go into charge state.
        /// 
        double TimeChargeTrig() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Storages_Get_TimeChargeTrig(ctx);
        }
        IStorages& TimeChargeTrig(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Storages_Set_TimeChargeTrig(ctx, value);
            return *this;
        }

        /// 
        /// Indicates if the reactive power generation/absorption does not respect the inverter status
        /// 
        int32_t VarFollowInverter() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Storages_Get_VarFollowInverter(ctx);
        }
        IStorages& VarFollowInverter(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Storages_Set_VarFollowInverter(ctx, value);
            return *this;
        }
    };

    class IWindGens: public ContextState
    {
    public:

        IWindGens(altdss::APIUtil *util) :
            ContextState(util)
        {
        }

        ///
        /// Number of WindGen objects in active circuit.
        ///
        int32_t Count() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WindGens_Get_Count(ctx);
        }

        ///
        /// Sets the first WindGen active. Returns 0 if no more.
        ///
        int32_t First() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WindGens_Get_First(ctx);
        }

        ///
        /// Get the name of the current active WindGen
        ///
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WindGens_Get_Name(ctx);
        }

        ///
        /// Sets the active WindGen by Name.
        ///
        IWindGens& Name(const char *value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WindGens_Set_Name(ctx, value);
            return *this;
        }
        IWindGens& Name(const string &value)
        {
            return Name(value.c_str());
        }

        ///
        /// Sets the next WindGen active. Returns 0 if no more.
        ///
        int32_t Next() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WindGens_Get_Next(ctx);
        }

        ///
        /// Get active WindGen by index; index is 1-based: 1..count
        ///
        int32_t idx() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WindGens_Get_idx(ctx);
        }

        ///
        /// Get active WindGen by index; index is 1-based: 1..count
        ///
        void idx(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WindGens_Set_idx(ctx, value);
        }

        /// 
        /// Nominal rated (1.0 per unit) voltage for the active WindGen, in kV.
        /// 
        double kV() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WindGens_Get_kV(ctx);
        }
        IWindGens& kV(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WindGens_Set_kV(ctx, value);
            return *this;
        }

        /// 
        /// Base kvar for the active WindGen.
        /// 
        double kvar() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WindGens_Get_kvar(ctx);
        }
        IWindGens& kvar(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WindGens_Set_kvar(ctx, value);
            return *this;
        }

        /// 
        /// Total base kW for the active WindGen.
        /// 
        double kW() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WindGens_Get_kW(ctx);
        }
        IWindGens& kW(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WindGens_Set_kW(ctx, value);
            return *this;
        }

        /// 
        /// WindGen power factor. Power factor (pos. = producing vars).
        /// 
        double PF() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WindGens_Get_PF(ctx);
        }
        IWindGens& PF(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WindGens_Set_PF(ctx, value);
            return *this;
        }

        /// 
        /// KVA rating of the electrical machine in the WindGen.
        /// 
        double kVA() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WindGens_Get_kVA(ctx);
        }
        IWindGens& kVA(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WindGens_Set_kVA(ctx, value);
            return *this;
        }

        /// 
        /// Gearbox ratio
        /// 
        double Ag() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WindGens_Get_Ag(ctx);
        }
        IWindGens& Ag(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WindGens_Set_Ag(ctx, value);
            return *this;
        }

        /// 
        /// Turbine performance coefficient.
        /// 
        double Cp() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WindGens_Get_Cp(ctx);
        }
        IWindGens& Cp(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WindGens_Set_Cp(ctx, value);
            return *this;
        }

        /// 
        /// Tip speed ratio
        /// 
        double Lamda() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WindGens_Get_Lamda(ctx);
        }
        IWindGens& Lamda(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WindGens_Set_Lamda(ctx, value);
            return *this;
        }

        /// 
        /// Number of WTG in aggregation
        /// 
        int32_t N_WTG() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WindGens_Get_N_WTG(ctx);
        }
        IWindGens& N_WTG(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WindGens_Set_N_WTG(ctx, value);
            return *this;
        }

        /// 
        /// Number of pole pairs of the induction generator
        /// 
        int32_t NPoles() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WindGens_Get_NPoles(ctx);
        }
        IWindGens& NPoles(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WindGens_Set_NPoles(ctx, value);
            return *this;
        }

        /// 
        /// Air density in kg/m3
        /// 
        double pd() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WindGens_Get_pd(ctx);
        }
        IWindGens& pd(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WindGens_Set_pd(ctx, value);
            return *this;
        }

        /// 
        /// Steady state output real power.
        /// 
        double PSS() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WindGens_Get_PSS(ctx);
        }
        IWindGens& PSS(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WindGens_Set_PSS(ctx, value);
            return *this;
        }

        /// 
        /// Non-zero values enable reactive power and voltage control in the dynamic model.
        /// 
        int32_t QFlag() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WindGens_Get_QFlag(ctx);
        }
        IWindGens& QFlag(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WindGens_Set_QFlag(ctx, value);
            return *this;
        }

        /// 
        /// Q control mode (0:Q, 1:PF, 2:VV).
        /// 
        int32_t QMode() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WindGens_Get_QMode(ctx);
        }
        IWindGens& QMode(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WindGens_Set_QMode(ctx, value);
            return *this;
        }

        /// 
        /// Steady state output reactive power.
        /// 
        double QSS() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WindGens_Get_QSS(ctx);
        }
        IWindGens& QSS(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WindGens_Set_QSS(ctx, value);
            return *this;
        }

        /// 
        /// Rotor radius in meters
        /// 
        double Rad() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WindGens_Get_Rad(ctx);
        }
        IWindGens& Rad(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WindGens_Set_Rad(ctx, value);
            return *this;
        }

        /// 
        /// Per unit Thevenin equivalent resistance (R).
        /// 
        double RThev() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WindGens_Get_RThev(ctx);
        }
        IWindGens& RThev(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WindGens_Set_RThev(ctx, value);
            return *this;
        }

        /// 
        /// Cut-in speed for the wind generator
        /// 
        double VCutIn() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WindGens_Get_VCutIn(ctx);
        }
        IWindGens& VCutIn(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WindGens_Set_VCutIn(ctx, value);
            return *this;
        }

        /// 
        /// Cut-out speed for the wind generator
        /// 
        double VCutOut() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WindGens_Get_VCutOut(ctx);
        }
        IWindGens& VCutOut(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WindGens_Set_VCutOut(ctx, value);
            return *this;
        }

        /// 
        /// Steady state voltage magnitude.
        /// 
        double Vss() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WindGens_Get_Vss(ctx);
        }
        IWindGens& Vss(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WindGens_Set_Vss(ctx, value);
            return *this;
        }

        /// 
        /// Wind speed in m/s
        /// 
        double WindSpeed() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WindGens_Get_WindSpeed(ctx);
        }
        IWindGens& WindSpeed(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WindGens_Set_WindSpeed(ctx, value);
            return *this;
        }

        /// 
        /// Per unit Thevenin equivalent reactance (X).
        /// 
        double XThev() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WindGens_Get_XThev(ctx);
        }
        IWindGens& XThev(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WindGens_Set_XThev(ctx, value);
            return *this;
        }

        /// 
        /// Number of phases
        /// 
        /// (API Extension)
        /// 
        int32_t Phases() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WindGens_Get_Phases(ctx);
        }
        IWindGens& Phases(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WindGens_Set_Phases(ctx, value);
            return *this;
        }

        /// 
        /// Name of the loadshape for daily wind speed
        /// 
        /// (API Extension)
        /// 
        string daily() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WindGens_Get_daily(ctx);
        }
        IWindGens& daily(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WindGens_Set_daily(ctx, value);
            return *this;
        }
        IWindGens& daily(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WindGens_Set_daily(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Name of the loadshape for a duty cycle simulation.
        /// 
        /// (API Extension)
        /// 
        string duty() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WindGens_Get_duty(ctx);
        }
        IWindGens& duty(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WindGens_Set_duty(ctx, value);
            return *this;
        }
        IWindGens& duty(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WindGens_Set_duty(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Name of yearly loadshape
        /// 
        /// (API Extension)
        /// 
        string Yearly() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WindGens_Get_Yearly(ctx);
        }
        IWindGens& Yearly(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WindGens_Set_Yearly(ctx, value);
            return *this;
        }
        IWindGens& Yearly(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WindGens_Set_Yearly(ctx, value.c_str());
            return *this;
        }

        /// 
        /// WindGen connection. True/1 if delta connection, False/0 if wye.
        /// 
        /// (API Extension)
        /// 
        bool IsDelta() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WindGens_Get_IsDelta(ctx);
        }
        IWindGens& IsDelta(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WindGens_Set_IsDelta(ctx, value);
            return *this;
        }

        /// 
        /// An arbitrary integer number representing the class of WindGen so that WindGen values may be segregated by class.
        /// 
        /// (API Extension)
        /// 
        int32_t Class() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WindGens_Get_Class_(ctx);
        }
        IWindGens& Class(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WindGens_Set_Class_(ctx, value);
            return *this;
        }

        /// 
        /// Bus to which the WindGen is connected. May include specific node specification.
        /// 
        /// (API Extension)
        /// 
        string Bus1() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WindGens_Get_Bus1(ctx);
        }
        IWindGens& Bus1(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WindGens_Set_Bus1(ctx, value);
            return *this;
        }
        IWindGens& Bus1(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WindGens_Set_Bus1(ctx, value.c_str());
            return *this;
        }
    };

    class ISolution: public ContextState
    {
    public:

        ISolution(altdss::APIUtil *util) :
            ContextState(util)
        {
        }
        void BuildYMatrix(int32_t BuildOption, int32_t AllocateVI)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_BuildYMatrix(ctx, BuildOption, AllocateVI);
        }
        void CheckControls()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_CheckControls(ctx);
        }
        void CheckFaultStatus()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_CheckFaultStatus(ctx);
        }
        void Cleanup()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Cleanup(ctx);
        }
        void DoControlActions()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_DoControlActions(ctx);
        }
        void FinishTimeStep()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_FinishTimeStep(ctx);
        }
        void InitSnap()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_InitSnap(ctx);
        }
        void SampleControlDevices()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_SampleControlDevices(ctx);
        }
        void Sample_DoControlActions()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Sample_DoControlActions(ctx);
        }
        void Solve()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Solve(ctx);
        }
        void SolveDirect()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_SolveDirect(ctx);
        }
        void SolveNoControl()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_SolveNoControl(ctx);
        }
        void SolvePflow()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_SolvePflow(ctx);
        }
        void SolvePlusControl()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_SolvePlusControl(ctx);
        }
        void SolveSnap()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_SolveSnap(ctx);
        }

        /// 
        /// Type of device to add in AutoAdd Mode: {dssGen (Default) | dssCap}
        /// 
        /// Original COM help: https://opendss.epri.com/AddType.html
        /// 
        int32_t AddType() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_AddType(ctx);
        }
        ISolution& AddType(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_AddType(ctx, value);
            return *this;
        }

        /// 
        /// Base Solution algorithm
        /// 
        /// Original COM help: https://opendss.epri.com/Algorithm.html
        /// 
        SolutionAlgorithms Algorithm() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return static_cast<SolutionAlgorithms>(ctx_Solution_Get_Algorithm(ctx));
        }
        ISolution& Algorithm(SolutionAlgorithms value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_Algorithm(ctx, static_cast<int32_t>(value));
            return *this;
        }

        /// 
        /// Capacitor kvar for adding capacitors in AutoAdd mode
        /// 
        /// Original COM help: https://opendss.epri.com/Capkvar.html
        /// 
        double Capkvar() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_Capkvar(ctx);
        }
        ISolution& Capkvar(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_Capkvar(ctx, value);
            return *this;
        }

        /// 
        /// Flag indicating the control actions are done.
        /// 
        /// Original COM help: https://opendss.epri.com/ControlActionsDone.html
        /// 
        bool ControlActionsDone() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_ControlActionsDone(ctx);
        }
        ISolution& ControlActionsDone(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_ControlActionsDone(ctx, value);
            return *this;
        }

        /// 
        /// Value of the control iteration counter
        /// 
        /// Original COM help: https://opendss.epri.com/ControlIterations.html
        /// 
        int32_t ControlIterations() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_ControlIterations(ctx);
        }
        ISolution& ControlIterations(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_ControlIterations(ctx, value);
            return *this;
        }

        /// 
        /// Modes for control devices
        /// 
        /// Original COM help: https://opendss.epri.com/ControlMode.html
        /// 
        ControlModes ControlMode() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return static_cast<ControlModes>(ctx_Solution_Get_ControlMode(ctx));
        }
        ISolution& ControlMode(ControlModes value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_ControlMode(ctx, static_cast<int32_t>(value));
            return *this;
        }

        /// 
        /// Flag to indicate whether the circuit solution converged
        /// 
        /// Original COM help: https://opendss.epri.com/Converged.html
        /// 
        bool Converged() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_Converged(ctx);
        }
        ISolution& Converged(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_Converged(ctx, value);
            return *this;
        }

        /// 
        /// Default daily load shape (defaults to "Default")
        /// 
        /// Original COM help: https://opendss.epri.com/DefaultDaily.html
        /// 
        string DefaultDaily() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_DefaultDaily(ctx);
        }
        ISolution& DefaultDaily(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_DefaultDaily(ctx, value);
            return *this;
        }
        ISolution& DefaultDaily(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_DefaultDaily(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Default Yearly load shape (defaults to "Default")
        /// 
        /// Original COM help: https://opendss.epri.com/DefaultYearly.html
        /// 
        string DefaultYearly() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_DefaultYearly(ctx);
        }
        ISolution& DefaultYearly(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_DefaultYearly(ctx, value);
            return *this;
        }
        ISolution& DefaultYearly(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_DefaultYearly(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Array of strings containing the Event Log
        /// 
        /// Original COM help: https://opendss.epri.com/EventLog.html
        /// 
        strings EventLog() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Solution_Get_EventLog);
        }

        /// 
        /// Set the Frequency for next solution
        /// 
        /// Original COM help: https://opendss.epri.com/Frequency1.html
        /// 
        double Frequency() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_Frequency(ctx);
        }
        ISolution& Frequency(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_Frequency(ctx, value);
            return *this;
        }

        /// 
        /// Default Multiplier applied to generators (like LoadMult)
        /// 
        /// Original COM help: https://opendss.epri.com/GenMult.html
        /// 
        double GenMult() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_GenMult(ctx);
        }
        ISolution& GenMult(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_GenMult(ctx, value);
            return *this;
        }

        /// 
        /// PF for generators in AutoAdd mode
        /// 
        /// Original COM help: https://opendss.epri.com/GenPF.html
        /// 
        double GenPF() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_GenPF(ctx);
        }
        ISolution& GenPF(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_GenPF(ctx, value);
            return *this;
        }

        /// 
        /// Generator kW for AutoAdd mode
        /// 
        /// Original COM help: https://opendss.epri.com/GenkW.html
        /// 
        double GenkW() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_GenkW(ctx);
        }
        ISolution& GenkW(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_GenkW(ctx, value);
            return *this;
        }

        /// 
        /// Set Hour for time series solutions.
        /// 
        /// Original COM help: https://opendss.epri.com/Hour.html
        /// 
        int32_t Hour() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_Hour(ctx);
        }
        ISolution& Hour(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_Hour(ctx, value);
            return *this;
        }

        /// 
        /// Get/Set the Solution.IntervalHrs variable used for devices that integrate / custom solution algorithms
        /// 
        double IntervalHrs() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_IntervalHrs(ctx);
        }
        ISolution& IntervalHrs(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_IntervalHrs(ctx, value);
            return *this;
        }

        /// 
        /// Number of iterations taken for last solution. (Same as Totaliterations)
        /// 
        /// Original COM help: https://opendss.epri.com/Iterations.html
        /// 
        int32_t Iterations() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_Iterations(ctx);
        }

        /// 
        /// Load-Duration Curve name for LD modes
        /// 
        /// Original COM help: https://opendss.epri.com/LDCurve.html
        /// 
        string LDCurve() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_LDCurve(ctx);
        }
        ISolution& LDCurve(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_LDCurve(ctx, value);
            return *this;
        }
        ISolution& LDCurve(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_LDCurve(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Load Model: {dssPowerFlow (default) | dssAdmittance}
        /// 
        /// Original COM help: https://opendss.epri.com/LoadModel.html
        /// 
        int32_t LoadModel() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_LoadModel(ctx);
        }
        ISolution& LoadModel(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_LoadModel(ctx, value);
            return *this;
        }

        /// 
        /// Default load multiplier applied to all non-fixed loads
        /// 
        /// Original COM help: https://opendss.epri.com/LoadMult.html
        /// 
        double LoadMult() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_LoadMult(ctx);
        }
        ISolution& LoadMult(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_LoadMult(ctx, value);
            return *this;
        }

        /// 
        /// Maximum allowable control iterations
        /// 
        /// Original COM help: https://opendss.epri.com/MaxControlIterations.html
        /// 
        int32_t MaxControlIterations() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_MaxControlIterations(ctx);
        }
        ISolution& MaxControlIterations(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_MaxControlIterations(ctx, value);
            return *this;
        }

        /// 
        /// Max allowable iterations.
        /// 
        /// Original COM help: https://opendss.epri.com/MaxIterations.html
        /// 
        int32_t MaxIterations() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_MaxIterations(ctx);
        }
        ISolution& MaxIterations(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_MaxIterations(ctx, value);
            return *this;
        }

        /// 
        /// Minimum number of iterations required for a power flow solution.
        /// 
        /// Original COM help: https://opendss.epri.com/MinIterations.html
        /// 
        int32_t MinIterations() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_MinIterations(ctx);
        }
        ISolution& MinIterations(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_MinIterations(ctx, value);
            return *this;
        }

        /// 
        /// Set present solution mode
        /// 
        /// Original COM help: https://opendss.epri.com/Mode2.html
        /// 
        SolveModes Mode() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return static_cast<SolveModes>(ctx_Solution_Get_Mode(ctx));
        }
        ISolution& Mode(SolveModes value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_Mode(ctx, static_cast<int32_t>(value));
            return *this;
        }

        /// 
        /// ID (text) of the present solution mode
        /// 
        /// Original COM help: https://opendss.epri.com/ModeID.html
        /// 
        string ModeID() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_ModeID(ctx);
        }

        /// 
        /// Max number of iterations required to converge at any control iteration of the most recent solution.
        /// 
        /// Original COM help: https://opendss.epri.com/MostIterationsDone.html
        /// 
        int32_t MostIterationsDone() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_MostIterationsDone(ctx);
        }

        /// 
        /// Number of solutions to perform for Monte Carlo and time series simulations
        /// 
        /// Original COM help: https://opendss.epri.com/Number1.html
        /// 
        int32_t Number() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_Number(ctx);
        }
        ISolution& Number(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_Number(ctx, value);
            return *this;
        }

        /// 
        /// Gets the time required to perform the latest solution (Read only)
        /// 
        /// Original COM help: https://opendss.epri.com/Process_Time.html
        /// 
        double Process_Time() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_Process_Time(ctx);
        }

        /// 
        /// Randomization mode for random variables "Gaussian" or "Uniform"
        /// 
        /// Original COM help: https://opendss.epri.com/Random.html
        /// 
        int32_t Random() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_Random(ctx);
        }
        ISolution& Random(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_Random(ctx, value);
            return *this;
        }

        /// 
        /// Seconds from top of the hour.
        /// 
        /// Original COM help: https://opendss.epri.com/Seconds.html
        /// 
        double Seconds() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_Seconds(ctx);
        }
        ISolution& Seconds(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_Seconds(ctx, value);
            return *this;
        }

        /// 
        /// Time step size in sec
        /// 
        /// Original COM help: https://opendss.epri.com/StepSize.html
        /// 
        double StepSize() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_StepSize(ctx);
        }
        ISolution& StepSize(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_StepSize(ctx, value);
            return *this;
        }

        /// 
        /// Flag that indicates if elements of the System Y have been changed by recent activity.
        /// 
        /// Original COM help: https://opendss.epri.com/SystemYChanged.html
        /// 
        bool SystemYChanged() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_SystemYChanged(ctx);
        }

        /// 
        /// Get the solution process time + sample time for time step
        /// 
        /// Original COM help: https://opendss.epri.com/Time_of_Step.html
        /// 
        double Time_of_Step() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_Time_of_Step(ctx);
        }

        /// 
        /// Solution convergence tolerance.
        /// 
        /// Original COM help: https://opendss.epri.com/Tolerance.html
        /// 
        double Tolerance() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_Tolerance(ctx);
        }
        ISolution& Tolerance(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_Tolerance(ctx, value);
            return *this;
        }

        /// 
        /// Gets/sets the accumulated time of the simulation
        /// 
        /// This accumulator has to be reset manually.
        /// 
        /// Original COM help: https://opendss.epri.com/Total_Time.html
        /// 
        double Total_Time() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_Total_Time(ctx);
        }
        ISolution& Total_Time(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_Total_Time(ctx, value);
            return *this;
        }

        /// 
        /// Total iterations including control iterations for most recent solution.
        /// 
        /// Original COM help: https://opendss.epri.com/Totaliterations.html
        /// 
        int32_t Totaliterations() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_Totaliterations(ctx);
        }

        /// 
        /// Set year for planning studies
        /// 
        /// Original COM help: https://opendss.epri.com/Year.html
        /// 
        int32_t Year() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_Year(ctx);
        }
        ISolution& Year(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_Year(ctx, value);
            return *this;
        }

        /// 
        /// Hour as a double, including fractional part
        /// 
        /// Original COM help: https://opendss.epri.com/dblHour1.html
        /// 
        double dblHour() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_dblHour(ctx);
        }
        ISolution& dblHour(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_dblHour(ctx, value);
            return *this;
        }

        /// 
        /// Percent default  annual load growth rate
        /// 
        /// Original COM help: https://opendss.epri.com/pctGrowth.html
        /// 
        double pctGrowth() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_pctGrowth(ctx);
        }
        ISolution& pctGrowth(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_pctGrowth(ctx, value);
            return *this;
        }

        /// 
        /// (write-only) Set Stepsize in Hr
        /// 
        ISolution& StepsizeHr(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_StepsizeHr(ctx, value);
            return *this;
        }

        /// 
        /// (write-only) Set Stepsize in minutes
        /// 
        ISolution& StepsizeMin(double value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_StepsizeMin(ctx, value);
            return *this;
        }

        /// 
        /// Bus levels for all the buses in the model. 
        /// 
        /// The bus levels are calculated after calculating the incidence branch-to-node (B2N) 
        /// matrix and they represent the distance from the buses to a reference that goes from
        /// the feeder head to the farthest bus in the model. The bus level index matches with
        /// the bus list obtained with the circuit interface.
        /// 
        /// Original COM help: https://opendss.epri.com/BusLevels.html
        /// 
        template <typename VectorT=Eigen::Matrix<int32_t, Eigen::Dynamic, 1>>
        VectorT BusLevels() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Get_BusLevels_GR(ctx);
            return api_util->get_int32_gr_array<VectorT>();
        }

        /// 
        /// Incidence branch-to-node (B2N) matrix calculated for the model as a vector of integers.
        /// 
        /// The vector represents a sparse matrix (non-zero values are the only ones delivered) and
        /// can be interpreted as follows: The first element is the row number, the second one is
        /// the column and the third is the value, this way, by dividing the number of elements
        /// in the array by 3 the user can obtain the number of rows in case of wanting to sort 
        /// the vector values within a matrix.
        /// 
        /// Original COM help: https://opendss.epri.com/IncMatrix.html
        /// 
        template <typename VectorT=Eigen::Matrix<int32_t, Eigen::Dynamic, 1>>
        VectorT IncMatrix() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Get_IncMatrix_GR(ctx);
            return api_util->get_int32_gr_array<VectorT>();
        }

        /// 
        /// Names of the columns of the branch-to-node (B2N) matrix.
        /// 
        /// Original COM help: https://opendss.epri.com/IncMatrixCols.html
        /// 
        strings IncMatrixCols() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Solution_Get_IncMatrixCols);
        }

        /// 
        /// Names of the rows of the branch-to-node (B2N) matrix.
        /// 
        /// Original COM help: https://opendss.epri.com/IncMatrixRows.html
        /// 
        strings IncMatrixRows() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Solution_Get_IncMatrixRows);
        }

        /// 
        /// Laplacian matrix calculated in OpenDSS based on the latest branch-to-node (B2N) matrix.
        /// 
        /// The vector represents a sparse matrix (non-zero values are the only ones delivered) and
        /// can be interpreted as follows: The first element is the row number, the second one is
        /// the column and the third is the value, this way, by dividing the number of elements
        /// in the array by 3 the user can obtain the number of rows in case of wanting to sort
        /// the vector values within a matrix. The tables for the columns and rows are the same
        /// as the columns for the B2N columns (square matrix).        
        /// 
        /// Original COM help: https://opendss.epri.com/Laplacian.html
        /// 
        template <typename VectorT=Eigen::Matrix<int32_t, Eigen::Dynamic, 1>>
        VectorT Laplacian() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Get_Laplacian_GR(ctx);
            return api_util->get_int32_gr_array<VectorT>();
        }
    };

    class ICircuit: public ContextState
    {
    public:
        IBus Buses;
        ICktElement CktElements;
        ICktElement ActiveElement;
        ISolution Solution;
        IBus ActiveBus;
        IGenerators Generators;
        IMeters Meters;
        IMonitors Monitors;
        ISettings Settings;
        ILines Lines;
        ICtrlQueue CtrlQueue;
        ILoads Loads;
        ICktElement ActiveCktElement;
        IDSSElement ActiveDSSElement;
        IActiveClass ActiveClass;
        ICapControls CapControls;
        IRegControls RegControls;
        ISwtControls SwtControls;
        ITransformers Transformers;
        ICapacitors Capacitors;
        ITopology Topology;
        ISensors Sensors;
        IXYCurves XYCurves;
        IPDElements PDElements;
        IReclosers Reclosers;
        IRelays Relays;
        ILoadShapes LoadShapes;
        IFuses Fuses;
        IISources Isources;
        IDSSimComs DSSim_Coms;
        IPVSystems PVSystems;
        IVsources Vsources;
        ILineCodes LineCodes;
        ILineGeometries LineGeometries;
        ILineSpacings LineSpacings;
        IWireData WireData;
        ICNData CNData;
        ITSData TSData;
        IReactors Reactors;
        IReduceCkt ReduceCkt;
        IStorages Storages;
        IGICSources GICSources;
        IWindGens WindGens;
        IParallel Parallel;

        ICircuit(altdss::APIUtil *util) :
            ContextState(util),
            Buses(util),
            CktElements(util),
            ActiveElement(util),
            Solution(util),
            ActiveBus(util),
            Generators(util),
            Meters(util),
            Monitors(util),
            Settings(util),
            Lines(util),
            CtrlQueue(util),
            Loads(util),
            ActiveCktElement(util),
            ActiveDSSElement(util),
            ActiveClass(util),
            CapControls(util),
            RegControls(util),
            SwtControls(util),
            Transformers(util),
            Capacitors(util),
            Topology(util),
            Sensors(util),
            XYCurves(util),
            PDElements(util),
            Reclosers(util),
            Relays(util),
            LoadShapes(util),
            Fuses(util),
            Isources(util),
            DSSim_Coms(util),
            PVSystems(util),
            Vsources(util),
            LineCodes(util),
            LineGeometries(util),
            LineSpacings(util),
            WireData(util),
            CNData(util),
            TSData(util),
            Reactors(util),
            ReduceCkt(util),
            Storages(util),
            GICSources(util),
            WindGens(util),
            Parallel(util)
        {
        }
    
        ///
        /// Activates and returns a bus by its (zero-based) index.
        ///
        IBus& get_Buses(int32_t idx)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            if (ctx_Circuit_SetActiveBusi(ctx, idx) < 0)
                throw std::runtime_error("Invalid bus");
        
            return ActiveBus;
        }

        ///
        /// Activates and returns a bus by its name.
        ///
        IBus& get_Buses(const char *name)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            if (ctx_Circuit_SetActiveBus(ctx, name) < 0)
                throw std::runtime_error("Invalid bus");

            return ActiveBus;
        }
        IBus& get_Buses(const string &name)
        {
            return get_Buses(name.c_str());
        }

        ///
        /// Activates and returns a CktElement by its global (zero-based) index.
        ///
        ICktElement get_CktElements(int32_t idx)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_SetCktElementIndex(ctx, idx);
            return ActiveCktElement;
        }

        ///
        /// Activates and returns a CktElement by its full name (e.g. "load.abc").
        ///
        ICktElement get_CktElements(const char *fullName)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_SetCktElementName(ctx, fullName);
            return ActiveCktElement;
        }
        ICktElement get_CktElements(const string &fullName)
        {
            return get_CktElements(fullName.c_str());
        }

        /// 
        /// Replaces the circuit, if any, with the one provided from a JSON-encoded string.
        /// 
        /// The expected layout is defined from the JSON schema proposed at
        /// https://github.com/dss-extensions/AltDSS-Schema
        /// 
        /// The `options` parameter contains bit-flags to toggle specific features.
        /// See the enum `DSSJSONFlags`.
        /// 
        /// **(API Extension)**
        /// 
        void FromJSON(const char *data, DSSJSONFlags options=static_cast<DSSJSONFlags>(0))
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_FromJSON(ctx, data, options);
        }
        void FromJSON(const string &data, DSSJSONFlags options=static_cast<DSSJSONFlags>(0))
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_FromJSON(ctx, data.c_str(), options);
        }
    
        /// 
        /// Compute the maximum load the active circuit can serve in the PRESENT YEAR.
        /// 
        /// This method uses the EnergyMeter objects with the registers set with the 
        /// `SET UEREGS= (...)` command for the AutoAdd functions. 
        /// 
        /// Returns the metered kW (load + losses - generation) and per unit load multiplier 
        /// for the loading level at which something in the system reports an overload or 
        /// undervoltage. If no violations, then it returns the metered kW for peak load 
        /// for the year (1.0 multiplier). 
        /// 
        /// Aborts and returns 0 if no EnergyMeters.
        /// 
        /// Original COM help: https://opendss.epri.com/Capacity1.html
        /// 
        double Capacity(double Start, double Increment)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_Capacity(ctx, Start, Increment);
        }
        /// 
        /// Disable a circuit element by name (removes from circuit but leave in database).
        /// 
        /// Original COM help: https://opendss.epri.com/Disable.html
        /// 
        void Disable(const char *Name)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_Disable(ctx, Name);
        }
        /// 
        /// Disable a circuit element by name (removes from circuit but leave in database).
        /// 
        /// Original COM help: https://opendss.epri.com/Disable.html
        /// 
        void Disable(const string &Name)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_Disable(ctx, Name.c_str());
        }
        /// 
        /// Enable a circuit element by name
        /// 
        /// Original COM help: https://opendss.epri.com/Enable.html
        /// 
        void Enable(const char *Name)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_Enable(ctx, Name);
        }
        /// 
        /// Enable a circuit element by name
        /// 
        /// Original COM help: https://opendss.epri.com/Enable.html
        /// 
        void Enable(const string &Name)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_Enable(ctx, Name.c_str());
        }
        /// 
        /// Call `EndOfTimeStepCleanup` in SolutionAlgs (Do cleanup, sample monitors, and increment time).
        /// 
        /// Original COM help: https://opendss.epri.com/EndOfTimeStepUpdate.html
        /// 
        void EndOfTimeStepUpdate()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_EndOfTimeStepUpdate(ctx);
        }
        /// 
        /// Set the first element of active class to be the Active element in the active circuit.
        /// 
        /// Returns 0 if none.
        /// 
        /// Original COM help: https://opendss.epri.com/FirstElement.html
        /// 
        int32_t FirstElement()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_FirstElement(ctx);
        }
        /// 
        /// Set the first Power Conversion (PC) element to be the active element.
        /// 
        /// Returns 0 if none.
        /// 
        /// Original COM help: https://opendss.epri.com/FirstPCElement.html
        /// 
        int32_t FirstPCElement()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_FirstPCElement(ctx);
        }
        /// 
        /// Set the first Power Delivery (PD) element to be the active element.
        /// 
        /// Returns 0 if none.
        /// 
        /// Original COM help: https://opendss.epri.com/FirstPDElement.html
        /// 
        int32_t FirstPDElement()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_FirstPDElement(ctx);
        }
        /// 
        /// Returns an array of doubles representing the distances to parent EnergyMeter. Sequence of array corresponds to other node ByPhase properties.
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT AllNodeDistancesByPhase(int32_t Phase)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_Get_AllNodeDistancesByPhase_GR(ctx, Phase);
            return api_util->get_float64_gr_array<VectorT>();
        }
        /// 
        /// Return array of strings of the node names for the By Phase criteria. Sequence corresponds to other ByPhase properties.
        /// 
        strings AllNodeNamesByPhase(int32_t Phase)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Circuit_Get_AllNodeNamesByPhase, Phase);
        }
        /// 
        /// Returns Array of doubles represent voltage magnitudes for nodes on the specified phase.
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT AllNodeVmagByPhase(int32_t Phase)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_Get_AllNodeVmagByPhase_GR(ctx, Phase);
            return api_util->get_float64_gr_array<VectorT>();
        }
        /// 
        /// Returns array of per unit voltage magnitudes for each node by phase
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT AllNodeVmagPUByPhase(int32_t Phase)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_Get_AllNodeVmagPUByPhase_GR(ctx, Phase);
            return api_util->get_float64_gr_array<VectorT>();
        }
        /// 
        /// Set the next element of the active class to be the active element in the active circuit.
        /// Returns 0 if no more elements..
        /// 
        /// Original COM help: https://opendss.epri.com/NextElement.html
        /// 
        int32_t NextElement()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_NextElement(ctx);
        }
        /// 
        /// Get the next Power Conversion (PC) element to be the active element.
        /// 
        /// Original COM help: https://opendss.epri.com/NextPCElement.html
        /// 
        int32_t NextPCElement()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_NextPCElement(ctx);
        }
        /// 
        /// Get the next Power Delivery (PD) element to be the active element.
        /// 
        /// Original COM help: https://opendss.epri.com/NextPDElement.html
        /// 
        int32_t NextPDElement()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_NextPDElement(ctx);
        }
        /// 
        /// Force all Meters and Monitors to take a sample.
        /// 
        /// Original COM help: https://opendss.epri.com/Sample.html
        /// 
        void Sample()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_Sample(ctx);
        }
        /// 
        /// Force all meters and monitors to save their current buffers.
        /// 
        /// Original COM help: https://opendss.epri.com/SaveSample.html
        /// 
        void SaveSample()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_SaveSample(ctx);
        }
        /// 
        /// Sets Active bus by name. 
        /// 
        /// Ignores node list. Returns bus index (zero based) compatible with `AllBusNames` and Buses collection.
        /// 
        /// Original COM help: https://opendss.epri.com/SetActiveBus.html
        /// 
        int32_t SetActiveBus(const char *BusName)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_SetActiveBus(ctx, BusName);
        }
        /// 
        /// Sets Active bus by name. 
        /// 
        /// Ignores node list. Returns bus index (zero based) compatible with `AllBusNames` and Buses collection.
        /// 
        /// Original COM help: https://opendss.epri.com/SetActiveBus.html
        /// 
        int32_t SetActiveBus(const string &BusName)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_SetActiveBus(ctx, BusName.c_str());
        }
        /// 
        /// Set ActiveBus by an integer value. 
        /// 
        /// 0-based index compatible with SetActiveBus return value and AllBusNames indexing. 
        /// Returns 0 if OK.
        /// 
        /// Original COM help: https://opendss.epri.com/SetActiveBusi.html
        /// 
        int32_t SetActiveBusi(int32_t BusIndex)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_SetActiveBusi(ctx, BusIndex);
        }
        /// 
        /// Set the active class by name. 
        /// 
        /// Use FirstElement, NextElement to iterate through the class. Returns -1 if fails.
        /// 
        /// Original COM help: https://opendss.epri.com/SetActiveClass.html
        /// 
        int32_t SetActiveClass(const char *ClassName)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_SetActiveClass(ctx, ClassName);
        }
        /// 
        /// Set the active class by name. 
        /// 
        /// Use FirstElement, NextElement to iterate through the class. Returns -1 if fails.
        /// 
        /// Original COM help: https://opendss.epri.com/SetActiveClass.html
        /// 
        int32_t SetActiveClass(const string &ClassName)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_SetActiveClass(ctx, ClassName.c_str());
        }
        /// 
        /// Set the Active Circuit Element using the full object name (e.g. "generator.g1"). 
        /// 
        /// Returns -1 if not found. Else index to be used in CktElements collection or `AllElementNames`.
        /// 
        /// Original COM help: https://opendss.epri.com/SetActiveElement.html
        /// 
        int32_t SetActiveElement(const char *FullName)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_SetActiveElement(ctx, FullName);
        }
        /// 
        /// Set the Active Circuit Element using the full object name (e.g. "generator.g1"). 
        /// 
        /// Returns -1 if not found. Else index to be used in CktElements collection or `AllElementNames`.
        /// 
        /// Original COM help: https://opendss.epri.com/SetActiveElement.html
        /// 
        int32_t SetActiveElement(const string &FullName)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_SetActiveElement(ctx, FullName.c_str());
        }
        /// 
        /// Force an update to all storage classes. 
        /// 
        /// Typically done after a solution. Done automatically in intrinsic solution modes.
        /// 
        /// Original COM help: https://opendss.epri.com/UpdateStorage.html
        /// 
        void UpdateStorage()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_UpdateStorage(ctx);
        }

        /// 
        /// Returns distance from each bus to parent EnergyMeter. Corresponds to sequence in AllBusNames.
        /// 
        /// Original COM help: https://opendss.epri.com/AllBusDistances.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT AllBusDistances() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_Get_AllBusDistances_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Array of strings containing names of all buses in circuit (see AllNodeNames).
        /// 
        /// Original COM help: https://opendss.epri.com/AllBusNames.html
        /// 
        strings AllBusNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Circuit_Get_AllBusNames);
        }

        /// 
        /// Array of magnitudes (doubles) of voltages at all buses
        /// 
        /// Original COM help: https://opendss.epri.com/AllBusVmag.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT AllBusVmag() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_Get_AllBusVmag_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Array of all bus voltages (each node) magnitudes in Per unit
        /// 
        /// Original COM help: https://opendss.epri.com/AllBusVmagPu.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT AllBusVmagPu() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_Get_AllBusVmagPu_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Complex array of all bus, node voltages from most recent solution
        /// 
        /// Original COM help: https://opendss.epri.com/AllBusVolts.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT AllBusVolts() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_Get_AllBusVolts_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Array of total losses (complex) in each circuit element
        /// 
        /// Original COM help: https://opendss.epri.com/AllElementLosses.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT AllElementLosses() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_Get_AllElementLosses_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Array of strings containing Full Name of all elements.
        /// 
        /// Original COM help: https://opendss.epri.com/AllElementNames.html
        /// 
        strings AllElementNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Circuit_Get_AllElementNames);
        }

        /// 
        /// Returns an array of distances from parent EnergyMeter for each Node. Corresponds to AllBusVMag sequence.
        /// 
        /// Original COM help: https://opendss.epri.com/AllNodeDistances.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT AllNodeDistances() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_Get_AllNodeDistances_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Array of strings containing full name of each node in system in same order as returned by AllBusVolts, etc.
        /// 
        /// Original COM help: https://opendss.epri.com/AllNodeNames.html
        /// 
        strings AllNodeNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Circuit_Get_AllNodeNames);
        }

        /// 
        /// Complex total line losses in the circuit
        /// 
        /// Original COM help: https://opendss.epri.com/LineLosses.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT LineLosses() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_Get_LineLosses_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Total losses in active circuit, complex number (two-element array of double).
        /// 
        /// Original COM help: https://opendss.epri.com/Losses.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Losses() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_Get_Losses_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Name of the active circuit.
        /// 
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_Get_Name(ctx);
        }

        /// 
        /// Total number of Buses in the circuit.
        /// 
        /// Original COM help: https://opendss.epri.com/NumBuses.html
        /// 
        int32_t NumBuses() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_Get_NumBuses(ctx);
        }

        /// 
        /// Number of CktElements in the circuit.
        /// 
        /// Original COM help: https://opendss.epri.com/NumCktElements.html
        /// 
        int32_t NumCktElements() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_Get_NumCktElements(ctx);
        }

        /// 
        /// Total number of nodes in the circuit.
        /// 
        /// Original COM help: https://opendss.epri.com/NumNodes1.html
        /// 
        int32_t NumNodes() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_Get_NumNodes(ctx);
        }

        /// 
        /// Sets Parent PD element, if any, to be the active circuit element and returns index>0; Returns 0 if it fails or not applicable.
        /// 
        /// Original COM help: https://opendss.epri.com/ParentPDElement.html
        /// 
        int32_t ParentPDElement() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_Get_ParentPDElement(ctx);
        }

        /// 
        /// Complex losses in all transformers designated to substations.
        /// 
        /// Original COM help: https://opendss.epri.com/SubstationLosses.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT SubstationLosses() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_Get_SubstationLosses_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// System Y matrix (after a solution has been performed). 
        /// This is deprecated as it returns a dense matrix. Only use it for small systems.
        /// For large-scale systems, prefer YMatrix.GetCompressedYMatrix.
        /// 
        /// Original COM help: https://opendss.epri.com/SystemY.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT SystemY() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_Get_SystemY_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Total power (complex), kVA delivered to the circuit
        /// 
        /// Original COM help: https://opendss.epri.com/TotalPower.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT TotalPower() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_Get_TotalPower_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Array of doubles containing complex injection currents for the present solution. It is the "I" vector of I=YV
        /// 
        /// Original COM help: https://opendss.epri.com/YCurrents.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT YCurrents() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_Get_YCurrents_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Array of strings containing the names of the nodes in the same order as the Y matrix
        /// 
        /// Original COM help: https://opendss.epri.com/YNodeOrder.html
        /// 
        strings YNodeOrder() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Circuit_Get_YNodeOrder);
        }

        /// 
        /// Complex array of actual node voltages in same order as SystemY matrix.
        /// 
        /// Original COM help: https://opendss.epri.com/YNodeVarray.html
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT YNodeVarray() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_Get_YNodeVarray_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }
        /// 
        /// Array of total losses (complex) in a selection of elements.
        /// Use the element indices (starting at 1) as parameter.
        /// 
        /// **(API Extension)**
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>, typename InVectorT=Eigen::Matrix<int32_t, Eigen::Dynamic, 1>>
        VectorT ElementLosses(const InVectorT &value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_Get_ElementLosses_GR(ctx, &value[0], value.size());
            return api_util->get_float64_gr_array<VectorT>();
        }
        /// 
        /// Returns data for all objects and basic circuit properties as a JSON-encoded string.
        /// 
        /// The JSON data is organized using the JSON schema proposed at 
        /// https://github.com/dss-extensions/AltDSS-Schema
        /// 
        /// The `options` parameter contains bit-flags to toggle specific features.
        /// See the enum `DSSJSONFlags` or `Obj_ToJSON` (C-API) for more.
        /// 
        /// **(API Extension)**
        /// 
        string ToJSON(int32_t options=0)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_ToJSON(ctx, options);
        }
        /// 
        /// Equivalent of the "save circuit" DSS command, but allows customization
        /// through the `saveFlags` argument, which is a set of bit flags. 
        /// See the "DSSSaveFlags" enumeration for available flags:
        /// 
        /// - `CalcVoltageBases`: Include the command CalcVoltageBases.
        /// - `SetVoltageBases`: Include commands to set the voltage bases individually.
        /// - `IncludeOptions`: Include most of the options (from the Set/Get DSS commands).
        /// - `IncludeDisabled`: Include disabled circuit elements (and LoadShapes).
        /// - `ExcludeDefault`: Exclude default DSS items if they are not modified by the user.
        /// - `SingleFile`: Use a single file instead of a folder for output.
        /// - `KeepOrder`: Save the circuit elements in the order they were loaded in the active circuit. Guarantees better reproducibility, especially when the system is ill-conditioned. Requires "SingleFile" flag.
        /// - `ExcludeMeterZones`: Do not export meter zones (as "feeders") separately. Has no effect when using a single file.
        /// - `IsOpen`: Export commands to open terminals of elements.
        /// - `ToString`: to the result string. Requires "SingleFile" flag.
        /// 
        /// If `SingleFile` is enabled, the path argument (`dirOrFilePath`) is the file path,
        /// otherwise it is the folder path. For string output, the argument is not used.
        /// 
        /// **(API Extension)**
        /// 
        string Save(const char *dirOrFilePath, uint32_t saveFlags)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_Save(ctx, dirOrFilePath, saveFlags);
        }
        /// 
        /// Equivalent of the "save circuit" DSS command, but allows customization
        /// through the `saveFlags` argument, which is a set of bit flags. 
        /// See the "DSSSaveFlags" enumeration for available flags:
        /// 
        /// - `CalcVoltageBases`: Include the command CalcVoltageBases.
        /// - `SetVoltageBases`: Include commands to set the voltage bases individually.
        /// - `IncludeOptions`: Include most of the options (from the Set/Get DSS commands).
        /// - `IncludeDisabled`: Include disabled circuit elements (and LoadShapes).
        /// - `ExcludeDefault`: Exclude default DSS items if they are not modified by the user.
        /// - `SingleFile`: Use a single file instead of a folder for output.
        /// - `KeepOrder`: Save the circuit elements in the order they were loaded in the active circuit. Guarantees better reproducibility, especially when the system is ill-conditioned. Requires "SingleFile" flag.
        /// - `ExcludeMeterZones`: Do not export meter zones (as "feeders") separately. Has no effect when using a single file.
        /// - `IsOpen`: Export commands to open terminals of elements.
        /// - `ToString`: to the result string. Requires "SingleFile" flag.
        /// 
        /// If `SingleFile` is enabled, the path argument (`dirOrFilePath`) is the file path,
        /// otherwise it is the folder path. For string output, the argument is not used.
        /// 
        /// **(API Extension)**
        /// 
        string Save(const string &dirOrFilePath, uint32_t saveFlags)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string(ctx_Circuit_Save(ctx, dirOrFilePath.c_str(), saveFlags));
        }
        /// 
        /// Flatten the circuit
        /// 
        /// Flatten the circuit structures, removing any object of the following types:
        /// 
        /// - XfmrCode
        /// - LineCode
        /// - LineSpacing
        /// - LineGeometry
        /// - WireData
        /// - CNData
        /// - TSData
        /// 
        /// The general data from those objects is propagated to the referencing Line and Transformer objects,
        /// and the properties on the latter are updated to remove any references to the removed objects.
        /// 
        /// This is useful for some converting the DSS circuit to another format, without requiring the user to handle all 
        /// the types listed above. This, of course, results in some limitations since a lot of detail is removed. Numerically,
        /// a normal snapshot or daily solution should be the same before and after the flatten operation.
        /// 
        /// Available only on AltDSS.
        /// 
        /// **(API Extension)**
        /// 
        void Flatten()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_Flatten(ctx);
        }
    };

    class IDSS: public ContextState
    {
    public:
        ICircuit ActiveCircuit;
        ICircuit Circuits;
        IError Error;
        IText Text;
        IDSSProgress DSSProgress;
        IActiveClass ActiveClass;
        IDSS_Executive Executive;
        IParser Parser;
        IDSSimComs DSSim_Coms;
        IYMatrix YMatrix;
        IZIP ZIP;
        bool owns_util;

        IDSS(altdss::APIUtil *util, bool owns=false) :
            ContextState(util),
            ActiveCircuit(util),
            Circuits(util),
            Error(util),
            Text(util),
            DSSProgress(util),
            ActiveClass(util),
            Executive(util),
            Parser(util),
            DSSim_Coms(util),
            YMatrix(util),
            ZIP(util),
            owns_util(owns)
        {
        }
    
        ///
        /// Expose DSS without a constructor parameters for backwards compatibility.
        /// This contructors always exposes the prime/default instance of OpenDSS.
        ///
        IDSS(): IDSS(new APIUtil(ctx_Get_Prime()))
        {
        }

        /// Creates a new DSS engine context.
        /// A DSS Context encapsulates most of the global state of the original OpenDSS engine,
        /// allowing the user to create multiple instances in the same process. By creating contexts
        /// manually, the management of threads and potential issues should be handled by the user.
        ///
        /// (API Extension)
        static IDSS *NewContext()
        {
            APIUtil *new_api_util = new APIUtil(ctx_New());
            return new IDSS(new_api_util, true);
        }
    
        ~IDSS()
        {
            if (owns_util)
            {
                delete api_util;
            }
        }

        /// 
        /// Share general DSS objects from this AltDSS context to another.
        /// 
        /// **WARNING:** currently, the pointers are not tracked! The user must ensure this context
        /// and its objects are kept alive while other contexts require it.
        /// 
        /// Optionally, as a shortcut, the user can provide `skip_cmds` to be passed to the `Settings.SkipCommands` 
        /// and `skip_file_regexp` to be passed to `Settings.SkipFileRegExp`, in the second DSS context. 
        /// 
        /// *Note*: If the `clear` command is included in `Settings.SkipCommands`, the `DSS.ClearAll()` method can still be called
        /// and it will reset both skip settings.
        /// 
        /// ***EXPERIMENTAL***
        /// 
        /// **(API Extension)**
        /// 
        void ShareGeneral(IDSS* otherContext, const strings& skip_cmds={}, const string& std:skip_file_regexp="")
        {
            APIUtil::ErrorChecker error_checker(api_util);
            if ((api_util->_is_oddie || otherContext->api_util->_is_oddie))
            {
                throw std::runtime_error("Only AltDSS engine contexts can share data.");
            }
            ctx_ShareGeneral(ctx, otherContext.api_util->ctx);
            if (!skip_cmds.empty())
            {
                otherContext.ActiveCircuit.Settings.SkipCommands(skip_cmds);
            }
            if (!skip_file_regexp.empty())
            {
                otherContext.ActiveCircuit.Settings.SkipFileRegExp(skip_file_regexp);
            }
        }

    
        /// 
        /// Returns True if this instance is based on the Oddie compatibility layer for
        /// EPRI's OpenDSS Direct API (a.k.a. DCSL).
        /// 
        /// Note that the default engine in DSS-Python has been based on AltDSS since
        /// 2018, even though it was not called AltDSS then.
        /// 
        bool is_oddie()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->_is_oddie;
        }
        void ClearAll()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_DSS_ClearAll(ctx);
        }
        /// 
        /// This is a no-op function, does nothing. Left for compatibility.
        /// 
        /// Original COM help: https://opendss.epri.com/Reset1.html
        /// 
        void Reset()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_DSS_Reset(ctx);
        }
        int32_t SetActiveClass(const char *ClassName)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSS_SetActiveClass(ctx, ClassName);
        }
        int32_t SetActiveClass(const string &ClassName)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSS_SetActiveClass(ctx, ClassName.c_str());
        }
        /// 
        /// This is a no-op function, does nothing. Left for compatibility.
        /// 
        /// Calling `Start` in AltDSS/DSS-Extensions is required but that is already
        /// handled automatically, so the users do not need to call it manually,
        /// unless using AltDSS/DSS C-API directly without further tools.
        /// 
        /// On EPRI's OpenDSS, `Start` also does nothing at all in the current
        /// Delphi versions. It is required for OpenDSS-C, but also handled behind
        /// the scenes on DSS-Extensions.
        /// 
        /// Original COM help: https://opendss.epri.com/Start.html
        /// 
        bool Start(int32_t code)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSS_Start(ctx, code);
        }

        /// 
        /// List of DSS intrinsic classes (names of the classes)
        /// 
        /// Original COM help: https://opendss.epri.com/Classes1.html
        /// 
        strings Classes() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_DSS_Get_Classes);
        }

        /// 
        /// DSS Data File Path.  Default path for reports, etc. from DSS
        /// 
        /// Original COM help: https://opendss.epri.com/DataPath.html
        /// 
        string DataPath() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSS_Get_DataPath(ctx);
        }
        IDSS& DataPath(const char *value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_DSS_Set_DataPath(ctx, value);
            return *this;
        }
        IDSS& DataPath(const string &value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_DSS_Set_DataPath(ctx, value.c_str());
            return *this;
        }

        /// 
        /// Returns the path name for the default text editor.
        /// 
        /// Original COM help: https://opendss.epri.com/DefaultEditor.html
        /// 
        string DefaultEditor() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSS_Get_DefaultEditor(ctx);
        }

        /// 
        /// Number of Circuits currently defined
        /// 
        /// Original COM help: https://opendss.epri.com/NumCircuits.html
        /// 
        int32_t NumCircuits() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSS_Get_NumCircuits(ctx);
        }

        /// 
        /// Number of DSS intrinsic classes
        /// 
        /// Original COM help: https://opendss.epri.com/NumClasses.html
        /// 
        int32_t NumClasses() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSS_Get_NumClasses(ctx);
        }

        /// 
        /// Number of user-defined classes
        /// 
        /// Original COM help: https://opendss.epri.com/NumUserClasses.html
        /// 
        int32_t NumUserClasses() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSS_Get_NumUserClasses(ctx);
        }

        /// 
        /// List of user-defined classes
        /// 
        /// Original COM help: https://opendss.epri.com/UserClasses.html
        /// 
        strings UserClasses() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_DSS_Get_UserClasses);
        }

        /// 
        /// Get version string for the DSS.
        /// 
        /// Original COM help: https://opendss.epri.com/Version.html
        /// 
        string Version() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSS_Get_Version(ctx);
        }

        /// 
        /// Indicates whether text output is allowed or forms are used. Disable to silence most output.
        /// 
        /// Currently, forms/windows are only used for EPRI's OpenDSS distribution on Windows.
        /// 
        /// Original COM help: https://opendss.epri.com/AllowForms.html
        /// 
        bool AllowForms() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSS_Get_AllowForms(ctx);
        }
        IDSS& AllowForms(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_DSS_Set_AllowForms(ctx, value);
            return *this;
        }

        /// 
        /// Gets/sets whether running the external editor for "Show" is allowed
        /// 
        /// AllowEditor controls whether the external editor is used in commands like "Show".
        /// If you set to 0 (false), the editor is not executed. Note that other side effects,
        /// such as the creation of files, are not affected.
        /// 
        /// **Deprecated:** Use `Settings.AllowEditor` instead (same behavior, the setting was just moved there for better organization).
        /// 
        /// **(API Extension)**
        /// 
        bool AllowEditor() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            // #warning (""AllowEditor" was moved to the Settings interface. This property still works, but will be removed in a future release. Please use `...Settings.AllowEditor` instead.", DeprecationWarning, stacklevel=2);
            return ctx_DSS_Get_AllowEditor(ctx);
        }
        IDSS& AllowEditor(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_DSS_Set_AllowEditor(ctx, value);
            return *this;
        }
        void ShowPanel()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            if (api_util->_is_oddie)
            {
                ctx_Text_Set_Command(ctx, "panel");
            }
        }
        /// 
        /// Make a new circuit and returns the interface to the active circuit.
        /// 
        /// Original COM help: https://opendss.epri.com/NewCircuit.html
        /// 
        ICircuit& NewCircuit(const char *name)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_DSS_NewCircuit(ctx, name);
            return ActiveCircuit;
        }
        /// 
        /// Make a new circuit and returns the interface to the active circuit.
        /// 
        /// Original COM help: https://opendss.epri.com/NewCircuit.html
        /// 
        ICircuit& NewCircuit(const string &name)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_DSS_NewCircuit(ctx, name.c_str());
            return ActiveCircuit;
        }

        /// 
        /// LegacyModels was a flag used to toggle legacy (pre-2019) models for PVSystem, InvControl, Storage and
        /// StorageControl.
        /// In EPRI's OpenDSS version 9.0, the old models were removed. They were temporarily present here
        /// but were also removed in DSS C-API v0.13.0.
        ///     
        /// **NOTE**: this property will be removed for v1.0. It is left to avoid breaking the current API too soon.
        /// 
        /// **(API Extension)**
        /// 
        bool LegacyModels() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSS_Get_LegacyModels(ctx);
        }
        IDSS& LegacyModels(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_DSS_Set_LegacyModels(ctx, value);
            return *this;
        }

        /// 
        /// If disabled, the engine will not change the active working directory during execution. E.g. a "compile"
        /// command will not "chdir" to the file path.
        /// 
        /// If you have issues with long paths, enabling this might help in some scenarios.
        /// 
        /// Defaults to True (allow changes, backwards compatible) in the 0.10.x versions of DSS C-API. 
        /// This might change to False in future versions.
        /// 
        /// This can also be set through the environment variable DSS_CAPI_ALLOW_CHANGE_DIR. Set it to 0 to
        /// disallow changing the active working directory.
        /// 
        /// **Deprecated:** Use `Settings.AllowChangeDir` instead (same behavior, the setting was just moved there for better organization).
        /// 
        /// **(API Extension)**
        /// 
        bool AllowChangeDir() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            // #warning (""AllowChangeDir" was moved to the Settings interface. This property still works, but will be removed in a future release. Please use `...Settings.AllowChangeDir` instead.", DeprecationWarning, stacklevel=2);
            return ctx_DSS_Get_AllowChangeDir(ctx);
        }
        IDSS& AllowChangeDir(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_DSS_Set_AllowChangeDir(ctx, value);
            return *this;
        }

        /// 
        /// If enabled, the `DOScmd` command is allowed. Otherwise, an error is reported if the user tries to use it.
        /// 
        /// Defaults to False/0 (disabled state). Users should consider DOScmd deprecated on DSS-Extensions.
        /// 
        /// This can also be set through the environment variable DSS_CAPI_ALLOW_DOSCMD. Setting it to 1 enables
        /// the command.
        /// 
        /// **Deprecated:** Use `Settings.AllowDOScmd` instead (same behavior, the setting was just moved there for better organization).
        /// 
        /// **(API Extension)**
        /// 
        bool AllowDOScmd() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            // #warning (""AllowDOScmd" was moved to the Settings interface. This property still works, but will be removed in a future release. Please use `...Settings.AllowDOScmd` instead.", DeprecationWarning, stacklevel=2);
            return ctx_DSS_Get_AllowDOScmd(ctx);
        }
        IDSS& AllowDOScmd(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_DSS_Set_AllowDOScmd(ctx, value);
            return *this;
        }

        /// 
        /// If enabled, in case of errors or empty arrays, the API returns arrays with values compatible with
        /// EPRI's OpenDSS COM interface. 
        /// 
        /// For example, consider the function `Loads_Get_ZIPV`. If there is no active circuit or active load element:
        /// 
        /// - In the disabled state (COMErrorResults=False), the function will return "[]", an array with 0 elements.
        /// - In the enabled state (COMErrorResults=True), the function will return "[0.0]" instead. This should
        /// be compatible with the return value of EPRI's COM interface.
        /// 
        /// Defaults to false (disabled state) in AltDSS since the v0.15.x series.
        /// 
        /// This does not affect the results when using EPRI's OpenDSS distribution through Oddie.
        /// 
        /// This can also be set through the environment variable `DSS_CAPI_COM_DEFAULTS`. Setting it to 1 enables
        /// the legacy/COM behavior. The value can be toggled through the API at any time.
        /// 
        /// **Deprecated:** Use `Settings.COMErrorResults` instead (same behavior, the setting was just moved there for better organization).
        /// 
        /// **(API Extension)**
        /// 
        bool COMErrorResults() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            // #warning (""COMErrorResults" was moved to the Settings interface. This property still works, but will be removed in a future release. Please use `...Settings.COMErrorResults` instead.", DeprecationWarning, stacklevel=2);
            return ctx_DSS_Get_COMErrorResults(ctx);
        }
        IDSS& COMErrorResults(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            // #warning (""COMErrorResults" was moved to the Settings interface. This property still works, but will be removed in a future release. Please use `...Settings.COMErrorResults` instead.", DeprecationWarning, stacklevel=2);
            ctx_DSS_Set_COMErrorResults(ctx, value);
            return *this;
        }

        /// 
        /// Controls some compatibility flags introduced to toggle some behavior from EPRI's OpenDSS.
        /// 
        /// **THE FLAGS ARE GLOBAL, affecting all AltDSS engines in the process.**  
        /// CompatFlags for Oddie-loaded instances (OpenDSS and OpenDSS-C engines) are handled by the Oddie code itself,
        /// so it is global for each Oddie library.
        /// 
        /// These flags may change for each version of DSS C-API, but the same value will not be reused. That is,
        /// when we remove a compatibility flag, it will have no effect but will also not affect anything else
        /// besides raising an error if the user tries to toggle a flag that was available in a previous version.
        /// 
        /// We expect to keep a very limited number of flags. Since the flags are more transient than the other
        /// options/flags, it was preferred to add this generic function instead of a separate function per
        /// flag.
        /// 
        /// See the enumeration `DSSCompatFlags` for available flags, including description.
        /// 
        /// **Deprecated:** Use `Settings.CompatFlags` instead (same behavior, the setting was just moved there for better organization).
        /// 
        /// **(API Extension)**
        /// 
        DSSCompatFlags CompatFlags() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSS_Get_CompatFlags(ctx);
        }
        IDSS& CompatFlags(DSSCompatFlags value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            // #warning (""CompatFlags" was moved to the Settings interface. This property still works, but will be removed in a future release. Please use `...Settings.CompatFlags` instead.", DeprecationWarning, stacklevel=2);
            ctx_DSS_Set_CompatFlags(ctx, static_cast<uint32_t>(value));
            return *this;
        }
    };

} } // namespace altdss::classic
#endif // #ifndef ALTDSS_CPP_CLASSIC_API
