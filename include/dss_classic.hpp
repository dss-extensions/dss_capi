/*!

dss_classic.hpp: a C++ layer for the DSS Extensions/C-API, classic API
Copyright (c) 2021-2022 Paulo Meira
Copyright (c) 2021-2022 DSS Extensions contributors

Version: 0.5.0 (2022-07)

**POTENTIAL BREAKING CHANGES UNTIL VERSION 1.0**

*/
        
#pragma once
#ifndef DSS_CPP_CLASSIC_API
#define DSS_CPP_CLASSIC_API

#include "dss_common.hpp"
#include "dss_obj.hpp"

namespace dss { namespace classic {

#ifdef DSS_CAPI_NAMESPACE
using namespace dss::capi;
#endif



    class IDSSProgress: public ContextState
    {
    public:

        IDSSProgress(dss::APIUtil *util) :
            ContextState(util)
        {
        }
        void Close()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_DSSProgress_Close(ctx);
        }
        void Show()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_DSSProgress_Show(ctx);
        }

        /// 
        /// (write-only) Caption to appear on the bottom of the DSS Progress form.
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
        /// (write-only) Percent progress to indicate [0..100]
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

        IDSSProperty(dss::APIUtil *util) :
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
        string Description() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSSProperty_Get_Description(ctx);
        }

        /// 
        /// Name of Property
        /// 
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSSProperty_Get_Name(ctx);
        }

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

        IDSS_Executive(dss::APIUtil *util) :
            ContextState(util)
        {
        }
        /// 
        /// Get i-th command
        /// 
        string Command(int32_t i)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSS_Executive_Get_Command(ctx, i);
        }
        /// 
        /// Get help string for i-th command
        /// 
        string CommandHelp(int32_t i)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSS_Executive_Get_CommandHelp(ctx, i);
        }
        /// 
        /// Get i-th option
        /// 
        string Option(int32_t i)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSS_Executive_Get_Option(ctx, i);
        }
        /// 
        /// Get help string for i-th option
        /// 
        string OptionHelp(int32_t i)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSS_Executive_Get_OptionHelp(ctx, i);
        }
        /// 
        /// Get present value of i-th option
        /// 
        string OptionValue(int32_t i)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSS_Executive_Get_OptionValue(ctx, i);
        }

        /// 
        /// Number of DSS Executive Commands
        /// 
        int32_t NumCommands() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSS_Executive_Get_NumCommands(ctx);
        }

        /// 
        /// Number of DSS Executive Options
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

        IError(dss::APIUtil *util) :
            ContextState(util)
        {
        }

        /// 
        /// Description of error for last operation
        /// 
        string Description() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Error_Get_Description(ctx);
        }

        /// 
        /// Error Number (returns current value and then resets to zero)
        /// 
        int32_t Number() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Error_Get_Number(ctx);
        }

        /// 
        /// EarlyAbort controls whether all errors halts the DSS script processing (Compile/Redirect), defaults to True.
        /// 
        /// (API Extension)
        /// 
        bool EarlyAbort() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_Error_Get_EarlyAbort(ctx) != 0);
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
        /// original/official COM interface, the checks do not produce any error 
        /// message. An error value can be returned by a function but this value
        /// can, for many of the functions, be a valid value. As such, the user
        /// has no means to detect an invalid API call. 
        /// 
        /// Extended errors use the Error interface to provide a more clear message
        /// and should help users, especially new users, to find usage issues earlier.
        /// 
        /// At Python level, an exception is raised when an error is detected through
        /// the Error interface.
        /// 
        /// The current default state is ON. For compatibility, the user can turn it
        /// off to restore the previous behavior.
        /// 
        /// (API Extension)
        /// 
        /// 
        bool ExtendedErrors() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_Error_Get_ExtendedErrors(ctx) != 0);
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

        IFuses(dss::APIUtil *util) :
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
        void Close()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Fuses_Close(ctx);
        }
        /// 
        /// Current state of the fuses. TRUE if any fuse on any phase is blown. Else FALSE.
        /// 
        bool IsBlown()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_Fuses_IsBlown(ctx) != 0);
        }
        /// 
        /// Manual opening of all phases of the fuse.
        /// 
        void Open()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Fuses_Open(ctx);
        }
        /// 
        /// Reset fuse to normal state.
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
        int32_t NumPhases() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Fuses_Get_NumPhases(ctx);
        }

        /// 
        /// Multiplier or actual amps for the TCCcurve object. Defaults to 1.0. 
        /// Multiply current values of TCC curve by this to get actual amps.
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

        IGenerators(dss::APIUtil *util) :
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
        /// Indicates whether the generator is forced ON regardles of other dispatch criteria.
        /// 
        bool ForcedON() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_Generators_Get_ForcedON(ctx) != 0);
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
        strings RegisterNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Generators_Get_RegisterNames);
        }

        /// 
        /// Array of valus in generator energy meter registers.
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
        /// (API Extension)
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
        /// (API Extension)
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
        /// (API Extension)
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
        /// (API Extension)
        /// 
        int32_t Status() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Generators_Get_Status(ctx);
        }
        IGenerators& Status(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Generators_Set_Status(ctx, value);
            return *this;
        }

        /// 
        /// Generator connection. True/1 if delta connection, False/0 if wye.
        /// 
        /// (API Extension)
        /// 
        bool IsDelta() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_Generators_Get_IsDelta(ctx) != 0);
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
        /// (API Extension)
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
        /// 
        /// (API Extension)
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
        /// (API Extension)
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

        IISources(dss::APIUtil *util) :
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

        ILineCodes(dss::APIUtil *util) :
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
        bool IsZ1Z0() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_LineCodes_Get_IsZ1Z0(ctx) != 0);
        }

        /// 
        /// Normal Ampere rating
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

        int32_t Units() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LineCodes_Get_Units(ctx);
        }
        ILineCodes& Units(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineCodes_Set_Units(ctx, value);
            return *this;
        }

        /// 
        /// Zero Sequence Reactance, Ohms per unit length
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
        /// Posiive-sequence reactance, ohms per unit length
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

        ILineSpacings(dss::APIUtil *util) :
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

        int32_t Units() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LineSpacings_Get_Units(ctx);
        }
        ILineSpacings& Units(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LineSpacings_Set_Units(ctx, value);
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

        ILoadShapes(dss::APIUtil *util) :
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
        int32_t New(const char *Name)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LoadShapes_New(ctx, Name);
        }
        int32_t New(const string &Name)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_LoadShapes_New(ctx, Name.c_str());
        }
        void Normalize()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LoadShapes_Normalize(ctx);
        }

        /// 
        /// Fixed interval time value, hours.
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
        /// Time array in hours correscponding to P and Q multipliers when the Interval=0.
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
        bool UseActual() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_LoadShapes_Get_UseActual(ctx) != 0);
        }
        ILoadShapes& UseActual(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_LoadShapes_Set_UseActual(ctx, value);
            return *this;
        }

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
        /// (API Extension)
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
        /// (API Extension)
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

        IMonitors(dss::APIUtil *util) :
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
        void Process()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Monitors_Process(ctx);
        }
        void ProcessAll()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Monitors_ProcessAll(ctx);
        }
        void Reset()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Monitors_Reset(ctx);
        }
        void ResetAll()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Monitors_ResetAll(ctx);
        }
        void Sample()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Monitors_Sample(ctx);
        }
        void SampleAll()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Monitors_SampleAll(ctx);
        }
        void Save()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Monitors_Save(ctx);
        }
        void SaveAll()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Monitors_SaveAll(ctx);
        }
        void Show()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Monitors_Show(ctx);
        }

        /// 
        /// Byte Array containing monitor stream values. Make sure a "save" is done first (standard solution modes do this automatically)
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
        string FileName() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Monitors_Get_FileName(ctx);
        }

        /// 
        /// Monitor File Version (integer)
        /// 
        int32_t FileVersion() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Monitors_Get_FileVersion(ctx);
        }

        /// 
        /// Header string;  Array of strings containing Channel names
        /// 
        strings Header() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Monitors_Get_Header);
        }

        /// 
        /// Set Monitor mode (bitmask integer - see DSS Help)
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
        int32_t NumChannels() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Monitors_Get_NumChannels(ctx);
        }

        /// 
        /// Size of each record in ByteStream (Integer). Same as NumChannels.
        /// 
        int32_t RecordSize() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Monitors_Get_RecordSize(ctx);
        }

        /// 
        /// Number of Samples in Monitor at Present
        /// 
        int32_t SampleCount() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Monitors_Get_SampleCount(ctx);
        }

        /// 
        /// Terminal number of element being monitored.
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

        IParser(dss::APIUtil *util) :
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
        void ResetDelimiters()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Parser_ResetDelimiters(ctx);
        }

        /// 
        /// Default is FALSE. If TRUE parser automatically advances to next token after DblValue, IntValue, or StrValue. Simpler when you don't need to check for parameter names.
        /// 
        bool AutoIncrement() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_Parser_Get_AutoIncrement(ctx) != 0);
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
        double DblValue() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Parser_Get_DblValue(ctx);
        }

        /// 
        /// String defining hard delimiters used to separate token on the command string. Default is , and =. The = separates token name from token value. These override whitesspace to separate tokens.
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
        int32_t IntValue() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Parser_Get_IntValue(ctx);
        }

        /// 
        /// Get next token and return tag name (before = sign) if any. See AutoIncrement.
        /// 
        string NextParam() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Parser_Get_NextParam(ctx);
        }

        /// 
        /// Return next parameter as a string
        /// 
        string StrValue() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Parser_Get_StrValue(ctx);
        }

        /// 
        /// (read) Get the characters used for White space in the command string.  Default is blank and Tab.
        /// (write) Set the characters used for White space in the command string.  Default is blank and Tab.
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

        IReduceCkt(dss::APIUtil *util) :
            ContextState(util)
        {
        }

        /// 
        /// Zmag (ohms) for Reduce Option for Z of short lines
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
        /// Keep load flag (T/F) for Reduction options that remove branches
        /// 
        bool KeepLoad() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_ReduceCkt_Get_KeepLoad(ctx) != 0);
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
        /// Name of Energymeter to use for reduction
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
        void DoDefault()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ReduceCkt_DoDefault(ctx);
        }
        /// 
        /// Do ShortLines algorithm: Set Zmag first if you don't want the default
        /// 
        void DoShortLines()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ReduceCkt_DoShortLines(ctx);
        }
        /// 
        /// Reduce Dangling Algorithm; branches with nothing connected
        /// 
        void DoDangling()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ReduceCkt_DoDangling(ctx);
        }
        void DoLoopBreak()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ReduceCkt_DoLoopBreak(ctx);
        }
        void DoParallelLines()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ReduceCkt_DoParallelLines(ctx);
        }
        void DoSwitches()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ReduceCkt_DoSwitches(ctx);
        }
        void Do1phLaterals()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ReduceCkt_Do1phLaterals(ctx);
        }
        void DoBranchRemove()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ReduceCkt_DoBranchRemove(ctx);
        }
    };

    class IYMatrix: public ContextState
    {
    public:

        IYMatrix(dss::APIUtil *util) :
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
            return (ctx_YMatrix_Get_SystemYChanged(ctx) != 0);
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
            return (ctx_YMatrix_Get_UseAuxCurrents(ctx) != 0);
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
            return (ctx_YMatrix_CheckConvergence(ctx) != 0);
        }
        void SetGeneratordQdV()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_YMatrix_SetGeneratordQdV(ctx);
        }

        bool LoadsNeedUpdating() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_YMatrix_Get_LoadsNeedUpdating(ctx) != 0);
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
            return (ctx_YMatrix_Get_SolutionInitialized(ctx) != 0);
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

        IMeters(dss::APIUtil *util) :
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
        void CloseAllDIFiles()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Meters_CloseAllDIFiles(ctx);
        }
        void DoReliabilityCalc(bool AssumeRestoration)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Meters_DoReliabilityCalc(ctx, AssumeRestoration);
        }
        void OpenAllDIFiles()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Meters_OpenAllDIFiles(ctx);
        }
        void Reset()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Meters_Reset(ctx);
        }
        void ResetAll()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Meters_ResetAll(ctx);
        }
        void Sample()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Meters_Sample(ctx);
        }
        void SampleAll()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Meters_SampleAll(ctx);
        }
        void Save()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Meters_Save(ctx);
        }
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
        /// Wide string list of all branches in zone of the active energymeter object.
        /// 
        strings AllBranchesInZone() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Meters_Get_AllBranchesInZone);
        }

        /// 
        /// Array of names of all zone end elements.
        /// 
        strings AllEndElements() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Meters_Get_AllEndElements);
        }

        /// 
        /// Array of doubles: set the phase allocation factors for the active meter.
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
        double AvgRepairTime() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_AvgRepairTime(ctx);
        }

        /// 
        /// Set the magnitude of the real part of the Calculated Current (normally determined by solution) for the Meter to force some behavior on Load Allocation
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
        /// Number of branches in Active energymeter zone. (Same as sequencelist size)
        /// 
        int32_t CountBranches() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_CountBranches(ctx);
        }

        /// 
        /// Number of zone end elements in the active meter zone.
        /// 
        int32_t CountEndElements() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_CountEndElements(ctx);
        }

        /// 
        /// Total customer interruptions for this Meter zone based on reliability calcs.
        /// 
        double CustInterrupts() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_CustInterrupts(ctx);
        }

        /// 
        /// Global Flag in the DSS to indicate if Demand Interval (DI) files have been properly opened.
        /// 
        bool DIFilesAreOpen() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_Meters_Get_DIFilesAreOpen(ctx) != 0);
        }

        /// 
        /// Sum of Fault Rate time Repair Hrs in this section of the meter zone
        /// 
        double FaultRateXRepairHrs() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_FaultRateXRepairHrs(ctx);
        }

        /// 
        /// Set Name of metered element
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
        /// set Number of Metered Terminal
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
        int32_t NumSectionBranches() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_NumSectionBranches(ctx);
        }

        /// 
        /// Number of Customers in the active section.
        /// 
        int32_t NumSectionCustomers() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_NumSectionCustomers(ctx);
        }

        /// 
        /// Number of feeder sections in this meter's zone
        /// 
        int32_t NumSections() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_NumSections(ctx);
        }

        /// 
        /// Type of OCP device. 1=Fuse; 2=Recloser; 3=Relay
        /// 
        int32_t OCPDeviceType() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_OCPDeviceType(ctx);
        }

        /// 
        /// Array of doubles to set values of Peak Current property
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
        strings RegisterNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Meters_Get_RegisterNames);
        }

        /// 
        /// Array of all the values contained in the Meter registers for the active Meter.
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
        double SAIDI() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_SAIDI(ctx);
        }

        /// 
        /// Returns SAIFI for this meter's Zone. Execute Reliability Calc method first.
        /// 
        double SAIFI() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_SAIFI(ctx);
        }

        /// 
        /// SAIFI based on kW rather than number of customers. Get after reliability calcs.
        /// 
        double SAIFIKW() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_SAIFIKW(ctx);
        }

        /// 
        /// SequenceIndex of the branch at the head of this section
        /// 
        int32_t SectSeqIdx() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_SectSeqIdx(ctx);
        }

        /// 
        /// Total Customers downline from this section
        /// 
        int32_t SectTotalCust() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_SectTotalCust(ctx);
        }

        /// 
        /// Size of Sequence List
        /// 
        int32_t SeqListSize() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_SeqListSize(ctx);
        }

        /// 
        /// Get/set Index into Meter's SequenceList that contains branch pointers in lexical order. Earlier index guaranteed to be upline from later index. Sets PDelement active.
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
        double SumBranchFltRates() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_SumBranchFltRates(ctx);
        }

        /// 
        /// Total Number of customers in this zone (downline from the EnergyMeter)
        /// 
        int32_t TotalCustomers() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Meters_Get_TotalCustomers(ctx);
        }

        /// 
        /// Totals of all registers of all meters
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

        IPDElements(dss::APIUtil *util) :
            ContextState(util)
        {
        }

        /// 
        /// accummulated failure rate for this branch on downline
        /// 
        double AccumulatedL() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PDElements_Get_AccumulatedL(ctx);
        }

        /// 
        /// Number of PD elements (including disabled elements)
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
            return (ctx_PDElements_Get_IsShunt(ctx) != 0);
        }

        /// 
        /// Failure rate for this branch. Faults per year including length of line.
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
        int32_t Numcustomers() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PDElements_Get_Numcustomers(ctx);
        }

        /// 
        /// Sets the parent PD element to be the active circuit element.
        /// Returns 0 if no more elements upline.
        /// 
        int32_t ParentPDElement() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PDElements_Get_ParentPDElement(ctx);
        }

        /// 
        /// Average repair time for this element in hours
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
        int32_t SectionID() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PDElements_Get_SectionID(ctx);
        }

        /// 
        /// Total miles of line from this element to the end of the zone. For recloser siting algorithm.
        /// 
        double TotalMiles() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PDElements_Get_TotalMiles(ctx);
        }

        /// 
        /// Total number of customers from this branch to the end of the zone
        /// 
        int32_t Totalcustomers() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PDElements_Get_Totalcustomers(ctx);
        }

        /// 
        /// Get/Set percent of faults that are permanent (require repair). Otherwise, fault is assumed to be transient/temporary.
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
        /// (API Extension)
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
        /// (API Extension)
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
        /// (API Extension)
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
        /// (API Extension)
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
        /// (API Extension)
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
        /// (API Extension)
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
        /// (API Extension)
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT AllCplxSeqCurrents() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PDElements_Get_AllCplxSeqCurrents_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Double array of the symmetrical component currents into each 3-phase terminal, for each PD element.
        /// 
        /// (API Extension)
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
        /// (API Extension)
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT AllPowers() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_PDElements_Get_AllPowers_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Double array of sequence powers into each 3-phase teminal, for each PD element
        /// 
        /// (API Extension)
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
        /// (API Extension)
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
        /// (API Extension)
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
        /// (API Extension)
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

        IPVSystems(dss::APIUtil *util) :
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
        /// Array of PVSYSTEM energy meter register names
        /// 
        strings RegisterNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_PVSystems_Get_RegisterNames);
        }

        /// 
        /// Array of doubles containing values in PVSystem registers.
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
        /// get kW output
        /// 
        double kW() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PVSystems_Get_kW(ctx);
        }

        /// 
        /// Get/set kvar output value
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
        /// Name of the loadshape for a daily PVSystem profile.
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
        double IrradianceNow() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PVSystems_Get_IrradianceNow(ctx);
        }

        /// 
        /// Gets/sets the rated max power of the PV array for 1.0 kW/sq-m irradiance 
        /// and a user-selected array temperature of the active PVSystem.
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
        string Sensor() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_PVSystems_Get_Sensor(ctx);
        }
    };

    class IParallel: public ContextState
    {
    public:

        IParallel(dss::APIUtil *util) :
            ContextState(util)
        {
        }
        void CreateActor()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Parallel_CreateActor(ctx);
        }
        void Wait()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Parallel_Wait(ctx);
        }

        /// 
        /// Gets/sets the ID of the Active Actor
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
        int32_t NumCPUs() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Parallel_Get_NumCPUs(ctx);
        }

        /// 
        /// Delivers the number of Cores of the local PC
        /// 
        int32_t NumCores() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Parallel_Get_NumCores(ctx);
        }

        /// 
        /// Gets the number of Actors created
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

        IReactors(dss::APIUtil *util) :
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
        int32_t SpecType() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Reactors_Get_SpecType(ctx);
        }

        /// 
        /// Delta connection or wye?
        /// 
        bool IsDelta() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_Reactors_Get_IsDelta(ctx) != 0);
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
            return (ctx_Reactors_Get_Parallel(ctx) != 0);
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
        /// Not necessary to specify for delta (LL) connection
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

        IReclosers(dss::APIUtil *util) :
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
        /// Ground (3I0) instantaneous trip setting - curve multipler or actual amps.
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
        /// Phase instantaneous curve multipler or actual amps
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
        /// Get/Set present state of recloser. 
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

        IRegControls(dss::APIUtil *util) :
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
        /// Regulation bandwidth in forward direciton, centered on Vreg
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
        /// Time delay is inversely adjsuted, proportinal to the amount of voltage outside the regulating band.
        /// 
        bool IsInverseTime() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_RegControls_Get_IsInverseTime(ctx) != 0);
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
        bool IsReversible() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_RegControls_Get_IsReversible(ctx) != 0);
        }
        IRegControls& IsReversible(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_RegControls_Set_IsReversible(ctx, value);
            return *this;
        }

        /// 
        /// Maximum tap change per iteration in STATIC solution mode. 1 is more realistic, 16 is the default for a faster soluiton.
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
        /// Integer number of the tap that the controlled transformer winding is currentliy on.
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

        IRelays(dss::APIUtil *util) :
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
        void Open()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Relays_Open(ctx);
        }
        /// 
        /// Close the switched object controlled by the relay. Resets relay to first operation.
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
        /// Get/Set present state of relay. 
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

        ISensors(dss::APIUtil *util) :
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
        bool IsDelta() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_Sensors_Get_IsDelta(ctx) != 0);
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
        bool ReverseDelta() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_Sensors_Get_ReverseDelta(ctx) != 0);
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

        ISwtControls(dss::APIUtil *util) :
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
        /// Time delay [s] betwen arming and opening or closing the switch.  Control may reset before actually operating the switch.
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
        bool IsLocked() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_SwtControls_Get_IsLocked(ctx) != 0);
        }
        ISwtControls& IsLocked(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_SwtControls_Set_IsLocked(ctx, value);
            return *this;
        }

        /// 
        /// Get/set Normal state of switch (see actioncodes) dssActionOpen or dssActionClose
        /// 
        int32_t NormalState() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_SwtControls_Get_NormalState(ctx);
        }
        ISwtControls& NormalState(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_SwtControls_Set_NormalState(ctx, value);
            return *this;
        }

        /// 
        /// Set it to force the switch to a specified state, otherwise read its present state.
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

        ITSData(dss::APIUtil *util) :
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

        IText(dss::APIUtil *util) :
            ContextState(util)
        {
        }

        /// Runs a large string as command lines directly in the DSS engine.
        /// Intermediate results are ignored.
        /// 
        /// (API Extension)
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
        string Result() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Text_Get_Result(ctx);
        }
    };

    class ITopology: public ContextState
    {
    public:

        ITopology(dss::APIUtil *util) :
            ContextState(util)
        {
        }

        /// 
        /// Returns index of the active branch
        /// 
        int32_t ActiveBranch() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Topology_Get_ActiveBranch(ctx);
        }

        /// 
        /// Topological depth of the active branch
        /// 
        int32_t ActiveLevel() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Topology_Get_ActiveLevel(ctx);
        }

        /// 
        /// Array of all isolated branch names.
        /// 
        strings AllIsolatedBranches() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Topology_Get_AllIsolatedBranches);
        }

        /// 
        /// Array of all isolated load names.
        /// 
        strings AllIsolatedLoads() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Topology_Get_AllIsolatedLoads);
        }

        /// 
        /// Array of all looped element names, by pairs.
        /// 
        strings AllLoopedPairs() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Topology_Get_AllLoopedPairs);
        }

        /// 
        /// MOve back toward the source, return index of new active branch, or 0 if no more.
        /// 
        int32_t BackwardBranch() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Topology_Get_BackwardBranch(ctx);
        }

        /// 
        /// Name of the active branch.
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
        int32_t First() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Topology_Get_First(ctx);
        }

        /// 
        /// First load at the active branch, return index or 0 if none.
        /// 
        int32_t FirstLoad() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Topology_Get_FirstLoad(ctx);
        }

        /// 
        /// Move forward in the tree, return index of new active branch or 0 if no more
        /// 
        int32_t ForwardBranch() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Topology_Get_ForwardBranch(ctx);
        }

        /// 
        /// Move to looped branch, return index or 0 if none.
        /// 
        int32_t LoopedBranch() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Topology_Get_LoopedBranch(ctx);
        }

        /// 
        /// Sets the next branch active, returns 0 if no more.
        /// 
        int32_t Next() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Topology_Get_Next(ctx);
        }

        /// 
        /// Next load at the active branch, return index or 0 if no more.
        /// 
        int32_t NextLoad() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Topology_Get_NextLoad(ctx);
        }

        /// 
        /// Number of isolated branches (PD elements and capacitors).
        /// 
        int32_t NumIsolatedBranches() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Topology_Get_NumIsolatedBranches(ctx);
        }

        /// 
        /// Number of isolated loads
        /// 
        int32_t NumIsolatedLoads() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Topology_Get_NumIsolatedLoads(ctx);
        }

        /// 
        /// Number of loops
        /// 
        int32_t NumLoops() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Topology_Get_NumLoops(ctx);
        }

        /// 
        /// Move to directly parallel branch, return index or 0 if none.
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

        ITransformers(dss::APIUtil *util) :
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
        bool IsDelta() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_Transformers_Get_IsDelta(ctx) != 0);
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
        /// Active Winding number of tap steps betwein MinTap and MaxTap.
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
        /// Name of an XfrmCode that supplies electircal parameters for this Transformer.
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
        /// Percent reactance between windigns 1 and 3, on winding 1 kVA base.  Use for 3-winding transformers only.
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
        string strWdgCurrents() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Transformers_Get_strWdgCurrents(ctx);
        }

        /// 
        /// Transformer Core Type: 0=shell;1 = 1-phase; 3= 3-leg; 5= 5-leg
        /// 
        int32_t CoreType() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Transformers_Get_CoreType(ctx);
        }
        ITransformers& CoreType(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Transformers_Set_CoreType(ctx, value);
            return *this;
        }

        /// 
        /// dc Resistance of active winding in ohms for GIC analysis
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
        /// Complex array with the losses by type (total losses, load losses, no-load losses), in VA
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

        IVsources(dss::APIUtil *util) :
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

        IWireData(dss::APIUtil *util) :
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

        int32_t GMRUnits() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WireData_Get_GMRUnits(ctx);
        }
        IWireData& GMRUnits(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WireData_Set_GMRUnits(ctx, value);
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

        int32_t ResistanceUnits() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_WireData_Get_ResistanceUnits(ctx);
        }
        IWireData& ResistanceUnits(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_WireData_Set_ResistanceUnits(ctx, value);
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

        IXYCurves(dss::APIUtil *util) :
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
        /// Get/set X values as a Array of doubles. Set Npts to max number expected if setting
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
        /// Get/Set Y values in curve; Set Npts to max number expected if setting
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

        IZIP(dss::APIUtil *util) :
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
        /// (API Extension)
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
        /// (API Extension)
        /// 
        void Open(const string &FileName)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_ZIP_Open(ctx, FileName.c_str());
        }
        /// 
        /// Closes the current open ZIP file
        /// 
        /// (API Extension)
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
        /// (API Extension)
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
        /// (API Extension)
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
        /// (API Extension)
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
        /// (API Extension)
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
        /// (API Extension)
        /// 
        bool Contains(const char *Name)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_ZIP_Contains(ctx, Name) != 0);
        }
        /// 
        /// Check if the given path name is present in the current ZIP file.
        /// 
        /// (API Extension)
        /// 
        bool Contains(const string &Name)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_ZIP_Contains(ctx, Name.c_str()) != 0);
        }
    };

    class IActiveClass: public ContextState
    {
    public:

        IActiveClass(dss::APIUtil *util) :
            ContextState(util)
        {
        }

        /// 
        /// Returns name of active class.
        /// 
        string ActiveClassName() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_ActiveClass_Get_ActiveClassName(ctx);
        }

        /// 
        /// Array of strings consisting of all element names in the active class.
        /// 
        strings AllNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_ActiveClass_Get_AllNames);
        }

        /// 
        /// Number of elements in Active Class. Same as NumElements Property.
        /// 
        int32_t Count() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_ActiveClass_Get_Count(ctx);
        }

        /// 
        /// Sets first element in the active class to be the active DSS object. If object is a CktElement, ActiveCktELment also points to this element. Returns 0 if none.
        /// 
        int32_t First() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_ActiveClass_Get_First(ctx);
        }

        /// 
        /// Name of the Active Element of the Active Class
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
        /// Sets next element in active class to be the active DSS object. If object is a CktElement, ActiveCktElement also points to this element.  Returns 0 if no more.
        /// 
        int32_t Next() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_ActiveClass_Get_Next(ctx);
        }

        /// 
        /// Number of elements in this class. Same as Count property.
        /// 
        int32_t NumElements() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_ActiveClass_Get_NumElements(ctx);
        }

        /// 
        /// Get the name of the parent class of the active class
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
        /// (API Extension)
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

        IBus(dss::APIUtil *util) :
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
        int32_t GetUniqueNodeNumber(int32_t StartNumber)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Bus_GetUniqueNodeNumber(ctx, StartNumber);
        }
        bool ZscRefresh()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_Bus_ZscRefresh(ctx) != 0);
        }

        /// 
        /// False=0 else True. Indicates whether a coordinate has been defined for this bus
        /// 
        bool Coorddefined() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_Bus_Get_Coorddefined(ctx) != 0);
        }

        /// 
        /// Complex Double array of Sequence Voltages (0, 1, 2) at this Bus.
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
        double Cust_Duration() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Bus_Get_Cust_Duration(ctx);
        }

        /// 
        /// Annual number of customer-interruptions from this bus
        /// 
        double Cust_Interrupts() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Bus_Get_Cust_Interrupts(ctx);
        }

        /// 
        /// Distance from energymeter (if non-zero)
        /// 
        double Distance() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Bus_Get_Distance(ctx);
        }

        /// 
        /// Average interruption duration, hr.
        /// 
        double Int_Duration() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Bus_Get_Int_Duration(ctx);
        }

        /// 
        /// Short circuit currents at bus; Complex Array.
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
        double Lambda() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Bus_Get_Lambda(ctx);
        }

        /// 
        /// Total numbers of customers served downline from this bus
        /// 
        int32_t N_Customers() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Bus_Get_N_Customers(ctx);
        }

        /// 
        /// Number of interruptions this bus per year
        /// 
        double N_interrupts() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Bus_Get_N_interrupts(ctx);
        }

        /// 
        /// Name of Bus
        /// 
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Bus_Get_Name(ctx);
        }

        /// 
        /// Integer Array of Node Numbers defined at the bus in same order as the voltages.
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
        int32_t NumNodes() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Bus_Get_NumNodes(ctx);
        }

        /// 
        /// Integer ID of the feeder section in which this bus is located.
        /// 
        int32_t SectionID() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Bus_Get_SectionID(ctx);
        }

        /// 
        /// Double Array of sequence voltages at this bus.
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
        double TotalMiles() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Bus_Get_TotalMiles(ctx);
        }

        /// 
        /// For 2- and 3-phase buses, returns array of complex numbers represetin L-L voltages in volts. Returns -1.0 for 1-phase bus. If more than 3 phases, returns only first 3.
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT VLL() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Bus_Get_VLL_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Array of doubles containing voltages in Magnitude (VLN), angle (deg) 
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
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Zsc0() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Bus_Get_Zsc0_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Complex Positive-Sequence short circuit impedance at bus..
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
        double kVBase() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Bus_Get_kVBase(ctx);
        }

        /// 
        /// Returns Complex array of pu L-L voltages for 2- and 3-phase buses. Returns -1.0 for 1-phase bus. If more than 3 phases, returns only 3 phases.
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT puVLL() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Bus_Get_puVLL_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Array of doubles containig voltage magnitude, angle pairs in per unit
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
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT puVoltages() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Bus_Get_puVoltages_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Array of doubles (complex) containing the complete 012 Zsc matrix
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT ZSC012Matrix() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Bus_Get_ZSC012Matrix_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// X Coordinate for bus (double)
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
        /// Y coordinate for bus(double)
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
        strings LoadList() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Bus_Get_LoadList);
        }

        /// 
        /// List of strings: Full Names of LINE elements connected to the active bus.
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

        ICNData(dss::APIUtil *util) :
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

        int32_t GMRUnits() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CNData_Get_GMRUnits(ctx);
        }
        ICNData& GMRUnits(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CNData_Set_GMRUnits(ctx, value);
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

        int32_t RadiusUnits() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CNData_Get_RadiusUnits(ctx);
        }
        ICNData& RadiusUnits(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CNData_Set_RadiusUnits(ctx, value);
            return *this;
        }

        int32_t ResistanceUnits() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CNData_Get_ResistanceUnits(ctx);
        }
        ICNData& ResistanceUnits(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CNData_Set_ResistanceUnits(ctx, value);
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

        ICapControls(dss::APIUtil *util) :
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
        void Reset()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CapControls_Reset(ctx);
        }

        /// 
        /// Transducer ratio from pirmary current to control current.
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
        /// Time delay [s] before swithcing off a step. Control may reset before actually switching.
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
        int32_t Mode() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CapControls_Get_Mode(ctx);
        }
        ICapControls& Mode(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CapControls_Set_Mode(ctx, value);
            return *this;
        }

        /// 
        /// Full name of the element that PT and CT are connected to.
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
        bool UseVoltOverride() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_CapControls_Get_UseVoltOverride(ctx) != 0);
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

        ICapacitors(dss::APIUtil *util) :
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
            return (ctx_Capacitors_AddStep(ctx) != 0);
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
            return (ctx_Capacitors_SubtractStep(ctx) != 0);
        }

        /// 
        /// Number of Steps available in cap bank to be switched ON.
        /// 
        int32_t AvailableSteps() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Capacitors_Get_AvailableSteps(ctx);
        }

        /// 
        /// Delta connection or wye?
        /// 
        bool IsDelta() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_Capacitors_Get_IsDelta(ctx) != 0);
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
        /// A array of  integer [0..numsteps-1] indicating state of each step. If the read value is -1 an error has occurred.
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

        ICtrlQueue(dss::APIUtil *util) :
            ContextState(util)
        {
        }
        void ClearActions()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CtrlQueue_ClearActions(ctx);
        }
        void ClearQueue()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CtrlQueue_ClearQueue(ctx);
        }
        void Delete(int32_t ActionHandle)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CtrlQueue_Delete(ctx, ActionHandle);
        }
        void DoAllQueue()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CtrlQueue_DoAllQueue(ctx);
        }
        void Show()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CtrlQueue_Show(ctx);
        }

        /// 
        /// Code for the active action. Long integer code to tell the control device what to do
        /// 
        int32_t ActionCode() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CtrlQueue_Get_ActionCode(ctx);
        }

        /// 
        /// Handle (User defined) to device that must act on the pending action.
        /// 
        int32_t DeviceHandle() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CtrlQueue_Get_DeviceHandle(ctx);
        }

        /// 
        /// Number of Actions on the current actionlist (that have been popped off the control queue by CheckControlActions)
        /// 
        int32_t NumActions() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CtrlQueue_Get_NumActions(ctx);
        }
        /// 
        /// Push a control action onto the DSS control queue by time, action code, and device handle (user defined). Returns Control Queue handle.
        /// 
        int32_t Push(int32_t Hour, double Seconds, int32_t ActionCode, int32_t DeviceHandle)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CtrlQueue_Push(ctx, Hour, Seconds, ActionCode, DeviceHandle);
        }

        /// 
        /// Pops next action off the action list and makes it the active action. Returns zero if none.
        /// 
        int32_t PopAction() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CtrlQueue_Get_PopAction(ctx);
        }

        /// 
        /// Array of strings containing the entire queue in CSV format
        /// 
        strings Queue() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_CtrlQueue_Get_Queue);
        }

        /// 
        /// Number of items on the OpenDSS control Queue
        /// 
        int32_t QueueSize() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CtrlQueue_Get_QueueSize(ctx);
        }

        /// 
        /// (write-only) Set the active action by index
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

        IDSSElement(dss::APIUtil *util) :
            ContextState(util),
            Properties(util)
        {
        }

        /// 
        /// Array of strings containing the names of all properties for the active DSS object.
        /// 
        strings AllPropertyNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_DSSElement_Get_AllPropertyNames);
        }

        /// 
        /// Full Name of Active DSS Object (general element or circuit element).
        /// 
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSSElement_Get_Name(ctx);
        }

        /// 
        /// Number of Properties for the active DSS object.
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
        /// (API Extension)
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

        ILineGeometries(dss::APIUtil *util) :
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
            return (ctx_LineGeometries_Get_Reduce(ctx) != 0);
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

        ILines(dss::APIUtil *util) :
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
        int32_t New(const char *Name)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_New(ctx, Name);
        }
        int32_t New(const string &Name)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_New(ctx, Name.c_str());
        }

        /// 
        /// Name of bus for terminal 1.
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
        int32_t NumCust() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_Get_NumCust(ctx);
        }

        /// 
        /// Sets Parent of the active Line to be the active line. Returns 0 if no parent or action fails.
        /// 
        int32_t Parent() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_Get_Parent(ctx);
        }

        /// 
        /// Number of Phases, this Line element.
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
        int32_t TotalCust() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_Get_TotalCust(ctx);
        }

        int32_t Units() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_Get_Units(ctx);
        }
        ILines& Units(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Lines_Set_Units(ctx, value);
            return *this;
        }

        /// 
        /// Zero Sequence reactance ohms per unit length.
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
        /// Yprimitive: Does Nothing at present on Put; Dangerous
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
        double SeasonRating() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Lines_Get_SeasonRating(ctx);
        }

        /// 
        /// Sets/gets the Line element switch status. Setting it has side-effects to the line parameters.
        /// 
        bool IsSwitch() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_Lines_Get_IsSwitch(ctx) != 0);
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

        ILoads(dss::APIUtil *util) :
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
        /// Factor relates average to peak kw.  Used for allocation with kwh and kwhdays
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
        bool IsDelta() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_Loads_Get_IsDelta(ctx) != 0);
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
        int32_t Model() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_Model(ctx);
        }
        ILoads& Model(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_Model(ctx, value);
            return *this;
        }

        /// 
        /// Number of customers in this load, defaults to one.
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
        /// Relative Weighting factor for the active LOAD
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
        /// Name of harmonic current spectrrum shape.
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
        int32_t Status() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_Status(ctx);
        }
        ILoads& Status(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Loads_Set_Status(ctx, value);
            return *this;
        }

        /// 
        /// Maximum per-unit voltage to use the load model. Above this, constant Z applies.
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
        /// Set kV rating for active Load. For 2 or more phases set Line-Line kV. Else actual kV across terminals.
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
        /// Get/set kvar for active Load. If set, updates PF based on present kW.
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
        /// kwh billed for this period. Can be used with Cfactor for load allocation.
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
        /// Length of kwh billing period for average demand calculation. Default 30.
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
        string Sensor() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Loads_Get_Sensor(ctx);
        }

        /// 
        /// Number of phases
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

        ISettings(dss::APIUtil *util) :
            ContextState(util)
        {
        }

        /// 
        /// {True | False*} Designates whether to allow duplicate names of objects
        /// 
        bool AllowDuplicates() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_Settings_Get_AllowDuplicates(ctx) != 0);
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
        /// {dssMultiphase (0) * | dssPositiveSeq (1) } Indicate if the circuit model is positive sequence.
        /// 
        int32_t CktModel() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Settings_Get_CktModel(ctx);
        }
        ISettings& CktModel(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Settings_Set_CktModel(ctx, value);
            return *this;
        }

        /// 
        /// {True | False*} Denotes whether to trace the control actions to a file.
        /// 
        bool ControlTrace() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_Settings_Get_ControlTrace(ctx) != 0);
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
        /// {True | False *} Gets value of trapezoidal integration flag in energy meters.
        /// 
        bool Trapezoidal() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_Settings_Get_Trapezoidal(ctx) != 0);
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
        /// {True | False*}  Locks Zones on energy meters to prevent rebuilding if a circuit change occurs.
        /// 
        bool ZoneLock() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_Settings_Get_ZoneLock(ctx) != 0);
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
        /// (API Extension)
        /// 
        bool LoadsTerminalCheck() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_Settings_Get_LoadsTerminalCheck(ctx) != 0);
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
        /// (API Extension)
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
    };

    class ICktElement: public ContextState
    {
    public:
        IDSSProperty Properties;

        ICktElement(dss::APIUtil *util) :
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
        bool IsOpen(int32_t Term, int32_t Phs)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_CktElement_IsOpen(ctx, Term, Phs) != 0);
        }
        void Open(int32_t Term, int32_t Phs)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Open(ctx, Term, Phs);
        }

        /// 
        /// Array containing all property names of the active device.
        /// 
        strings AllPropertyNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_CktElement_Get_AllPropertyNames);
        }

        /// 
        /// Array of strings listing all the published variable names, if a PCElement. Otherwise, null string.
        /// 
        strings AllVariableNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_CktElement_Get_AllVariableNames);
        }

        /// 
        /// Array of doubles. Values of state variables of active element if PC element.
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT AllVariableValues() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Get_AllVariableValues_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Array of strings. Get  Bus definitions to which each terminal is connected.
        /// 
        strings BusNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_CktElement_Get_BusNames);
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
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Currents() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Get_Currents_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Currents in magnitude, angle format as a array of doubles.
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
        bool Enabled() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_CktElement_Get_Enabled(ctx) != 0);
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
        string EnergyMeter() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CktElement_Get_EnergyMeter(ctx);
        }

        /// 
        /// globally unique identifier for this object
        /// 
        string GUID() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CktElement_Get_GUID(ctx);
        }

        /// 
        /// Pointer to this object
        /// 
        int32_t Handle() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CktElement_Get_Handle(ctx);
        }

        /// 
        /// True if a recloser, relay, or fuse controlling this ckt element. OCP = Overcurrent Protection 
        /// 
        bool HasOCPDevice() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_CktElement_Get_HasOCPDevice(ctx) != 0);
        }

        /// 
        /// This element has a SwtControl attached.
        /// 
        bool HasSwitchControl() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_CktElement_Get_HasSwitchControl(ctx) != 0);
        }

        /// 
        /// This element has a CapControl or RegControl attached.
        /// 
        bool HasVoltControl() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_CktElement_Get_HasVoltControl(ctx) != 0);
        }

        /// 
        /// Total losses in the element: two-element complex array
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Losses() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Get_Losses_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Full Name of Active Circuit Element
        /// 
        string Name() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CktElement_Get_Name(ctx);
        }

        /// 
        /// Array of integer containing the node numbers (representing phases, for example) for each conductor of each terminal. 
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
        int32_t NumConductors() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CktElement_Get_NumConductors(ctx);
        }

        /// 
        /// Number of controls connected to this device. 
        /// Use to determine valid range for index into Controller array.
        /// 
        int32_t NumControls() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CktElement_Get_NumControls(ctx);
        }

        /// 
        /// Number of Phases
        /// 
        int32_t NumPhases() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CktElement_Get_NumPhases(ctx);
        }

        /// 
        /// Number of Properties this Circuit Element.
        /// 
        int32_t NumProperties() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CktElement_Get_NumProperties(ctx);
        }

        /// 
        /// Number of Terminals this Circuit Element
        /// 
        int32_t NumTerminals() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CktElement_Get_NumTerminals(ctx);
        }

        /// 
        /// Index into Controller list of OCP Device controlling this CktElement
        /// 
        int32_t OCPDevIndex() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CktElement_Get_OCPDevIndex(ctx);
        }

        /// 
        /// 0=None; 1=Fuse; 2=Recloser; 3=Relay;  Type of OCP controller device
        /// 
        int32_t OCPDevType() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_CktElement_Get_OCPDevType(ctx);
        }

        /// 
        /// Complex array of losses by phase
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT PhaseLosses() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Get_PhaseLosses_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Complex array of powers into each conductor of each terminal
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Powers() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Get_Powers_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Residual currents for each terminal: (mag, angle)
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Residuals() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Get_Residuals_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Double array of symmetrical component currents into each 3-phase terminal
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT SeqCurrents() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Get_SeqCurrents_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Double array of sequence powers into each 3-phase teminal
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT SeqPowers() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Get_SeqPowers_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Double array of symmetrical component voltages at each 3-phase terminal
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
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT VoltagesMagAng() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Get_VoltagesMagAng_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// YPrim matrix, column order, complex numbers (paired)
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT Yprim() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_CktElement_Get_Yprim_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Returns true if the current active element is isolated.
        /// Note that this only fetches the current value. See also the Topology interface.
        /// 
        bool IsIsolated() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_CktElement_Get_IsIsolated(ctx) != 0);
        }

        /// 
        /// Returns the total powers (complex) at ALL terminals of the active circuit element.
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

        IGICSources(dss::APIUtil *util) :
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

        IStorages(dss::APIUtil *util) :
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
        /// Related enumeration: StorageStates
        /// 
        int32_t State() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Storages_Get_State(ctx);
        }
        IStorages& State(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Storages_Set_State(ctx, value);
            return *this;
        }

        /// 
        /// Array of Names of all Storage energy meter registers
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
    };

    class ISolution: public ContextState
    {
    public:

        ISolution(dss::APIUtil *util) :
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
        /// Base Solution algorithm: {dssNormalSolve | dssNewtonSolve}
        /// 
        int32_t Algorithm() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_Algorithm(ctx);
        }
        ISolution& Algorithm(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_Algorithm(ctx, value);
            return *this;
        }

        /// 
        /// Capacitor kvar for adding capacitors in AutoAdd mode
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
        bool ControlActionsDone() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_Solution_Get_ControlActionsDone(ctx) != 0);
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
        /// {dssStatic* | dssEvent | dssTime}  Modes for control devices
        /// 
        int32_t ControlMode() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_ControlMode(ctx);
        }
        ISolution& ControlMode(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_ControlMode(ctx, value);
            return *this;
        }

        /// 
        /// Flag to indicate whether the circuit solution converged
        /// 
        bool Converged() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_Solution_Get_Converged(ctx) != 0);
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
        strings EventLog() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Solution_Get_EventLog);
        }

        /// 
        /// Set the Frequency for next solution
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
        /// Number of iterations taken for last solution. (Same as TotalIterations)
        /// 
        int32_t Iterations() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_Iterations(ctx);
        }

        /// 
        /// Load-Duration Curve name for LD modes
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
        /// Set present solution mode (by a text code - see DSS Help)
        /// 
        int32_t Mode() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_Mode(ctx);
        }
        ISolution& Mode(int32_t value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Set_Mode(ctx, value);
            return *this;
        }

        /// 
        /// ID (text) of the present solution mode
        /// 
        string ModeID() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_ModeID(ctx);
        }

        /// 
        /// Max number of iterations required to converge at any control iteration of the most recent solution.
        /// 
        int32_t MostIterationsDone() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_MostIterationsDone(ctx);
        }

        /// 
        /// Number of solutions to perform for Monte Carlo and time series simulations
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
        double Process_Time() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_Process_Time(ctx);
        }

        /// 
        /// Randomization mode for random variables "Gaussian" or "Uniform"
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
        bool SystemYChanged() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_Solution_Get_SystemYChanged(ctx) != 0);
        }

        /// 
        /// Get the solution process time + sample time for time step
        /// 
        double Time_of_Step() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_Time_of_Step(ctx);
        }

        /// 
        /// Solution convergence tolerance.
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
        int32_t Totaliterations() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Solution_Get_Totaliterations(ctx);
        }

        /// 
        /// Set year for planning studies
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

        template <typename VectorT=Eigen::Matrix<int32_t, Eigen::Dynamic, 1>>
        VectorT BusLevels() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Get_BusLevels_GR(ctx);
            return api_util->get_int32_gr_array<VectorT>();
        }

        template <typename VectorT=Eigen::Matrix<int32_t, Eigen::Dynamic, 1>>
        VectorT IncMatrix() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Solution_Get_IncMatrix_GR(ctx);
            return api_util->get_int32_gr_array<VectorT>();
        }

        strings IncMatrixCols() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Solution_Get_IncMatrixCols);
        }

        strings IncMatrixRows() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Solution_Get_IncMatrixRows);
        }

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
        IParallel Parallel;

        ICircuit(dss::APIUtil *util) :
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
        double Capacity(double Start, double Increment)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_Capacity(ctx, Start, Increment);
        }
        void Disable(const char *Name)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_Disable(ctx, Name);
        }
        void Disable(const string &Name)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_Disable(ctx, Name.c_str());
        }
        void Enable(const char *Name)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_Enable(ctx, Name);
        }
        void Enable(const string &Name)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_Enable(ctx, Name.c_str());
        }
        void EndOfTimeStepUpdate()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_EndOfTimeStepUpdate(ctx);
        }
        int32_t FirstElement()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_FirstElement(ctx);
        }
        int32_t FirstPCElement()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_FirstPCElement(ctx);
        }
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
        int32_t NextElement()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_NextElement(ctx);
        }
        int32_t NextPCElement()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_NextPCElement(ctx);
        }
        int32_t NextPDElement()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_NextPDElement(ctx);
        }
        void Sample()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_Sample(ctx);
        }
        void SaveSample()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_SaveSample(ctx);
        }
        int32_t SetActiveBus(const char *BusName)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_SetActiveBus(ctx, BusName);
        }
        int32_t SetActiveBus(const string &BusName)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_SetActiveBus(ctx, BusName.c_str());
        }
        int32_t SetActiveBusi(int32_t BusIndex)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_SetActiveBusi(ctx, BusIndex);
        }
        int32_t SetActiveClass(const char *ClassName)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_SetActiveClass(ctx, ClassName);
        }
        int32_t SetActiveClass(const string &ClassName)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_SetActiveClass(ctx, ClassName.c_str());
        }
        int32_t SetActiveElement(const char *FullName)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_SetActiveElement(ctx, FullName);
        }
        int32_t SetActiveElement(const string &FullName)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_SetActiveElement(ctx, FullName.c_str());
        }
        void UpdateStorage()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_UpdateStorage(ctx);
        }

        /// 
        /// Returns distance from each bus to parent EnergyMeter. Corresponds to sequence in AllBusNames.
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
        strings AllBusNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Circuit_Get_AllBusNames);
        }

        /// 
        /// Array of magnitudes (doubles) of voltages at all buses
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT AllBusVmag() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_Get_AllBusVmag_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Double Array of all bus voltages (each node) magnitudes in Per unit
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
        strings AllElementNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Circuit_Get_AllElementNames);
        }

        /// 
        /// Returns an array of distances from parent EnergyMeter for each Node. Corresponds to AllBusVMag sequence.
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
        strings AllNodeNames() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Circuit_Get_AllNodeNames);
        }

        /// 
        /// Complex total line losses in the circuit
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
        int32_t NumBuses() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_Get_NumBuses(ctx);
        }

        /// 
        /// Number of CktElements in the circuit.
        /// 
        int32_t NumCktElements() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_Get_NumCktElements(ctx);
        }

        /// 
        /// Total number of nodes in the circuit.
        /// 
        int32_t NumNodes() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_Get_NumNodes(ctx);
        }

        /// 
        /// Sets Parent PD element, if any, to be the active circuit element and returns index>0; Returns 0 if it fails or not applicable.
        /// 
        int32_t ParentPDElement() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_Circuit_Get_ParentPDElement(ctx);
        }

        /// 
        /// Complex losses in all transformers designated to substations.
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
        /// For large scale systems, prefer YMatrix.GetCompressedYMatrix.
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT SystemY() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_Get_SystemY_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Total power, kW delivered to the circuit
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>>
        VectorT TotalPower() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_Get_TotalPower_GR(ctx);
            return api_util->get_float64_gr_array<VectorT>();
        }

        /// 
        /// Array of doubles containing complex injection currents for the present solution. Is is the "I" vector of I=YV
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
        strings YNodeOrder() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_Circuit_Get_YNodeOrder);
        }

        /// 
        /// Complex array of actual node voltages in same order as SystemY matrix.
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
        /// (API Extension)
        /// 
        template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>, typename InVectorT=Eigen::Matrix<int32_t, Eigen::Dynamic, 1>>
        VectorT ElementLosses(const InVectorT &value)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_Circuit_Get_ElementLosses_GR(ctx, &value[0], value.size());
            return api_util->get_float64_gr_array<VectorT>();
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

        IDSS(dss::APIUtil *util, bool owns=false) :
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
    
        void ClearAll()
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_DSS_ClearAll(ctx);
        }
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
        bool Start(int32_t code)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_DSS_Start(ctx, code) != 0);
        }

        /// 
        /// List of DSS intrinsic classes (names of the classes)
        /// 
        strings Classes() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_DSS_Get_Classes);
        }

        /// 
        /// DSS Data File Path.  Default path for reports, etc. from DSS
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
        string DefaultEditor() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSS_Get_DefaultEditor(ctx);
        }

        /// 
        /// Number of Circuits currently defined
        /// 
        int32_t NumCircuits() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSS_Get_NumCircuits(ctx);
        }

        /// 
        /// Number of DSS intrinsic classes
        /// 
        int32_t NumClasses() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSS_Get_NumClasses(ctx);
        }

        /// 
        /// Number of user-defined classes
        /// 
        int32_t NumUserClasses() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSS_Get_NumUserClasses(ctx);
        }

        /// 
        /// List of user-defined classes
        /// 
        strings UserClasses() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return api_util->get_string_array(ctx_DSS_Get_UserClasses);
        }

        /// 
        /// Get version string for the DSS.
        /// 
        string Version() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return ctx_DSS_Get_Version(ctx);
        }

        /// 
        /// Gets/sets whether text output is allowed
        /// 
        bool AllowForms() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_DSS_Get_AllowForms(ctx) != 0);
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
        /// (API Extension)
        /// 
        bool AllowEditor() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_DSS_Get_AllowEditor(ctx) != 0);
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
            // #warning ("ShowPanel is not implemented.");
        }
        ICircuit& NewCircuit(const char *name)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_DSS_NewCircuit(ctx, name);
            return ActiveCircuit;
        }
        ICircuit& NewCircuit(const string &name)
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_DSS_NewCircuit(ctx, name.c_str());
            return ActiveCircuit;
        }

        /// 
        /// If enabled, the legacy/deprecated models for PVSystem, InvControl, Storage and StorageControl are used.
        /// In the official OpenDSS version 9.0, the old models where removed. They are temporarily present here
        /// but may be removed in the near future. If they are important to you, please open an issue on GitHub
        /// or contact the authors from DSS Extensions: https://github.com/dss-extensions/
        /// 
        /// After toggling LegacyModels, run a "clear" command and the models will be loaded accordingly.
        /// Defaults to False. 
        /// 
        /// This can also be enabled by setting the environment variable DSS_CAPI_LEGACY_MODELS to 1.
        /// 
        /// NOTE: this option will be removed in a future release.
        /// 
        /// (API Extension)
        /// 
        bool LegacyModels() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_DSS_Get_LegacyModels(ctx) != 0);
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
        /// (API Extension)
        /// 
        bool AllowChangeDir() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_DSS_Get_AllowChangeDir(ctx) != 0);
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
        /// Defaults to False/0 (disabled state). Users should consider DOScmd deprecated on DSS Extensions.
        /// 
        /// This can also be set through the environment variable DSS_CAPI_ALLOW_DOSCMD. Setting it to 1 enables
        /// the command.
        /// 
        /// (API Extension)
        /// 
        bool AllowDOScmd() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_DSS_Get_AllowDOScmd(ctx) != 0);
        }
        IDSS& AllowDOScmd(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_DSS_Set_AllowDOScmd(ctx, value);
            return *this;
        }

        /// 
        /// If enabled, in case of errors or empty arrays, the API returns arrays with values compatible with the 
        /// official OpenDSS COM interface. 
        /// 
        /// For example, consider the function `Loads_Get_ZIPV`. If there is no active circuit or active load element:
        /// - In the disabled state (COMErrorResults=False), the function will return "[]", an array with 0 elements.
        /// - In the enabled state (COMErrorResults=True), the function will return "[0.0]" instead. This should
        /// be compatible with the return value of the official COM interface.
        /// 
        /// Defaults to True/1 (enabled state) in the v0.12.x series. This will change to false in future series.
        /// 
        /// This can also be set through the environment variable DSS_CAPI_COM_DEFAULTS. Setting it to 0 disables
        /// the legacy/COM behavior. The value can be toggled through the API at any time.
        /// 
        /// (API Extension)
        /// 
        bool COMErrorResults() // getter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            return (ctx_DSS_Get_COMErrorResults(ctx) != 0);
        }
        IDSS& COMErrorResults(bool value) // setter
        {
            APIUtil::ErrorChecker error_checker(api_util);
            ctx_DSS_Set_COMErrorResults(ctx, value);
            return *this;
        }

    };

} } // namespace dss::classic
#endif // #ifndef DSS_CPP_CLASSIC_API
