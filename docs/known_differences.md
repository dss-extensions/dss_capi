# Known differences 

This document assumes some knowledge of the COM API and the basic model of DSS C-API.

## Important notes

- **This library is not supported by the original authors of OpenDSS**. If you find issues or have features specific to the library, please open [an issue ticket on GitHub](https://github.com/dss-extensions/dss_capi/issues/) or send an email (pmeira at ieee.org). For general OpenDSS issues, be sure to test with the official distribution before posting in the official forum at SourceForge. Users are welcome to post at [our own General Discussion page](https://github.com/orgs/dss-extensions/discussions/categories/general).
- Several projects that use DSS C-API, such as DSS-Python, OpenDSSDirect.py, OpenDSSDirect.jl, DSS Sharp and DSS MATLAB are hosted at [DSS-Extensions](http://github.com/dss-extensions/). These projects provide user-friendly programming interfaces for different languages. There are some non-affiliate projects that expose our engine to other languages.
- DSS C-API is currently based on the open-source Free Pascal compiler instead of Delphi. If you find a system where the results are different between the two distributions, please share a sample with us to help identify it, allowing us to fix the issue for all users.
- Bits and pieces of the API code, as well as some small specific changes of the main OpenDSS code, have been modified for better multi-platform compatibility and, sometimes, performance. Most of these are not explicitly listed here since they do not affect the library behavior.
- The API is compatible with languages which support C calls and is mostly self-documented in the header files (`dss_capi.h` in the `include` folder). See also [the usage document](https://github.com/dss-extensions/dss_capi/blob/master/docs/usage.md) to get understand the memory model and advanced usage — including the Global Result interface, which can drastically reduce memory (re)allocations in some use-cases.
- Most of the COM documentation can be applied to the usage of DSS C-API.
- At the moment, there is no executable program. If you have a use-case that would benefit from an executable, please open a feature request ticket. Some other features are been slowly added to DSS-Python to complement this library.

## Differences

- There is no direct support for plots from the DSS language or general GUI elements. As a library, DSS C-API can, of course, be integrate with GUI environments. Optional integration with plotting system is being implemented. For example, [DSS-Python provides optional plotting capabilities based on matplotlib](https://github.com/dss-extensions/dss_python/blob/master/docs/examples/Plotting.ipynb).
- The TCP connection options are disabled as well as GIS features (which require proprietary software not available to us).
- This library contains fixes for user-written DLLs ("UserModels" and "UserControls" for elements like Generators and CapUserControls) which include truncated pointers and correct support for the DSS language `Edit` command. Some of these have been fixed in the official version after some time, but at least the `Edit` issue persists.
- Free Pascal hashlists are used for tracking elements. This means that accessing elements by name is usually faster in DSS C-API and present a linear behavior. We still recommend using the element index where possible for better performance in general.
- A custom version of the KLUSolve library is used, KLUSolveX. It generalizes and replaces most of the original code to use the upstream SuiteSparse code and adds extra functions to achieve better performance. It is also compatible with MATLAB on Linux.
- Optimized (small, dense) matrix-vector multiplication.
- Settings like the default circuit frequency are not saved to disk. 
    - Unlike the OpenDSS COM API, DSS C-API doesn't save and change the current working directory when you start it. The `Compile` command will still change to the script directory though.
    - On Linux and macOS, the default editor can be customized using the `EDITOR` environment variable, which is a common convention for other software.
    - Besides using the DSS language (recommended), the default system frequency can be set using the environment variable `DSS_BASE_FREQUENCY` (integers only!).

- There are some new settings:
    - The `DSS_CAPI_ALLOW_EDITOR` environment variable (also controlled through `DSS_Set_AllowEditor`): defaults to `1` (true, set to `0` to disable),  controls whether DSS commands like `Show` can trigger the external editor. 
    - The `DSS_CAPI_EARLY_ABORT` environment variable (also controlled through `DSS_Set_EarlyAbort`):  Controls the DSS script error-handling behavior. If a warning or error occurs and early abortion is enabled (default), the processing of the script is always halted. Otherwise, the processing of the script continues until a major error occurs or it finishes.
    - `Settings_Set_LoadsTerminalCheck`/`Settings_Get_LoadsTerminalCheck`: Use with care! Controls whether the terminals are checked when updating the currents in Load component. Defaults to True (that is, no change from the official OpenDSS). If the loads are guaranteed to have their terminals closed throughout the simulation, this can be set to False to save some precious simulation time.
    - The `DSS_CAPI_EXTENDED_ERRORS` environment variable (also controlled through `Error_Set_ExtendedErrors`): If enabled (default is true since v0.10.6), the active element and active circuit checks use the Error interface to report issues. When disabled, the usual behavior from the official COM implementation is followed -- return a signaling or default value if appropriate.
    
- DSS C-API allow multiple OpenDSS instances in the same process. The instances are each one thread-safe, so multiple user threads can be used to coordinate them.

- The DSS property system in DSS C-API was completely reworked/rewritten. This allowed us to add the `Obj_*` and `Batch_*` API extensions. See DSS-Python's `DSS.Obj` implementation and the `dss_hpp` C++ header-only library for examples of the capabilities of these exclusive features. All JSON-based exports are also based on this new system.

- The symmetric component transformation matrix uses more decimal places (a bit more precise) and its inverse is also more precise. This might explain some small differences in systems that use sequence values.

- The DSS command `var` (which lists current DSS language variables) returns all variables in a string in the global result (`Text_Get_Result()`), which is equivalent to the official OpenDSS COM property `DSS.Text.Result`. In COM, this uses a message form (window).

- The DSS commands `dump buslist`, `dump commands` and `dump devicelist` are allowed to run when `AllowForms` is false. The user can disable the editor with `DSS_Set_AllowEditor` and still get the relevant results in the output text files. After each of the `dump` commands, the output file name is copied to the global result, enabling easier automation.

- `DynamicExp` objects, `DynamicEq` and `DynOut` properties have been ported in DSS C-API v0.13. 
    - The official implementation does not seen mature yet, but we ported already to avoid too much divergency in the features. As with most new features, the implementation is slightly different, especially the code organization, to avoid some potential issues. 
    - For consistency between the Monitor and the rest of the methods/APIs, when DynamicExp is used, the CktElement API and reports use the same variables as used in the Monitor.
    - For now, writing to a CktElement variable is disabled when using DynamicExp.

- Other API extensions include (see also the table at [DSS-Extensions — OpenDSS: Overview of Python APIs](https://github.com/dss-extensions/dss-extensions/blob/main/python_apis.md)):
    - Many `PDElements` functions: `AllNames`, `AllMaxCurrents`, `AllPctNorm`, `AllPctEmerg`, `AllCurrents`, `AllCurrentsMagAng`, `AllCplxSeqCurrents`, `AllSeqCurrents`, `AllPowers`, `AllSeqPowers`, `AllNumPhases`, `AllNumConductors`, `AllNumTerminals`.
    - Access by `idx` for many more DSS elements (21 new function pairs).
    - Experimental access to the following classes: `WireData`, `TSData`, `Reactors`, `LineSpacings`, `LineGeometries`, `CNData`.
    - `Error_Get_NumberPtr` returns a pointer to the global error number variable. This can be used to get for errors with lower overhead.
    - `Transformers_Get_LossesByType` and `Transformers_Get_AllLossesByType` return [total losses, load losses, no-load losses] for one or all transformers, respectively.
    - `Lines_Get_IsSwitch`/`Lines_Set_IsSwitch`
    - `Loads_Get_Phases`/`Loads_Set_Phases`
    - `XYCurves_Get_AllNames` (was missing)
    - `CktElement_Get_IsIsolated`

- **Notable omissions below. If you feel we should port them, please raise you concerns on our GitHub issue tracker.**
    - `Generic5` and `FMonitor` have not been ported yet due to lack of usage examples for validation. Please open an issue ticket on GitHub if you'd like us to port those.
    - `WindGen` is also not ported yet, waiting for it to mature.
    - The Diakoptics features are currently disabled. The implementation will be slightly different here since we use a completely different multi-circuit organization.

- Monitor headers: In the official OpenDSS, since May 2021, the monitor binary stream doesn't include the header anymore. When porting the change to DSS-Extensions, we took the opportunity to rewrite the related code. As such, the implementation in DSS-Extensions deviate from the official one. Extra spaces are not included and should be more consistent. As a recommendation, if your code needs to be compatible with both implementations, trimming the fields should be enough.

👉 Besides the omissions and caveats listed here, our general policy on commits/features from official OpenDSS is to port them in a matter of hours/days, if possible. If we find that a SVN commit/feature in the official OpenDSS introduces issues/bugs, we may postpone porting and/or report the issues in the official OpenDSS forum. Our releases are not coordinated with the official version.

Starting on version 0.13.0, we started introducing explicit compatibility flags for some behavior. See the docs for `DSS_Set_CompatFlags` and the `DSSCompatFlags` enum. From our header as of v0.13.3:

```c
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
            Toggle some InvControl behavior introduced in OpenDSS 9.6.1.1. It could be a regression 
            but needs further investigation, so we added this flag in the time being.
        */

       DSSCompatFlags_SaveCalcVoltageBases = 0x00000008, /*!< 
            When using "save circuit", the official OpenDSS always includes the "CalcVoltageBases" command in the
            saved script. We found that it is not always a good idea, so we removed the command (leaving it commented).
            Use this flag to enable the command in the saved script.
        */
    };
```
