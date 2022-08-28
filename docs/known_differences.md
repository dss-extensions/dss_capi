# Known differences 

This document assumes some knowledge of the COM API and the basic model of DSS C-API.

## Important notes

- **This library is not supported by the original authors of OpenDSS**. If you find issues or have features specific to the library, please open [an issue ticket on GitHub](https://github.com/dss-extensions/dss_capi/issues/) or send an email (pmeira at ieee.org). For general OpenDSS issues, be sure to test with the official distribution before posting in the official forum at SourceForge.
- Several projects that use DSS C-API, such as DSS Python, OpenDSSDirect.py, OpenDSSDirect.jl, DSS Sharp and DSS MATLAB are hosted at [DSS Extensions](http://github.com/dss-extensions/). These projects provide user-friendly programming interfaces for different languages.
- DSS C-API is based on the open-source Free Pascal compiler instead of Delphi. If you find a system where the results are different between the two distributions, please share a sample with us to help identify it, allowing us to fix the issue for all users.
- Bits and pieces of the API code, as well as some small specific changes of the main OpenDSS code, have been modified for better multi-platform compatibility and, sometimes, performance. Most of these are not explicitely listed here since they do not affect the library behavior.
- The API is compatible with languages which support C calls and is mostly self-documented in the header files (`dss_capi.h` in the `include` folder). See also [the usage document](https://github.com/dss-extensions/dss_capi/blob/master/docs/usage.md) to get understand the memory model and advanced usage -- including the Global Result interface, which can drastically reduce memory (re)allocations in some use-cases.
- Most of the COM documentation can be applied to the usage of DSS C-API.
- At the moment, there is no executable program. If you have a use-case that would benefit from an executable, please open a feature request ticket. Some other features are been slowly added to DSS Python to complement this library.

## Differences

- There is no direct support for plots from the DSS language or general GUI elements. As a library, DSS C-API can, of course, be integrate with GUI environments. Optional integration with plotting system is being implemented. For example, DSS Python provides optional plotting capabilites based on matplotlib.
- The TCP connection options are disabled as well as GIS features (which require proprietary software)
- This library contains fixes for user-written DLLs ("UserModels" and "UserControls" for elements like Generators and CapUserControls) which include truncated pointers and correct support for the DSS language `Edit` command.
- Free Pascal hashlists are used for tracking elements. This means that accessing elements by name is usually faster in DSS C-API and present a linear behavior.
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
    - The `DSS_CAPI_LEGACY_MODELS` environment variable (also controlled through `DSS_Set_LegacyModels`): If enabled (default is false since v0.10.6), the legacy/pre-OpenDSS 9.0 models for some components (`PVsystem`, `Storage`, `InvControl`, and `StorageController`) are used. If toggled at runtime through the API, a DSS command `clear` is issued and the component models are exchanged.

- DSS C-API allow multiple OpenDSS instances in the same process. The instances are each one thread-safe, so multiple user threads can be used to coordinate them.

- The DSS property system in DSS C-API was completely reworked/rewritten. This allowed us to add the `Obj_*` and `Batch_*` API extensions. See DSS Python's `DSS.Obj` implementation and the `dss_hpp` C++ header-only library for examples of the capabilites of these exclusive features. All JSON-based exports are also based on this new system.

- The symmetric component transformation matrix uses more decimal places (a bit more precise) and its inverse is also more precise. This might explain some small differences in systems that use sequence values.

- The DSS command `var` (which lists current DSS language variables) returns all variables in a string in the global result (`Text_Get_Result()`), which is equivalent to the official OpenDSS COM property `DSS.Text.Result`. In COM, this uses a message form.

- The DSS commands `dump buslist`, `dump commands` and `dump devicelist` are allowed to run when `AllowForms` is false. The user can disable the editor with `DSS_Set_AllowEditor` and still get the relevant results in the output text files. After each of the `dump` commands, the output file name is copied to the global result, enabling easier automation.

- Other API extensions include:
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
    - `WindGen` and `DynamicExp` (`DynamicEq`, `DynOut`) are also not ported yet, waiting for them to mature.
    - The Diakoptics features are currently disabled. The implementation will be slightly different here since we use a complete different multi-circuit organization.

- Monitor headers: In the official OpenDSS, since May 2021, the monitor binary stream doesn't include the header anymore. When porting the change to DSS Extensions, we took the opportunity to rewrite the related code. As such, the implementation in DSS Extensions deviate from the official one. Extra spaces are not included and should be more consistent. As a recommendation, if your code needs to be compatible with both implementations, trimming the fields should be enough.

👉 Besides the omissions and caveats listed here, our general policy on commits/features from official OpenDSS is to port them in a matter of hours/days, if possible. If we find that a SVN commit/feature in the official OpenDSS introduces issues/bugs, we may postpone porting and/or report the issues in the official OpenDSS forum. Our releases are not coordinated with the official version.