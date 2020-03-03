# Known differences 

This document assumes some knowledge of the COM API and the basic model of DSS C-API.

## Important notes

- This library is not supported by the original authors of OpenDSS. If you find issues or have features specific to the library, please open [an issue ticket on GitHub](https://github.com/dss-extensions/dss_capi/issues/) or send an email (pmeira at ieee.org). For general OpenDSS issues, be sure to test with the official distribution before posting in the official forum at SourceForge.
- Several projects that use DSS C-API, such as DSS Python, OpenDSSDirect.py, OpenDSSDirect.jl, DSS Sharp and DSS MATLAB are hosted at [DSS Extensions](http://github.com/dss-extensions/). These projects provide user-friendly programming interfaces for different languages.
- DSS C-API is based on the open-source Free Pascal compiler instead of Delphi. If you find a system where the results are different between the two distributions, please share a sample with us to help identify it, allowing us to fix the issue for all users.
- Bits and pieces of the API code, as well as some small specific changes of the main OpenDSS code, have been modified for better multi-platform compatibility and, sometimes, performance. Most of these are not explicitely listed here since they do not affect the library behavior.
- The API is compatible with languages which support C calls and is mostly self-documented in the header files (`dss_capi.h` in the `include` folder). See also [the usage document](https://github.com/dss-extensions/dss_capi/blob/master/docs/usage.md) to get understand the memory model and advanced usage -- including the Global Result interface, which can drastically reduce memory (re)allocations in some use-cases.
- Most of the COM documentation can be applied to the usage of DSS C-API.
- Like the main OpenDSS, DSS C-API is distributed as two main flavors: Version 7 and Version 8. Some features from OpenDSS Version 8 have been backported to the Version 7 code, as Version 7 is considered the main code for DSS C-API. We try to keep both versions in sync and always build the two interfaces, although Version 8 is less tested -- DSS C-API V8 is still considered experimental, use V7 if you don't need the new particular features that have not been ported from V8 (notably fully working Diakoptics and integrated Parallel Machine features).
- At the moment, there is no executable program. If you have a use-case that would benefit from an executable, please open a feature request ticket.

## Differences

- There is no direct support for plots from the DSS language or general GUI elements. As a library, DSS C-API can, of course, be integrate with GUI environments.
- The TCP connection options are disabled.
- This library contains fixes for user-written DLLs ("UserModels" and "UserControls" for elements like Generators and CapUserControls) which include truncated pointers and correct support for the DSS language `Edit` command.
- Free Pascal hashlists are used for tracking elements. This means that accessing elements by name is usually faster in DSS C-API and present a linear behavior.
- A custom version of the KLUSolve library is used. It generalizes the original code to use the upstream SuiteSparse code and adds extra functions to achieve better performance. It is also compatible with MATLAB on Linux.
- Optimized (small, dense) matrix-vector multiplication.
- Settings like the default circuit frequency are not saved to disk. 
    - Unlike the OpenDSS COM API, DSS C-API doesn't save and change the current working directory when you start it. The `Compile` command will still change to the script directory though.
    - On Linux and macOS, the default editor can be customized using the `EDITOR` environment variable, which is a common convention for other software.
    - Besides using the DSS language (recommended), the default system frequency can be set using the environment variable `DSS_BASE_FREQUENCY` (integers only!).

- There are some new settings:
    - The `DSS_CAPI_ALLOW_EDITOR` environment variable (also controlled through `DSS_Set_AllowEditor`): defaults to `1` (true, set to `0` to disable),  controls whether DSS commands like `Show` can trigger the external editor. 
    - The `DSS_CAPI_EARLY_ABORT` environment variable (also controlled through `DSS_Set_EarlyAbort`):  Controls the DSS script error-handling behavior. If a warning or error occurs and early abortion is enabled (default), the processing of the
    script is always halted. Otherwise, the processing of the script continues until a major error occurs or it finishes.
    - `Settings_Set_LoadsTerminalCheck`/`Settings_Get_LoadsTerminalCheck`: Use with care! Controls whether the terminals are checked when updating the currents in Load component. Defaults to True (that is, no change from the official OpenDSS). If the loads are guaranteed to have their terminals closed throughout the simulation, this can be set to False to save some precious simulation time.

- The symmetric component transformation matrix uses more decimal places (a bit more precise) and its inverse is also more precise. This might explain some small differences in systems that use sequence values.

- The DSS command `var` (which lists current DSS language variables) returns all variables in a string in the global result (`Text_Get_Result()`), which is equivalent to the COM property `DSS.Text.Result`. In COM, this uses a message form.

- The DSS commands `dump buslist`, `dump commands` and `dump devicelist` are allowed to run when `AllowForms` is false. The user can disable the editor with `DSS_Set_AllowEditor` and still get the relevant results in the output text files. After each of the `dump` commands, the output file name is copied to the global result, enabling easier automation.

- Notable changes ported from Version 8 to Version 7 include the following: 
    - `StorageController`: `PeakShaveLow` mode
    - DSS commands: `CalcIncMatrix`, `CalcIncMatrix_O`, `Refine_BusLevels`, `CalcLaplacian` -- also includes related API functions
    - Export options: `IncMatrix`, `IncMatrixRows`, `IncMatrixCols`, `BusLevels`, `Laplacian`, `ZLL`, `ZCC`, `Contours`, `Y4`
    - `XfmrCode`, `Transformer`: new Seasons and Ratings properties
    - 4 components: `PVsystem2`, `Storage2`, `InvControl2`, `StorageController2` -- *added for early testing, no dedicated API functions yet*. At the moment, please consider them experimental features subject to change.
    
- Other API extensions include:
    - Access by `idx` for many more DSS elements (21 new function pairs).
    - Experimental access to the following classes: `WireData`, `TSData`, `Reactors`, `LineSpacings`, `LineGeometries`, `CNData`.
    - `Error_Get_NumberPtr` returns a pointer to the global error number variable. This can be used to get for errors with lower overhead.
    - `Transformers_Get_LossesByType` and `Transformers_Get_AllLossesByType` return [total losses, load losses, no-load losses] for one or all transformers, respectively.
    - `Lines_Get_IsSwitch`/`Lines_Set_IsSwitch`
    - `Loads_Get_Phases`/`Loads_Set_Phases`
    - `XYCurves_Get_AllNames` (was missing)
    - `CktElement_Get_IsIsolated`

- Notable omissions:
    - `Generic5` and `FMonitor` have not been ported yet due to lack of usage examples for validation. Please open an issue ticket on GitHub if you'd like us to port those.
