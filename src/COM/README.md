# DSS-Extensions: AltDSS COM bridge for OpenDSS/AltDSS engines

This folder contains the source code for the DSS-Extensions COM DLL, introduced in April 2025.

This DLL does not implemented the engines, just expose existing C-compatible libraries to Microsoft's Component Object Model (COM), by using DSS-Extensions DSS C-API and Oddie subprojects.

## Why?

In short:

- Allow using modern and alternative OpenDSS engines in existing software that integrates COM.
- Potentially, simplify using different versions of OpenDSS. For example, instead of needing to register multiple DLLs, users can register `DSSExtensions.DLL` and load different engines are run-time. 

Since the projects on DSS-Extensions were first published, especially DSS C-API in 2018, users have inquired about COM. A lot of new users are confused and do not quite understand that AltDSS can be used without COM, but some users need an easy way to experiment replacing the engine in existing projects. 

By the end of 2023, EPRI changed their DCSL/OpenDSSDirect.DLL approach to use native pointers insted of VARIANT structs. This was one of the main limitations of EPRI's approach 

### Benefits

- Register the DLL once, use multiple engines! As long as the engine DLLs are compatible, they should work with an existing installation.
    - Try different engines, different feature sets, without rewriting all of your code!
- MSI-based installation packages for easy enterprise deployment.
- By default, the DSS engine errors are inspected and forwarded as COM errors. This can be disabled by setting `dssObj.Settings.UseExceptions` to false.

## When to use this

For example, some users and companies have custom integrations of OpenDSS using VBA (Visual Basic for Applications) in applications like Microsoft Excel or Microsoft Access.

Assuming the software is installed (see below), replace `OpenDSSengine.DSS` with `DSSExtensions.DSS`. Now, choose your engine:

- A default engine is loaded when the COM object is created. This engine is the AltDSS engine version at the time of release. If you'd like to use AltDSS, that's all you need to do.
- If you want to load a different version of AltDSS, load the target engine with `dssObj.LoadAltDSS('C:\path_to\altdss_capi.dll')`.
- If you want to use EPRI's OpenDSS (original Delphi implementation), we recommend first installing it (a user installation is fine). Load the engine with `dssObj.LoadOpenDSS('C:\path_to\OpenDSSDirect.dll')`.
- If you'd like to try EPRI's OpenDSS-C (C++ implementation), acquire the binaries and load the engine with `dssObj.LoadOpenDSS('C:\path_to\OpenDSSC.dll')`.

Remember: you can check which version is loaded using `dssObj.Version`.

## When **not** to use this

If you are developing software in the following languages, prefer using our native bindings instead. They are multi-platform support (Linux, macOS, Windows) and provide a consistent experience.

- C#/.NET: [DSS_Sharp](https://github.com/dss-extensions/dss_sharp/) is available since 2018. Nuget packages available! Currently supports switching between AltDSS and OpenDSS at build time. Can toggle OpenDSS DLLs are runtime.
- MATLAB: [DSS_MATLAB](https://github.com/dss-extensions/dss_matlab) is also available since 2018; note that DSS_Sharp can also be loaded in MATLAB. In 2025, DSS_MATLAB acquired FastDSS support, with a dedicated, fast MEX support module!
- C++: currently we have `altdss.hpp` to allow easy usage of AltDSS and OpenDSS in C++, even the Delphi implementation. [See examples here]() **TODO**.
- Julia: OpenDSSDirect.jl is a mature, broadly used Julia package that exposes AltDSS and OpenDSS engines.
- Python: if you need compatibility with the classic OpenDSS interface, DSS-Python (drop-in replacement for the win32com and comtypes with OpenDSS) and OpenDSSDirect.py are the best alternatives, both in terms of maturity, community and performance. Combined, these two packages already achieved more than 1.2 million downloads. Alternatively, if you are not restricted to EPRI's engines, try [AltDSS-Python for an reimagined, highly integrated Python experience](https://dss-extensions.org/AltDSS-Python/).
- Rust and Go (golang): [AltDSS-Rust](https://github.com/dss-extensions/AltDSS-Rust) and [AltDSS-Go](https://github.com/dss-extensions/AltDSS-Go) are readily available.

## Documentation

This project is intended as an almost drop-in replacement for the official `OpenDSSengine.DLL`. Different identifications is used to allow using both versions.

[The offical OpenDSS documentation by EPRI can be used](https://opendss.epri.com/COMInterface.html). The main different, after installation, is using `DSSExtensions` instead of `OpenDSSengine` to grab the interfaces.

Some extras from DSS-Python were also ported to this DLL, so some of the docs can be useful: https://dss-extensions.org/DSS-Python/#the-dss-instance -- notably, the ZIP and YMatrix interfaces were added. A list of convenience functions were also included.

Dedicated documentation pages will be provided in the future.

### Batches

**TODO**

## What's inside?

**TODO**

## Missing functionality

- The DSSEvents API was not implemented. Although it should be feasible to implement it for AltDSS, the direct DLL version of OpenDSS does not provide the framework required for that yet.
- Plotting and other functions are dependent of the engine used. If you need plotting via COM, we recommend using EPRI's OpenDSS (Delphi implementation) if you need plotting support. Alternatively, our Python packages do have plotting support.

## Installation

Either install the DSS

## Troubleshooting

- Loading DLLs?
    - If you downloaded an existing OpenDSSDirect.DLL or built a custom DLL of any of the engines yourself, antivirus software can block loading them. Be sure to adjust the antivirus to allow those files.
    - OpenDSSDirect.DLL and OpenDSSC.DLL are loaded by Oddie's strict mode, i.e., all expected functions must be present for it to be loaded.
    - Ensure your included all DLL dependencies in the DLL search path, typically in the same folder of the target DLL, or the general Windows path variable.
    
- Crashes?
    - Please report. Try to provide a minimal example that reproduces it.
    - Misuse of some APIs can crash or generate "access violation" errors. When in doubt, please feel free to ask.
    
- Errors?
    - A limited number of functions are not yet implement in EPRI's DCSL/OpenDSSDirect.DLL API. These should result in "not implemented" error messages.
    - Properties and methods marked as "API Extension" are items originally implemented in AltDSS only. We were able to port some to allow them in this COM module, but some will fail. Notably, the whole ZIP interface and some of the YMatrix methods are only available with the AltDSS engine.
