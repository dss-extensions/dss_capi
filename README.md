[![Travis-CI: Linux and MacOS build status](https://travis-ci.com/dss-extensions/dss_capi.svg?branch=master)](https://travis-ci.com/dss-extensions/dss_capi)
[![AppVeyor: Windows build status](https://ci.appveyor.com/api/projects/status/eekf7ry9d0cy4k1b?svg=true)](https://ci.appveyor.com/project/PMeira/dss-capi-u9ben)

*Para uma versão em português deste arquivo, veja [README.pt-BR.md](https://github.com/dss-extensions/dss_capi/blob/master/README.pt-BR.md).*

# DSS C-API: An unofficial C API for EPRI's OpenDSS

This library exposes the OpenDSS/OpenDSS-PM (OpenDSS Version 7 and Version 8, respectively) engine in a plain C interface that tries to reproduce most of the COM methods. In fact, most of the code is derived from the COM implementation files. The resulting DLL can be using directly or through the `dss_python` module in Python, a module that mimics the COM structure (as exposed via `win32com` or `comtypes`), effectively enabling multi-platform compatibility at Python level. There is also support for some other programming languages — if you need support for a language not listed below, please open a new issue and we will evaluate that language.

<p align="center">
    <img alt="Overview of related repositories" src="https://raw.githubusercontent.com/dss-extensions/dss_capi/master/docs/images/repomap.png" width=600>
</p>

If you are looking for the bindings to other languages:

- [DSS Python](http://github.com/dss-extensions/dss_python/) is a multi-platform Python module (Windows, Linux, MacOS) very compatible with the original COM DLL. See also [OpenDSSDirect.py](http://github.com/dss-extensions/OpenDSSDirect.py/) if you don't need COM compatibility, or just would like to check its extra functionalities (you can mix DSS Python and OpenDSSDirect.py).
- [OpenDSSDirect.jl](http://github.com/dss-extensions/OpenDSSDirect.jl/) is a Julia module, created by Tom Short (@tshort), recently migrated with the help of Dheepak Krishnamurthy (@kdheepak) to DSS C-API instead of the DDLL.
- [DSS Sharp](http://github.com/dss-extensions/dss_sharp/) is available for .NET/C#, also mimics the COM classes, but Windows-only at the moment. Soon it will be possible to use it via COM too.
- [DSS MATLAB](http://github.com/dss-extensions/dss_sharp/) presents multi-platform integration (Windows, Linux, MacOS) with DSS C-API and is also very compatible bastante with the COM classes.

Version 0.10.3, based on OpenDSS revision 2609 (which is slightly newer than OpenDSS v8.5.9.1 and v7.6.5.86)

While the main objective of COM compatibility has been reach, this is still a work-in-progress and is subject to changes. 
*Note that, while the interface with OpenDSS is stable (v7, classic version), the OpenDSS-PM (v8, actor-based parallel machine version) interface is experimental in our builds.* From version 0.10, the v8 interface is a lot more stable than in 0.9.8.

Instead of using extra numeric parameters as in the official DDLL interface, each original COM property is exposed as a pair of functions. For example, the load kVA property is exposed as:

```
    double Loads_Get_kva();
    void Loads_Set_kva(double Value);
```

Besides low-level details such as memory management, most of the COM documentation can be used as-is. 

**Starting in version 0.9.8, we disabled the `opendsscmd.ini` creation. You can set the default base frequency using the environment variable DSS_BASE_FREQUENCY, or just set it in the DSS scripts (recommended). This also means that the initial datapath is set to the current working directory.**

Since 2019-03-05, the `dss_capi` repository contains all the Pascal code used to build DSS C-API. The API code is at the `src/` folder, while the main OpenDSS code is kept in `Version7/` and `Version8/`. The upstream/official code is kept in the branch named `opendss-official-svn` and is periodically merged -- see also the [upstream branch](https://github.com/dss-extensions/dss_capi/blob/master/docs/upstream_branch.md) document.

## Recent changes

See [the changelog](https://github.com/dss-extensions/dss_capi/blob/master/docs/changelog.md) for a detailed list.

- **2019-05-22 / version 0.10.3: Some important fixes, better general performance, new API extensions, new features ported from COM and the OpenDSS version 8 codebase.**
- 2019-03-05: the Git repository `electricdss-src` was merged into `dss_capi`.
- 2019-02-28 / version 0.10.2: Highlights: implements the missing `CtrlQueue_Push`; reworks `LoadShapes` for performance and validation; introduces `DSS_Get_AllowEditor`/`DSS_Set_AllowEditor` to toggle the editor calls.**
- 2019-02-12 / version 0.10.1: Highlights: more error checking, introduction of `Error_Get_NumberPtr`, fixes and better handling of Meters.
- 2018-11-17 / version 0.10.0: Reduce memory allocations if the current buffers are reusable, introduce a Global Result mechanism, many API extensions (`LineGeometry`, `WireData`, `LineSpacing`, `CNData`, `TSData`, `Reactor`) -- see [the usage document](https://github.com/dss-extensions/dss_capi/blob/master/docs/usage.md) and the [issue ticket #11](https://github.com/dss-extensions/dss_capi/issues/11).
- 2018-08-10 / version 0.9.8: Major reorganization of the source code, many minor fixes, new building scripts.
- 2018-04-05 / version 0.9.5: New functions `Circuit_SetCktElement*` to set the active circuit element.
- 2018-03-06 / version 0.9.4: Includes fixes for DSSProperty, includes of the original helpstrings in the C header, integrate upstream changes up to revision 2152. This version introduces a first version of .NET bindings to the native DLL.
- 2018-02-16 / version 0.9.3: Integrates COM interface fixes from revision 2136 (`First` `Next` iteration)
- 2018-02-12 / version 0.9.2: Experimental support for OpenDSS-PM (at the moment, a custom patch is provided for Free Pascal support) and port COM interface fixes (OpenDSS revision 2134)
- 2018-02-08 / version 0.9.1: First public release (OpenDSS revision 2123)

## Missing features and limitations

- Currently not implemented:
    - `DSSEvents` from `DLL/ImplEvents.pas`: seems too dependent on COM.
    - Plotting in general
    
## Extra features

Besides most of the COM methods, some of the unique DDLL methods are also exposed in adapted forms, namely the methods from `DYMatrix.pas`, especially `GetCompressedYMatrix` (check the source files for more information). Also check the [list of known differences](https://github.com/dss-extensions/dss_capi/blob/master/docs/known_differences.md) for extra methods and options.

## Download

[See the releases page](https://github.com/dss-extensions/dss_capi/releases). Pre-compiled files are available for Windows, Linux and MacOS. 

## Building

To build the DLL yourself:

- Install the [Free Pascal compiler](https://freepascal.org/). If you have the Lazarus IDE installed, you most likely already have the compiler too. Add the folder containing the compiler (`fpc.exe`) to your PATH environment variable.

- Download the DSS-Extensions KLUSolve files, either build the library from source code or download the pre-built binaries from https://github.com/dss-extensions/klusolve

- Get this repository and the patched OpenDSS source code in the root folder:
```    
    git clone https://github.com/dss-extensions/dss_capi
```

- Merge the `lib/` output folder from KLUSolve into the `lib/` output folder in `dss_capi`


### On Windows

If you just need the DLL, you can download it from the releases page. Otherwise:

- Install the x64 Free Pascal compiler -- see [the wiki](http://wiki.freepascal.org/Installing_Lazarus#Installing_The_Free_Pascal_Compiler) for further instructions.

- If you want to use the DLL from Visual Studio, you need to generate an import library. This can be done by starting the next step from the "x64 Native Tools Command Prompt for VS 2017" (or equivalent for your Visual Studio version) -- you only need the `dumpbin.exe` and `lib.exe` utilities.

- Open a command prompt on the `dss_capi` folder and run `build_win_x64.bat`

The output files will be placed into the `lib/win_x64` folder. 

If you just need the DLLs, you can also find the x64 DLLs and LIBs in [the artifacts from the AppVeyor instance](https://ci.appveyor.com/project/dss-extensions/dss-capi/branch/master/artifacts). These files are built after each commit to this repository and are kept for 6 months.

### On Linux

If the provided DLLs are not compatible with your distribution, the current recommendation is to build your own KLUSolve, so you need to download and install its dependencies. Since most distributions should include compatible SuiteSparse packages (which include the KLU library), a modified version of KLUSolve is included in the `klusolve` subfolder. Overall instructions:

- Install the x64 Free Pascal compiler -- see [the wiki](http://wiki.freepascal.org/Installing_Lazarus#Installing_The_Free_Pascal_Compiler) for further instructions.

- Build the main project:
```
    bash build_linux_x64.sh
```

### On MacOS

After taking care of KLUSolve and placing a copy of it in the same folder, overall instructions:

- Install the x64 Free Pascal compiler -- see [the wiki](http://wiki.freepascal.org/Installing_Lazarus#Installing_The_Free_Pascal_Compiler) for further instructions.
- Build the main project:
```
    bash build_macos_x64.sh
```

## Usage and examples

To understand the main concepts of DSS C-API and how it handles memory, see [the usage document](https://github.com/dss-extensions/dss_capi/blob/master/docs/usage.md).

Two minimal samples (without DSS scripts, please bring your own) are available in [examples](examples). Add the appropriate subfolder from `include` and the library from `lib` to the compilation. 

The source code from DSS Python, OpenDSSDirect.py and OpenDSSDirect.jl are more complete and advanced examples of usage.

## Testing

Currently most testing/validation is based on [DSS Python](http://github.com/dss-extensions/dss_python/). Other projects like [OpenDSSDirect.py](http://github.com/dss-extensions/OpenDSSDirect.py/) and [OpenDSSDirect.jl](http://github.com/dss-extensions/OpenDSSDirect.jl/) also provide important tests that help us find and fix potential bugs.


## Roadmap

Besides bug fixes, the main funcionality of this library is mostly done. Notable desirable features that may be implemented are:
- Expose more classes and important methods/properties for all classes
- More and better documentation. We already integrated the help strings from the IDL/COM definition files in the header files.
- Automate validation of the Linux binaries (compare the outputs to the Windows version). Currently this is a manual process.
- C++ wrappers: Expose the API to C++ using namespaces for organization, overload methods, etc. We expect this as soon as the API gets stable.

Other features that may include more invasive changes in the code base will probably be developed in another repository.


## Questions?

If you have any question related to the deveopment this project, feel free to open a ticket on GitHub or contact me through email (pmeira at ieee.org).
Please allow me a few days to respond.


## Credits / Acknowledgment

This project is derived from EPRI's OpenDSS and the same style of license is used. See `LICENSE` and `OPENDSS_LICENSE`, also check each subfolder for more details.

Note that, since OpenDSS depends on KLU via KLUSolve, the KLU licensing conditions (LGPL or GPL, depending on how you build KLU) apply to the resulting binaries; from the DSS-Extension KLUSolve repository, check the files `klusolve/COPYING`, `klusolve/lgpl_2_1.txt`, the SuiteSparse documentation and the Eigen3 documentation.

Thanks to colleagues at the University of Campinas, Brazil, for providing feedback and helping me test this project, as well as everyone that reported issues and helped the development.
