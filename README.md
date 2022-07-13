[![GitHub Actions: Builds](https://github.com/dss-extensions/dss_capi/actions/workflows/builds.yml/badge.svg)](https://github.com/dss-extensions/dss_capi/actions/workflows/builds.yml)
[![GitHub downloads](https://img.shields.io/github/downloads/dss-extensions/dss_capi/total?logo=GitHub&cacheSeconds=86400)](https://github.com/dss-extensions/dss_capi/releases)

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
- [DSS MATLAB](http://github.com/dss-extensions/dss_matlab/) presents multi-platform integration (Windows, Linux, MacOS) with DSS C-API and is also very compatible bastante with the COM classes.

Version 0.12.0 is based on OpenDSS revision 3460, with many extra/custom features.

**This is the work-in-progress branch, which will become 0.12.0. For a specific version, check the Git tags.**

While the main objective of COM compatibility has been reach, this is still a work-in-progress and is subject to changes. 

Instead of using extra numeric parameters as in the official DDLL interface, each original COM property is exposed as a pair of functions. For example, the load kVA property is exposed as:

```
    double Loads_Get_kva();
    void Loads_Set_kva(double Value);
```

Besides low-level details such as memory management, most of the COM documentation can be used as-is. 

**Starting in version 0.9.8, we disabled the `opendsscmd.ini` creation. You can set the default base frequency using the environment variable DSS_BASE_FREQUENCY, or just set it in the DSS scripts (recommended). This also means that the initial datapath is set to the current working directory.**

Since 2019-03-05, the `dss_capi` repository contains all the Pascal code used to build DSS C-API. After the 0.10.x series of releases, all Pascal code is contained in the `src/` folder. The `src/CAPI/` folder contains the main API code, which sits besides the general OpenDSS code. Although there have been extensive changes to the official OpenDSS code, the upstream/official code is kept in the branch named `opendss-official-svn` and is periodically ported -- see also the [upstream branch](https://github.com/dss-extensions/dss_capi/blob/master/docs/upstream_branch.md) document. 

## Recent changes

See [the changelog](https://github.com/dss-extensions/dss_capi/blob/0.12.x/docs/changelog.md) for a detailed list.

- **2022-07-13 / version 0.12.0: Extensive updates, includes ports of the PM functions, a new/rewritten property system, new API extensions, better performance, and other features.**
- 2021-03-09 / version 0.10.7-1: Includes a fix for some reports which presented corrupted text in version 0.10.7.
- 2020-12-28 / version 0.10.7: Maintenance release based on on OpenDSS revision 2963. Includes fixes and new features from the official OpenDSS. [A new document describing the DSS properties](https://github.com/dss-extensions/dss_capi/blob/0.10.x/docs/dss_properties.md) was added.
- 2020-07-31 / version 0.10.6: New API extensions, and ported changes from the official OpenDSS codebase. Includes some bugfixes, a new extended validation error messages and new compatibility toggles.
- 2020-03-03 / version 0.10.5: Maintenance release with several minor fixes. Includes changes ported from COM and the official OpenDSS codebase. Version 8 binary releases excluded.
- 2019-11-16 / version 0.10.4: Maintenance release. Fixes issue with long paths on Linux, includes some changes ported from COM and the official OpenDSS codebase.
- 2019-05-22 / version 0.10.3: Some important fixes, better general performance, new API extensions, new features ported from COM and the OpenDSS version 8 codebase.
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

- Currently not fully implemented:
    - Plotting in general: function callbacks for plotting were added in v0.12. For some examples, see DSS Python.
    - Diakoptics and `AggregateProfiles`: see [#46](https://github.com/dss-extensions/dss_capi/issues/46). Planned.

Closed-source features are not ported. New components or features without examples or tests are usually avoided until maturity is reached in the official OpenDSS.
    
## Extra features

Besides most of the COM methods, some of the unique DDLL methods are also exposed in adapted forms, namely the methods from `DYMatrix.pas`, especially `GetCompressedYMatrix` (check the source files for more information). Also check the [list of known differences](https://github.com/dss-extensions/dss_capi/blob/master/docs/known_differences.md) for extra methods and options.

There are a growing number of API extensions. Some are pending documentation and examples.

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

If you just need the DLL, you can download it from the releases page. Pre-release development versions can be downloaded from the build artifacts from the GitHub Actions workflow (you ).

Otherwise:

- Install the x64 Free Pascal compiler -- see [the wiki](http://wiki.freepascal.org/Installing_Lazarus#Installing_The_Free_Pascal_Compiler) for further instructions.

- If you want to use the DLL from Visual Studio, you need to generate an import library. This can be done by starting the next step from the "x64 Native Tools Command Prompt for VS 2017" (or equivalent for your Visual Studio version) -- you only need the `dumpbin.exe` and `lib.exe` utilities.

- Open a command prompt on the `dss_capi` folder and run `build/build_win_x64.bat`

The output files will be placed into the `lib/win_x64` folder. 

If you just need the DLLs, you can also find the x64 DLLs and LIBs in [the artifacts from the AppVeyor instance](https://ci.appveyor.com/project/dss-extensions/dss-capi/branch/master/artifacts). These files are built after each commit to this repository and are kept for 6 months.

### On Linux

If the provided DLLs are not compatible with your distribution, the current recommendation is to build your own KLUSolve, so you need to download and install its dependencies. Since most distributions should include compatible SuiteSparse packages (which include the KLU library), a modified version of KLUSolve is included in the `klusolve` subfolder. Overall instructions:

- Install the x64 Free Pascal compiler -- see [the wiki](http://wiki.freepascal.org/Installing_Lazarus#Installing_The_Free_Pascal_Compiler) for further instructions.

- Build the main project:
```
    bash build/build_linux_x64.sh
```

### On MacOS

After taking care of KLUSolve and placing a copy of it in the same folder, overall instructions:

- Install the x64 Free Pascal compiler -- see [the wiki](http://wiki.freepascal.org/Installing_Lazarus#Installing_The_Free_Pascal_Compiler) for further instructions.
- Build the main project:
```
    bash build/build_macos_x64.sh
```

Similar steps are required for the ARM64 version.

## Usage and examples

To understand the main concepts of DSS C-API and how it handles memory, see [the usage document](https://github.com/dss-extensions/dss_capi/blob/master/docs/usage.md).

Two minimal samples (without DSS scripts, please bring your own) are available in [examples](examples). Add the appropriate subfolder from `include` and the library from `lib` to the compilation. 

The source code from DSS Python, OpenDSSDirect.py and OpenDSSDirect.jl are more complete and advanced examples of usage.

More general documentation will be available in the future at https://github.com/dss-extensions/dss-extensions and https://dss-extensions.org

## Testing

Currently most testing/validation is based on [DSS Python](http://github.com/dss-extensions/dss_python/). Other projects like [OpenDSSDirect.py](http://github.com/dss-extensions/OpenDSSDirect.py/) and [OpenDSSDirect.jl](http://github.com/dss-extensions/OpenDSSDirect.jl/) also provide important tests that help us find and fix potential bugs.


## Roadmap

Besides bug fixes, the main funcionality of this library is mostly done. Notable desirable features that may be implemented are:
- More and better documentation. We already integrated the help strings from the IDL/COM definition files in the header files. The work for this will be done at https://github.com/dss-extensions/dss-extensions since it's shared across all projects.
- Automate validation of the Linux binaries (compare the outputs to the Windows version). Currently this is a manual process but there are on-going efforts to finalize automation.
- C++ wrappers: Expose the API to C++ using namespaces for organization, overloaded methods, etc. We expect this as soon as the API gets stable. An initial set of headers will be out in July 2022.

## Questions?

If you have any question related to the deveopment this project, feel free to open a ticket on GitHub or contact me through email (pmeira at ieee.org).
Please allow me a few days to respond.


## Credits / Acknowledgment

This project was derived from EPRI's OpenDSS and the same style of license is used. See `LICENSE` and `OPENDSS_LICENSE`, also check each subfolder for more details.

Note that, since OpenDSS depends on KLU via KLUSolve, the KLU licensing conditions (LGPL or GPL, depending on how you build KLU) apply to the resulting binaries; from the DSS-Extension KLUSolve repository, check the files `klusolve/COPYING`, `klusolve/lgpl_2_1.txt`, the SuiteSparse documentation and the Eigen3 documentation.

Also note that even though we don't add copyright notices to each of the files, most of files from the OpenDSS original codebase were modified to create the version presented today in this repository. Please refer to the Git commit history for more information.

Thanks to colleagues at the University of Campinas, Brazil, for providing feedback and helping me test this project, as well as everyone that reported issues and helped the development.

