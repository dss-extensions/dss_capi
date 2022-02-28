# Version 0.13

**not released**

- **Done**:
    - drop the GR API for strings (bytes, ints and floats will continue)
    - simplify the types used by the interface. For example, dropping the `uint16_t` type (used for booleans) and using `int32_t` instead -- this was an artifact to ensure initial compatibility with the COM code.
    - extend the API to work with 64-bit integers where appropriate
    - extend the API to allow 32-bit floats

- **Planned**:
    - expose the OpenDSS v8/v9 PM features as possible
    - continue work on the plotting and extended reporting API
    - see also the GitHub milestone: https://github.com/dss-extensions/dss_capi/milestone/6

# Version 0.12

**not released**

Since version 0.11 accumulated too many changes for too long (nearly 2 years), making it hard to keep two parallel but increasingly distinct codebases, version 0.12 is a stepping stone to the next big version (planned as 0.13) that will contain all of the 0.11 changes. As such, only some of the 0.11 features are included. The previous 0.10.8 changes are also included here.

This version still maintains basic compatibility with the 0.10.x series of releases, but there are many important changes. Version 0.13 will break API and ABI compatibility since function signatures and datatypes will be extensively adjusted. Still, if you use DSS C-API through one of the projects from DSS Extensions, we expect that your code will require few or no changes.

- The binary releases now use Free Pascal 3.2.2.
- The library name was changed from `dss_capi_v7` to `dss_capi`. The codebase was cleaned up and reorganized.
- The code was finally unified, merging remaining features from OpenDSS v8+ (with few exceptions). The DSS PM commands and functions will be enabled only in v0.13. To achieve this, most of the global variables from the OpenDSS engine were encapsulated in a new class, a DSS Context class. Multi-threaded features will be based on DSSContexts, both the original OpenDSS PM features and new extensions.
- Experimental ARM64/AARCH64 support added. ARM32 building scripts were also added.
- Finally use KLUSolveX (our KLUSolve fork, rewritten and extended), enabling incremental Y updates for transformers and capacitor banks. **Documentation including usage notes and limitations still not written.** This was planned for version v0.13, but momved back to v0.12 to enable ARM32 (armv7l) support and better results in ARM64 (aarch64).
- Experimental callbacks for plotting and message output. Expect initial support in Python soon.
- Introduce `AllowChangeDir` mechanism: defaults to enabled state for backwards compatibility. When disabled, the engine will not change the current working directory in any situation. This is exposed through a new pair of functions
`DSS_Set_AllowChangeDir` and `DSS_Get_AllowChangeDir`, besides the environment variable `DSS_CAPI_ALLOW_CHANGE_DIR`.
- Use `OutputDirectory` more. `OutputDirectory` is set to the current `DataPath` if `DataPath` is writable. If not, it's set to a general location (`%LOCALAPPDATA%/dss-extensions` and `/tmp/dss-extensions` since this release). This should make life easier for a user running files from a read-only location. Note that this is only an issue when running a `compile` command. If the user only uses `redirect` commands, the `DataPath` and `OutputDirectory` are left empty, meaning the files are written to the current working directory (CWD), which the user can control through the programming language driving DSS C-API. Note that the official OpenDSS COM behavior is different, since it loads the `DataPath` saved in the registry and modifies the CWD accordingly when OpenDSS is initialized.
- File IO rewritten to drop deprecated Pascal functions and features. This removes some limitations related to long paths due to the legacy implementation being limited to 255 chars.
- Reworked `TPowerTerminal` to achieve better memory layout. This makes simulations running `LoadsTerminalCheck=false` and `LoadsTerminalCheck=true` closer in performance, yet disabling the check is still faster.
- Use `TFPHashList` where possible (replacing the custom, original THashList implementation from OpenDSS).
- New LoadShape functions and internals: 
    - Port memory-mapped files from the official OpenDSS, used when `MemoryMapping=Yes` from a DSS script while creating a LoadShape object.
    - Release the `LoadShape_Set_Points` function, which can be used for faster LoadShape input, memory-mapping externally, shared memory, chunked input, etc.
- Some new functions: 
    - `Circuit_Get_ElementLosses`
    - `CktElement_Get_NodeRef`
- `DSS_Get_COMErrorResults`/`DSS_Set_COMErrorResults`: New compatibility setting for error/empty result. If enabled, in case of errors or empty arrays, the API returns arrays with values compatible with the official OpenDSS COM interface. 
    
    For example, consider the function Loads_Get_ZIPV. If there is no active circuit or active load element:
    - In the disabled state (COMErrorResults=False), the function will return "[]", an array with 0 elements.
    - In the enabled state (COMErrorResults=True), the function will return "[0.0]" instead. This should
      be compatible with the return value of the official COM interface.
    
    Defaults to True/1 (enabled state) in the v0.12.x series. This will change to false in future series.
    
    This can also be set through the environment variable DSS_CAPI_COM_DEFAULTS. Setting it to 0 disables
    the legacy/COM behavior. The value can be toggled through the API at any time.

- Drop function aliases: previously deprecated function aliases (`LoadShapes_Set_Sinterval` and `LoadShapes_Get_sInterval`) were removed to simplify the build process. Use `LoadShapes_Set_SInterval` and `LoadShapes_Get_SInterval` instead.
- Monitor headers: From the official OpenDSS, since May 2021, the monitor binary stream doesn't include the header anymore. When porting the change to DSS Extensions, we took the opportunity to rewrite the related code, simplifying it. As such, the implementation in DSS Extensions deviates from the official one. Extra blank chars are not included, and fields should be more consistent. As a recommendation, if your code needs to be compatible with both implementations, trimming the fields should be enough.
- Error messages: most messages are now more specific and, if running a DSS script from files, include the file names and line numbers.
- Spectrum: To reduce overhead during object edits, now required to exist before the object that uses it. This is consistent with most of the other types in OpenDSS.
- New object and batch APIs
- New experimental API for loading scripts/data from ZIP files
- New convenience functions to bulk load commands from the API

Due to the high number of IO changes, we recommend checking the performance before and after the upgrade to ensure your use case is not affected negatively. If issues are found, please do report.

# Version 0.11

**not released, will become 0.13**

# Version 0.10.8

**not released, was merged into 0.12**

# Version 0.10.7

- Simple maintenance release, which includes most changes up to OpenDSS v9.1.3.4 (revision 2963).
- Includes an important bug fix related to the `CapRadius` DSS property. If your DSS scripts included the pattern `GMRac=... rad=...` or `GMRac=... diam=...` (in this order and without specifying `CapRadius`), you should upgrade and re-evaluate the results. 
- This version should be fully API compatible with 0.10.3+.
- A reference document listing the DSS commands and properties for all DSS elements is now available at https://github.com/dss-extensions/dss_capi/blob/0.10.x/docs/dss_properties.md
- New functions API ported from the official OpenDSS include: `Bus_Get_AllPCEatBus`, `Bus_Get_AllPDEatBus`, `CktElement_Get_TotalPowers`, `Meters_Get_ZonePCE`.
- The changes ported from the official OpenDSS include the following (check the repository for more details):
    - "Adds LineType property to LineCode and LineGeometry objects."
    - "Correcting bug found in storage device when operating in idling mode. It was preventing the solution of other test feeders (IEEE 9500)"
    - "Enabling fuel option for generator, fixing bug found in TotalPower command."
    - "Adding kvar compensation calculation for normalizing reactive power at feeder head. v 9.1.2.4"
    - "Adding: - Line type variable to line definition. - AllPCEatBus and AllPDEatBus commands to the executive command set. - AllPCEatBus and AllPDEatBus commands to bus interface in COM/DLL. (...)"
    - "Adding capability to energy meter for getting the list of all PCE (shunt) within a zone. Interface "AllPCEatZone" for COM/DLL created."
    - "Fixing bug found when calculating voltage bases with large amount of numbers (large array)."
     


# Version 0.10.6

- This version should be fully API compatible with 0.10.3+. The behavior of some functions changed with the new extensions. Especially, empty strings are explicitely return as nulls instead of "\0". This conforms to the behavior already seen in arrays of strings.
- The binary releases now use Free Pascal 3.2.0. We observed the solution process is around 6% faster, and results are even closer to the official OpenDSS.
- The releases now include both the optimized/default binary and a non-optimized/debug version. See the [Debugging](https://github.com/dss-extensions/dss_capi/blob/0.10.x/docs/debug.md) document for more.
- Extended API validation and **Extended Errors** mechanism: 
    - The whole API was reviewed to add basic checks for active circuit and element access. 
    - By default, invalid accesses now result in errors reported through the Error interface. This can be disabled to achieve the previous behavior, more compatible with the official COM implementation -- that is, ignore the error, just return a default/invalid value and assume the user has handled it.
    - The mechanism can be toggled by API functions `DSS_Set_ExtendedErrors` and `DSS_Get_ExtendedErrors`, or environment variable `DSS_CAPI_EXTENDED_ERRORS=0` to disable (defaults to enabled state).
- New **Legacy Models** mechanism:
    - OpenDSS 9.0+ dropped the old `PVsystem`, `Storage`, `InvControl`, and `StorageController` models, replacing with the new versions previously known as `PVsystem2`, `Storage2`, `InvControl2` and `StorageController2`.
    - The behavior and parameters from the new models are different -- they are better, more complete and versatile models. Check the official OpenDSS docs and examples for further information. 
    - The implementation of the new models in DSS C-API was validated successfully with all test cases available. As such, we mirror the decision to make them the default models.
    - As an extension, we implemented the Legacy Models option. By toggling it, a `clear` command will be issued and the alternative models will be loaded. This should allow users to migrate to the new version but, if something that used to work with the old models stopped working somehow, the user can toggle the old models. The idea is to keep reproducibility of results while we keep updating the engine and the API.
    - Since EPRI dropped/deprecated the old models, we might drop them too, in a future release. Please open an issue on GitHub or send a message if those old models are important to you.
    - The mechanism can be controlled by API functions `DSS_Set_LegacyModels` and `DSS_Get_LegacyModels`, or environment variable `DSS_CAPI_LEGACY_MODELS=1` to enable (defaults to disabled state).
- WireData API: expose the `CapRadius` property as a new pair of functions.
- PDElements API: extended with many batch functions exposing equivalents to some CSV reports: `AllNames`, `AllMaxCurrents`, `AllPctNorm`, `AllPctEmerg`, `AllCurrents`, `AllCurrentsMagAng`, `AllCplxSeqCurrents`, `AllSeqCurrents`, `AllPowers`, `AllSeqPowers`, `AllNumPhases`, `AllNumConductors`, `AllNumTerminals`.
- `CktElement_Get_SeqPowers`: fix issue for positive sequence circuits (wrong results could corrupt memory).
- Many API functions were optimized to avoid unnecessary allocations and copies.
- Some bugs found in DSS C-API and also reported upstream (already fixed in SVN):
    - `CapRadius` DSS property: if the radius was initialized using `GMRac`, `CapRadius` was left uninitialized, resulting in invalid/NaN values.
    - `Sensors` API: some functions edited capacitors instead of sensors.
- Updated to the official OpenDSS revision 2903, corresponding to versions 9.0.0+. Changes include:
    - ExportCIMXML: updated.
    - Relay: Fix in `GetPropertyValue`.
    - Line: In `DumpProperties` and `MakePosSequence`, the length is handled differently for lines with `LineGeometry` or `LineSpacing`.
    - Bus API: new `LineList`, `LoadList` functions.
    - Lines API: SeasonRating now returns NormAmps if there's no SeasonSignal.
    - New command DSS `Zsc012`: "Returns symmetrical component short circuit impedances Z0, Z1, and Z2 for the ACTIVE 3-PHASE BUS. Determined from Zsc matrix."
    - `PVsystem2`, `Storage2`, `InvControl2`, `StorageController2` updated and renamed.
  
# Version 0.10.5

- Disable builds and distribution of v8-only variation -- the extra/missing parallel-machine will be completely merged in a mixed (v7+v8) codebase in the coming months.
- This version should be fully API compatible with 0.10.3+.
- `Bus` and `CktElement` API functions reworked with some more checks.
- Updated up to revision 2837 of the official OpenDSS code:
    - Ported changes from SVN (v7 and v8) into DSS C-API v7 variation (v8 was left untouched).
    - 4 new API level functions (`ActiveClass_Get_ActiveClassParent`, `PVSystems_Get_Pmpp`, `PVSystems_Set_Pmpp`, `PVSystems_Get_IrradianceNow`)
    - 4 new components: `PVsystem2`, `Storage2`, `InvControl2`, `StorageController2` -- *added for early testing, no dedicated API functions yet*. At the moment, please consider them experimental features subject to change.
    - `CIM100`: several changes
    - `ExpControl`: new `Tresponse` property
    - `ConductorData`, `LineConstants`, `LineGeometry`: new `Capradius` property
    - `XfmrCode`, `Transformer`: new Seasons and Ratings properties
    - `Bus_Get_puVLL` and `Bus_Get_VLL` -- see revision 2836 (official SVN). Included an extra fix in DSS C-API to avoid some corner cases.
    - Other small bug fixes like the Full Carson fix -- see https://sourceforge.net/p/electricdss/discussion/861976/thread/2de01d0cdb/ and revision 2805 (official SVN)

# Version 0.10.4

- Updated up to revision 2761 of the official OpenDSS code. The changes affect at least the following components: CIMXML export, `Capacitor`, `InvControl`, `LineGeometry`, `PVsystem`, `StorageController`, `Storage`, `Vsource`, `VCCS`.
- This version should be fully compatible with 0.10.3.
- Fixes issue with long paths on Linux, potentially other platforms too.

# Version 0.10.3

- Updated up to revision 2609 of the official OpenDSS code. Most of the new features have been ported and tested successfully.
- The KLUSolve code has been moved to a new repository at https://github.com/dss-extensions/klusolve/ -- to avoid confunsion with the upstream project (which hasn't seen any development for a couple of years), we refer to it by "DSS-Extensions KLUSolve". 
- The repository `electricdss-src` was merged into `dss_capi`. That is, the main repository for DSS C-API now contains most of the relevant OpenDSS code side-by-side with the API code. This simplified management and also makes it easier to build the library.
- The default packages use GCC-based DLLs for Windows. If you encounter issues with your application, our [KLUSolve](https://github.com/dss-extensions/klusolve/releases) also distributes MSVC2017 DLLs.
- On Windows, besides the original MSVC import libraries, the default packages now also include GCC import libraries.
- Fixes issue with the line parameters on Linux and macOS due to memory-address aliasing issues.
- Correctly enables DSS_CAPI_EARLY_ABORT by default.
- Extra validation has been added for many functions. This should avoid some crashes due to uninitialized states or user errors. The Error API is used extensively for this -- remember to check for errors periodically!
- v8: Several fixes, working on both macOS and Linux, including validation. On Windows there might be threading issues. Please open an issue if you'd like to use it on Windows (there are a couple of workarounds).
- Optimized (small, dense) matrix-vector multiplication, up to 20% time-savings
- The APIs for most classes now include `*_Get_idx` and `*_Set_idx` functions. 
- The `*_Set_Name` functions are now faster by default, as well as present linear behavior with the system size.
- `XYCurves`: Add `XYCurves_Get_AllNames`
- Ported to v7: the DSS commands `CalcIncMatrix`, `CalcIncMatrix_O`, `Refine_BusLevels`, `CalcLaplacian`, and related export options: `IncMatrix`, `IncMatrixRows`, `IncMatrixCols`, `BusLevels`, `Laplacian`, `ZLL`, `ZCC`, `Contours`, `Y4`

- General optimization of copies in the API-level code, avoiding extra copies or loops where possible
- `Transformers`: new `Transformers_Get_LossesByType` and `Transformers_Get_AllLossesByType` to get total losses, load losses and no-load losses for the transformers.
- New `Settings_Get_LoadsTerminalCheck` and `Settings_Set_LoadsTerminalCheck`: can be used for static scenarios (no open terminals for loads) to toggle the costly checks for open terminals. If set to false, it can save around 25% of simulation time, depending on the circuit characteristics.
- New `CktElement_Get_IsIsolated` (fetches the current value for a single element, see the Topology API for more info).
- New `Lines_Get_IsSwitch`/`Lines_Set_IsSwitch`, equivalent to the `switch=y/n` via DSS command.
- `PVSystems`: expose some requested properties (daily, duty, yearly, Tdaily, Tduty, Tyearly). Still missing some more properties.
- New `DSS_GR_DataPtr_P*` and `DSS_GR_CountPtr_P*`: alternative Global Result API functions, needed for [DSS_MATLAB](https://github.com/dss-extensions/dss_matlab).
- `DSS_Set_AllowForms`: on Windows, only allow enabling the console output if there is a console available.

Some highlights from the upstream OpenDSS changes:
- `ReduceCkt`: new interface with circuit reduction methods
- `Lines`: new seasonal ratings mechanism that allows changing the current rating through the simulation
- `ExpControl`: new `PreferQ` option

# Version 0.10.2

- `CtrlQueue`: Add the missing function `CtrlQueue_Push`.
- New `DSS_Get_AllowEditor` and `DSS_Set_AllowEditor`:  AllowEditor controls whether the external editor is used in commands like `Show`. If you set to 0 (false), the editor is not executed. Note that other side effects, such as the creation of files, are not affected.
- `LoadShapes` code reworked: more validation, fix potential memory issues after resizing using `Npts`, and faster reads and writes (up to 3x).
- The ouput libraries/DLLs have been moved from `lib/v7` and `lib/v8` to just `lib` since the libraries already have different names. The previous organization was due to expectations that we would keep only the OpenDSS Version 8 line of code but at the moment the main development is focused of DSS C-API on Version 7.

# Version 0.10.1

*Besides `CktElement_Get_Variable` and `CktElement_Get_Variablei`, version 0.10.1 should be API and ABI compatible with 0.10.0.*

## DSS C-API and custom OpenDSS changes

- Several more functions now use the Error interface to provide error information. If you use DSS C-API, you should check `Error_Get_Number`, at least from time to time
- New `Error_Get_NumberPtr`: returns a pointer to the current error. Whenever the pointer content is non-zero, check `Error_Get_Description` and then reset it zero
- New `Error_Get_EarlyAbort` and `Error_Set_EarlyAbort`: Gets/sets the DSS script error-handling behavior. If a warning or error occurs and early abortion is enabled (default), the processing of the script is always halted. Otherwise, the processing of the script continues until a major error occurs or it finishes.
- New `DSS_Get_PAnsiChar`: for use in [DSS MATLAB](https://github.com/PMeira/dss_matlab) to avoid triple-pointer indirections (MATLAB doesn't like those).
- New `GICSources` family of functions: ported from COM
- `BatchEdit` DSS command is now case-insensitive in FPC for consistency with COM
- Line breaks now respect the platform defaults (instead of CR+LF on all platforms).
- Implement `Monitors_Get_FileVersion`, which was incomplete in COM (should return 1 as the current version)
- Rename to `LoadShapes_Get_SInterval`/`LoadShapes_Set_SInterval` for case consistency. Aliases are provided for the previous names, but those were deprecated and will be removed in the future.
- `DSSProperty_Set_Val`: error out if the current property index is invalid
- `Circuit_SetCktElementIndex` and `Circuit_SetCktElementName`: error out if invalid inputs are provided
- `CktElement_Get_Variable` and `CktElement_Get_Variablei`: fix bug -- the second parameter, `Code`, is a pointer to integer, not a simple integer. `Code` is an out parameter that contains an error code.
- `Loads_Set_ZIPV`: check if exactly 7 values are provided, otherwise errors out
- `Meters` family: 
    - error out if current `ActiveSection` is invalid
    - check if meter zone lists are built more thorougly (avoids invalid memory accesses), errors out if invalid (also patched in `electricdss-src`)
    - avoid out-of-bounds memory access when using `Meters_DoReliabilityCalc` (patched in `electricdss-src`)
- Linux build changes for KLUSolve: changes target distributing the shared object for projects like DSS MATLAB:
    - The version script exposes only KLUSolve symbols, hiding all KLU related symbols that might get in conflict with external libraries that use other versions of KLU.
    - A rpath change makes it simple to load KLUSolve from the same folder as DSS C-API, without moving anything or changing `LD_LIBRARY_PATH`.
- Ported from V8 to V7:
    - StorageController: new `PeakShaveLow` charge mode
    - PVsystem: new `WattPriority` property
    - Sensor: "Bug found in sensor.pas when defining powers instead of currents for load allocation" (commit by davismont)
- Update building scripts
- Binary releases for Windows, Linux and macOS are now provided
- Reformat source code with JEDI Code Format
    
## OpenDSS commits merged in [`electricdss-src`](https://github.com/dss-extensions/electricdss-src/)

* Cleaning up (cherry picked from commit 750ffdadb48d57f32c5c0f86710fdf1507ac007a)

* Updates to the OpenDSS Viewer connection in OpenDSS V8. (cherry picked from commit 72da28f3cf0cd24a0016fb3031c75fccb15226d9) This version includes:
    - Dynamic update mode for scatter plots.
    - Changes to the name of the OpenDSS Viewer (formerly DSS Visualization Tool).
    - Changes and updates in help for commands related to the OpenDSS Viewer.

* New charging mode in storage controller implemented in V8. The new control mode provided by Valentin Rigoni (cherry picked from commit 550c1a252d8bee4934ccf48a63587878ab1d72d3)

* UCF microgrid controller added in V8. Validation pending (cherry picked from commit 0d41190ab7e250c72db6a801a04c6032dcb4b5ff)

* Catching up with V7, round 1 (cherry picked from commit 35bc5ad33a82de22f834df2a0cd1e35e29d036d1)

* Updates to the OpenDSS Viewer connection in OpenDSS V8. (cherry picked from commit 3a87e66e7dcdbd5ce72e2fca0b4ef42fd621deee) This version includes:
    - Dynamic update mode for voltage profile plots (3D and 2D).
    - Changes and updates in help for commands related to the OpenDSS Viewer.

* ZCT validated and ZCC calculated including the links impedance through ZLL (cherry picked from commit 37459d21857080af1bb70abb6901c366ad1388e4)

* Catching up with V7, round 2 (8.4.1) (cherry picked from commit 1d2189c39bc40ef64807919cf83979d49d8e680d)

* A-Diakoptics initialization rebuilt as state machine for error handling (cherry picked from commit 792524d19481baef3fe5537721d6a0b46c7bbb55)

* Integrating ZCC to be operated with ZLL in V8 (cherry picked from commit 8cae28252082555524df8668d25aa27769285bbd)

* ZCT calculation finished (cherry picked from commit 3ac589855a51ced24d51067e1c25824965e59b31)

* ZCT validated and ZCC calculated including the links impedance through ZLL (cherry picked from commit 37459d21857080af1bb70abb6901c366ad1388e4)

* ZCC calculated and validated. Y4 calculation (inverse) under development and ready to go. Bug in sparse math lib corrected when transposing matrices  (cherry picked from commit 9b51db53b8114005de39f2c744cfe2ec52474cdb)

* Mods to force neutral connection of AutoTrans to be the same node at the bus for the Common winding (cherry picked from commit a8d59d3cb4d6dd9b1007052849a6948f7625afdf)

* Increase fixed buffer size to prevent overruns. (cherry picked from commit 4c36cb1152b633afab6a2f00a6e4ddb1f368f155)

* Update Transformer to match V7 (cherry picked from commit 478a42cde50ac711b94c578c431385542d10a79a)

* V8 AutoTrans update to fix bus name. (cherry picked from commit 863818bb07827b7f812d95e123f61098a73b7c19)

* Clear problem fixed in V8, GICSources interface added to COM (it has to be added to Direct DLL), AuxParser indexed to avoid conflicts in the future (cherry picked from commit d038df566d1b9ec154e8c72451fbc3776c1c95ad)

* Version 8.4.1, adds the GICSources interface to DirectDLL and fixes several issues related with memory management in V8.3. Includes the initialization of A-Diakoptics validated and simplified (cherry picked from commit e3aace668914712a449df09a39c59c26acd8b821)

* Fixed divide by zero bug in AutoTrans (cherry picked from commit 7ac92989f0ff03f209a01e8044b79bdde3a41985)

* Fix divide-by-zero bug in AutoTrans, V8 (cherry picked from commit fd658c7abdbd1f2a651708ab80ec16ba90f929af)

* Bug found in GICSource.pas fixed. The bug was affecting the DirectDLL as well. All interfaces to GICSources are now working correctly. (cherry picked from commit dc0c0ff10ff9e7ad6b2b78642d03a4bb1ff83bbf)

* Preparing official release for A-Diakoptics solution algorithm (cherry picked from commit 18d097db609f070b26d7dc0a1582c8cff9fc6829)

* Memory allocation refined in Diakoptics.pas (cherry picked from commit 4d9d47f5fc1bc2ad4c78fc9cdee9b2d69456887e)

* Version ZERO of A-Diakoptics solution mode added into the actor's solution routine. It shouldn't affect other simulation modes when working with A-Diakoptics OFF (Default). New commands for debugging the A-Diakoptics process are also included. (cherry picked from commit 6b8e2ca1defe4f499a57f8acca1e794c2b3f10e6)

* Files on the command line are now Compiled rather than simply executed so that the path gets changed properly. (cherry picked from commit f140a135af82b046836cf4b28f0857cdec99f6a0)

* Files on the command line are now Compiled rather than simply executed so that the path gets changed properly. (cherry picked from commit fc6532bb1297308f158a94fe451f31828a94654b)

* Correction to panel initialization compile command. (cherry picked from commit 48a9b7d129666b55aff171a05504ccfa96afd5dd)

* Correction to panel initialization compile command. (cherry picked from commit c077f8e52d619189d409ecd9ac0fb087c51957f2)

* A-Diakoptics included in V8.5 (Beta) - still has some memory problems that need to be debugged (cherry picked from commit 0e8a9c8a1eb86b028fbe314babf28783bcfc98de)

* Adding tearing statistics to A-Diakoptics (cherry picked from commit c6f2de6eaabf8b685797e13bb9ca7593a584b68c)

* Statistics and other features added to A-Diakoptics (cherry picked from commit 4e7c44cda4003e9e4cd4ce345e816bab4c595005)

* Removing Parservars redefinition when creating the Executive Obj (cherry picked from commit 718f4294f495587017972b9ae8f9d879086f183b)

* Bug found in DLL and COM interfaces for Version 8 (for the parallel suite) (cherry picked from commit 82d74f1f1706c664128f9925b97dbabe2d14700d)

* Cosmetic Changes to a couple of pas modules (cherry picked from commit 21fe26bbd7328a2da3ca9f05f741050837427754)

* Incorporating some mostly cosmetic changes from Version 7 (cherry picked from commit 4e7bd4b580227b9608bc45770184b940ca27af0a)

* Adding option in Export monitor to export all the monitors at once, also, bug in A-Diakoptics solved. (cherry picked from commit 04b49a96bbe971c1c2f4b71d8e7216f2d4977b95)

* Mods to Energymeter to include PVSystem and Storage on ZoneList (cherry picked from commit bcdcafc1d40596d7f99a77e0e944b3ed21f2679c)

* Sync with Version 7 (cherry picked from commit bd4fb3966b75a06eefb4684a496c782cc4e74567)

* Storage controller documentation (new charging mode) (cherry picked from commit 5f92e3237c8d99097e611c506e6e69fd3725b96e)

* Harmonize changes with upstream SVN (part 1):
    - AutoTrans, including RegControl integration
    - Monitor - new mode 10, "Transformer Winding Voltages (across winding)"
    - GICsource

* Add missing PlotOptions code block, sync Panel code.
