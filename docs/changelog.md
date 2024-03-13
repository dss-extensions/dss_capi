# Version 1.0 (planned)

**not released**

- **Done**:
    - drop the GR API for strings (bytes, integers and floats will continue); we might introduce an alternative API for strings if there are benefits
    - simplify the types used by the interface. For example, dropping the `uint16_t` type (used for booleans) and using `int32_t` instead — this was an artifact to ensure initial compatibility with the COM code.
    - extend the API to work with 64-bit integers where appropriate
    - extend the API to allow 32-bit floats

- **Planned**:
    - (maybe) expose/reimplement remaining PM features (diakoptics)
    - continue work on the extended reporting API
    - see also the GitHub milestone: https://github.com/dss-extensions/dss_capi/milestone/6
    - i18n complements
    - drop the `ctx_` prefix for most functions, leave the DSSContext API as the default version. We plan to drop the current single-instance API, but we can add a header with inline C functions, prefixed, for easier migration. 

# Versions 0.14.x

## Version 0.14.3 (2024-03-13)

- Header/Alt: fix `dss_obj_float64_int32_func_t` (returns `double`, not `int32_t`).
- Header: add enums for (state) variables for several components (Generator, IndMach012, PVSystem, Storage, UPFC, VCCS).
- API/YMatrix: check for valid circuit in a few more functions.
- API/Circuit: adjust `SetActiveElement` to be more conformant with the official version, i.e., returns -1 for non-circuit elements.
- API/CircuitElement: in the classic API, call the Alt implementations for `Open`/`Close`/`IsOpen` to reduce code duplication.
- Alt/CircuitElement:
    - Fix error message
    - Fix logic for `DisplayName`
    - Fix 13+ old bug in `Open`/`Close`/`IsOpen` -- the terminal parameters was being effectively ignored, applying the operations to the active terminal; also add check for valid terminal index.
- API/Capacitors: fix `Close` (same issue as CE).
- API/Batch:
    - Implement `BatchOperation_Divide`; needed for integers, and could be slightly better for floats, even though it's a tiny bit slower in modern processors.
    - Generalize `Batch_SetFloat64Array`/`Batch_SetIn32Array` to `Batch_Float64Array`/`Batch_In32Array`. This allows dropping the basic batch operations to the engine for array values, and allow for future optimizations in C++. In the current Pascal codebase, this is still better than running the operations on user-side due to memory layout and potential extra memory allocations when running on user-side.
    - Add `Batch_CreateByFloat64PropertyRange` to allow creating batches based on the value of a `float64` property.
    - Add `Batch_FilterByFloat64PropertyRange` and `Batch_FilterByInt32Property` to allow filtering existing batches into new batches.
    - Make sure to zero all elements in intermediate buffers to avoid potential issues, especially with disabled elements grabbing values from the previous element.

## Version 0.14.2 (2024-02-26)

Minor release to address issues found in the Alt API. Besides `Lines_Get_Parent`, the other changes do not affect behavior of the classic API functions.

- Alt/Bus: fix Lines/Loads/PCElements/PDElements when multiple elements are present
- Alt/CE: Fix `Alt_CE_Get_RegisterValues` in the C header.
- Alt/PDE: Check for missing solution in a few functions.
- Commands/`CalcLaplacian`: Give proper error message instead of crashing or giving "access violation" messages.
- Circuit/API: check if there is a circuit in `Circuit_Save`.
- Lines/API: adjust behavior of `Lines_Get_Parent` when the compat flag is off.

## Version 0.14.1 (2024-02-16)

Minor release to address issues found through AltDSS-Python. These shouldn't affect software that use only the classic API.

- Alt_PCE: implement two missing functions
- DSSObj, LineGeometry: for the alternative API, add array shortcuts for Wire; use the array shortcuts more.
- `SetterFlags_AllowAllConductors`:
    - Update value of `SetterFlags_AllowAllConductors` to `0x40000000` to avoid difficulties in Python.
    - Propagate setting when using Obj/Batch APIs; adjust Line.Conductors.

## Version 0.14.0 (2024-02-09)

Starting on this version, we will call DSS C-API and related projects AltDSS, an alternative implementation of OpenDSS, to make it more clear that this is not supported by EPRI and many extra features are not present in the official OpenDSS. Watch the DSS-Extensions org on GitHub for more announcements soon, including some support for the official implementation (still Windows-only at the moment). The name change **does not** mean compatibility or other aspects are expected to change, the project will still follow the basic guidelines of compatibility that have been followed since 2018.

This version should match OpenDSS v9.8.0.1 (SVN r3723). Remember to check the compatibility flags and [Known differences](https://github.com/dss-extensions/dss_capi/blob/master/docs/known_differences.md).

- Another large internal (Pascal) code refactoring step and general clean-up. Check the commits for details, too many to list. A last step is under progress and will be merged in the next major release.

- **Capitalization of property names adjusted!** A first pass was done to try to make the property names more uniform. Since **OpenDSS is case insensitive**, we are free to change the capitalization as we see fit. In the past, the official OpenDSS already changed some property names, so a general suggestion for users that process the property names is to transform the names to upper or lower cases before comparing. This should ensure better stability across past and future versions of both DSS-Extensions and the official OpenDSS. For code that do not check the names in a case insensitive approach, DSS C-API 0.14.0 provides a function to adjust the property names (`Settings_SetPropertyNameStyle`), allowing the user to choose between three alternatives: the "Modern" version with adjusted capitalization; "Lowercase" names; and "Legacy" names. "Legacy" names can be used to avoid updating the names.

- **Introduces a preview of JSON schema (new), JSON export (updated) and import (new).** Too many details to include in the changelog. Includes many updates to the internal metadata. A separate project will be posted at https://github.com/dss-extensions/AltDSS-Schema and community feedback is welcome. Note: the JSON schema will be used for projects and features beyond JSON IO.

- **Introduced options and improve correctness for `save circuit`.** During the testing of the JSON dump, issues were fixed when saving Line, LineGeometry, Transformer, AutoTrans, XfmrCode. The issues vary from simple omissions to invalid exported DSS code. The property tracking feature below also helps generating clear and valid DSS scripts from `save circuit`, eliminating properties invalidated when changing some component definitions. To allow easier control of the save function without modifying the DSS language, a **new** `Save` function was added to the circuit API (`Circuit_Save`), coupled with the new `DSSSaveFlags` enumeration, which contains the option flags. This function also allows exporting the whole circuit as a single string.

- **Introduce property tracking.** Setting some properties now mark conflicting property definitions as unset. This affects some functionality for saving circuits and is also used for the JSON schema, for instance. An example: a user creates a load with `kW=10 kvar=0`. Later, the user updates the load, setting `PF=0.9`. Setting the power factor toggles the load internal state to use kW and PF as the load definition, but `kvar` is still marked as set. The DSS engine correctly tracks the order that properties were defined, so everything works as expected. Now, with the growing usage of DSS implementation to export the circuit, every single user is required to correctly implement this tracking if they want to ensure compatibility. That is, dumping the data from this example generator in an unordered dictionary/map/etc. for processing and later dumping back to DSS scripts will result in the wrong model.
    - Users can, of course, still query the value of all properties.
    - See also `NoPropertyTracking` compat flag.
    - Currently implemented, at least partially, in the following DSS classes (plus Lines and Loads API): LineCode, LineGeometry, LoadShape, Generator, Load, PVSystem, Storage, VSource, Fault, Line, Reactor, Transformer. 
    - This functionally may be extended to other classes in future versions, if required.

- **Introduce "Setter" flags.** These are flags used to tweak how the property update is done. Notably,`AvoidFullRecalc` (some other flags are only using by the engine, internally): some specific properties like `Load.kW` can be updated both directly through a DSS script (or Text interface), or through the dedicated Loads API in any of the specific language bindings of the many APIs. The dedicated API does not force a YPrim update and full load model recalculation. 
    - When using this flag `AvoidFullRecalc` in the Obj/Batch APIs, the behavior will follow the dedicated Loads API.
    - Other flags include `ImplicitSizes` and `AllowAllConductors`. Both are currently used for enabling some of the JSON Schema functionally.
    - For batch operations, `SkipNA` was introduced to allow handling scenarios with non-uniform data.
    - The relevant API functions were updated to include an extra function argument with the setter flags.
    - Is this `AvoidFullRecalc` the same as `SkipSideEffects` compat flag? **No.** `AvoidFullRecalc` is still valid and by design (including some behavior listed in OpenDSS docs), whereas `SkipSideEffects` is potentially unintended behavior.

- **New** compatibility flags in `DSSCompatFlags`: 
    - `ActiveLine` (0x10). In the official OpenDSS implementation, the Lines API use the active circuit element instead of the active line. This can lead to unexpected behavior if the user is not aware of this detail. For example, if the user accidentally enables any other circuit element, the next time they use the Lines API, the line object that was previously enabled is overwritten with another unrelated object. This flag enables this behavior above if compatibility at this level is required. On DSS-Extensions, we changed the behavior to follow what most of the other APIs do: use the active object in the internal list, starting on version v0.14.0.
    - `NoPropertyTracking` (0x20): On DSS-Extensions/AltDSS, when setting a property invalidates a previous input value, the engine will try to mark the invalidated data as unset. This allows for better exports and tracking of the current state of DSS objects. Set this flag to disable this behavior, following the original OpenDSS implementation for potential compatibility with older software that may require the original behavior; note that may lead to erroneous interpretation of the data in the DSS properties. This was introduced in DSS C-API v0.14.0 and will be further developed for future versions.
    - `SkipSideEffects` (0x40). Some specific functions on the official OpenDSS APIs and internal code skip important side-effects. By default, on DSS-Extensions/AltDSS, those side-effects are enabled. Use this flag
    to try to follow the behavior of the official APIs. Beware that some side-effects are
    important and skipping them may result in incorrect results.
    This flag affects some of the classic API functions (Loads, Generators, Vsources) as well as the behavior of some specific DSS properties (Line: Rg, Xg, rho; Transformer/AutoTrans: XSCArray).

- **New** Alt and Obj families of functions. A new family of functions was added to allow most legacy API operations and new functionality directly on objects and batches, instead of relying on "active..." idiom. A new package, **AltDSS-Python**, will be published to illustrate the new approach. Since the engine is shared, users can mix both approach (e.g. use AltDSS-Python and OpenDSSDirect.py in the same script).

- Batch/API: 
    - Allow using batches with simple functions that return float64/int32 for each object.
    - Allow using `INT32_MAX` and `NaN` for missing values, which can be skipped with the `SetterFlags_SkipNA` flag. Sooner or later, there should a better alternative, but this can be useful in the time being.

- Headers: 
    - Remove `stdint_compat.h`. This was only required for very old or non-standard compiler like MSVC 2008. Users that require that can still source the file from older releases or get similar files from other sources.
    - Include `stddef.h` when building as C code.
    - Mark `CktElement_Get_IsIsolated` with `(API Extension)`
    - Add more `const` qualifiers. Especially useful for the new Rust and Golang bindings.

- Specific bug fixes:
    - AutoTrans: fix `DumpProperties`, readd `bank` property (unused internally).
    - Commands/Save: add version and timestamp to master file.
    - DynamicExp and DynEqPCE: fix `DumpProperties`; extend to support JSON in/out.
    - Generator: fix default value for `D` (as DSS property); previously, the value was left uninitialized. It seems to only affect user models, so it doesn't seem like a big issue. Explicitly providing a value would also works fine as a workaround.
    - Line and LineCode: fix formatting issues in `DumpProperties`
    - LoadShape: fix some issues when copying/saving data when using float32 data (which users need to explicitly opt-in).
    - PriceShape/TempShape: adjust setters for mean/stddev properties
    - Relay: fix handling of `DOC_PhaseCurveInner` (DSS property)
    - Reactor: in some situations, R and X were not synched with Z. Fixed by removing the internal duplication (does not affect user code).
    - Sensor: `DeltaDirection` was not initialized; make `clear` an explicit action.
    - SwtControl: If no element is attached (misuse), set state as "none".
    - Transformers/API: `Transformers_Get_LossesByType` and `Transformers_Get_AllLossesByType` now check if the transformer is enabled.
    - API/general: avoid tiny memory leaks (pointer size, 8 bytes) on empty data and some error situations.
    - Bus/classic API: fix `Bus_Get_N_interrupts`; was previously returning the duration instead.
    - Circuit_Capacity (API, command): fix register index, remove magic numbers. (Bug introduced back in 2008!)
    - Circuit/API: fix `Circuit_Enable` and `Circuit_Disable` (enabling/disabling circuit elements by name). An equivalent fix is included in the official OpenDSS v9.7.1.1, in the COM interface. Additionally, provide error message when trying to use invalid element names.
    - Obj/Batch API: better handling of booleans. With this change, any non-zero value is interpreted as `true`, which simplifies integration with other programming languages. This was the original intention.
    - API/Iteration: Fix issues with iteration in a few of the internal functions which could have resulted in empty results in some situations. These situations occurred only during testing, so we don't expect many users were affected.

- Misc:
    - Error handling: add a few numeric checks, and check for errors in more places during solution algorithms. The changes should help finding issues earlier and/or more easily.
    - Check for null pointers (usually missing circuit solution) in a few more places.
    - Properties/capitalization: adjust capitalization of nearly all property names. Use lowercase for description keys (help strings). *Feedback on the capitalization is welcome.* As a reminder, OpenDSS is case-insensitive, but other other are not. Providing a better internal representation of the properties will help other tools to use the internal names without extra work (no need for each tool to adjust capitalization), and it doesn't affect the DSS engine itself nor compatibility with the upstream OpenDSS.
    - Obj/API: introduce `ExtraClassIDs` to get some special lists.
    - Classic to Obj/Alt API bridge: add many `*_Get_Pointer` functions (e.g. `CktElement_Get_Pointer`, `Loads_Get_Pointer`). These functions return a pointer that can be used in the Obj API. This should enable an easier migration from the classic API, or allow users to use the Obj API to complement the classic API where the latter is lacking.
    - API/Callbacks: `dss_callback_message_t` function signature extended with two more arguments. We are not aware of any user using these, but if you are, remember to update your callbacks.
    - API/Extended errors: add a few more checks, and disable some error messages when extended errors are disabled. (Extended errors are extra error checks introduced to signal wrong usage of the DSS engine; extra = not present in the original/official codebase)
    - API/Text: microoptimize `Text_CommandBlock` for single commands. That is, users could use `Text_CommandBlock` for both single and multi-line strings without performance impacts. We did not replace `Text_Set_Command` in order to avoid potential compatibility issues.
    - API/Events: generalize the API for event extensions.
    - API/Obj: handle incorrect array sizes better, add error messages.

- Ported from the official SVN:
    - r3637: "Removing force EventLog for StorageController to match all the other controls and their defaults." (by davismont)
    - r3640: "Updated "Show" fault study report to better arrange output. (...)" (by aovallev)
    - r3642: "Skipping disabled meters when interpolating all meters." (by celsorocha)
    - r3646: updates to `TInvDynamicVars.CalcGFMYprim` (by davismont)

# Versions 0.13.x

## Version 0.13.4 (2023-06-27)

Bugfix release for `CapControl`, couple with some incremental improvements.

This version should match OpenDSS v9.6.1.3 (SVN r3623).

- Ported from the official OpenDSS SVN code:
    - `CapControl`, port SVN 3622: "Solves a bug introduced in version 9.6.1.2 when using CapControl in time or follow control modes." (by davismont).

- Added `DSSEvents`: Implement an initial set of functions analog to the COM interface. This was added for historical compatibility since use of `DSSEvents` is rarely seen. [This document from EPRI (2011)](https://restservice.epri.com/publicdownload/000000000001020090/0/Product) presents the equivalent in the COM interface. Examples will be added in our downstream projects when time allows (feel free to request one to signal interest in this feature).
- Handle better how the internal `SolutionAbort` is used. Some invalid states where not being handled on absence of float-point exceptions, leading to potential useless results (NaN) and even crashes.
- `VSource`: Abort the solution if Z1 is zero.
- API/ArrayDimensions: fix `CktElement_Get_NodeOrder`; add to `CktElement_Get_Powers`.

## Version 0.13.3 (2023-06-11)

Bugfix release for some components. No other major changes.

Fixes ported from the official OpenDSS v9.6.1.2 (SVN r3619) released on 2023-06-06, plus our custom changes (including new tests). Test circuits cross-validated as usual.

- `LoadShape`: check if there's any allocated pointer before normalizing. Since we provide more ways to fill the LoadShape data besides the official alternatives, we needed to add a few more checks in case of misuse to avoid using invalid pointers. Includes a minor fix to how manual values (set by the user) for `mean` and `stddev` are handled.
- `show` command: adjust formatting for `show variables`, `show isolated`, `show loops`, `show faults`.
- `GICTransformer`: clean-up the code and add a minor fix for `BusX`.
- Editor: tweak how the process is started; works better on Linux for terminal-based editors (GUI editors are recommended for a better experience though).
- Obj/API and headers: new functions and add a few warnings in the docs.
- New compatibility flag in `DSSCompatFlags`: add `SaveCalcVoltageBases`. On recent versions, running a `save circuit` doesn't include a `CalcVoltageBases` anymore since that causes issues for some users. We added the new flag `SaveCalcVoltageBases` to restore the old/original behavior. More options are planned for a future version in a dedicated function in the API.

- Ported (and complemented) from the official OpenDSS SVN code:
    - `UPFC`, r3610: "Fixing losses in UPFC model, there was a bug introduced several years ago when trying to redefine losses based on residual currents (bad idea)." (by davismont)
    - `CapControl`, r3615: "Fixing property requirement (element) for capcontrol in Time and Follow control modes", by davismont
    - `Capacitor`/`Reactor`: fixes related to 1- or 2-phase LL objects and Yprim; NormAmps/EmergAmps.
        - r3613: "Fixing bug in Yprim formation for 1-ph and 2-ph delta-connected capacitors. Fix to user-specified NormAmps and EmergAmps for capacitors, which were always being overridden with default values. Pending to check for same issues on Reactors." (by celsorocha)
        - r3616: "Fixing bug in Yprim formation for 1-ph and 2-ph delta-connected reactors. Fix to user-specified NormAmps and EmergAmps for reactors, which were always being overridden with default values." (by celsorocha)
        - Also includes an extra fix for a corner-case issue detected with our tests on DSS-Extensions.
    - `PVSystem`/`Storage`/`InvControl`, r3597/r3598: "Adding current limiting capabilities to IBR in QSTS and dynamics modes. Examples also available." (by davismont). Also refactored more common code to InvBasedPCE on the DSS-Extensions version.

## Version 0.13.2 (2023-05-23)

Minor release. 

- Integrate some of the code ports from the official OpenDSS. A few changes were left in the `next_svn_merge` branch to be integrated after there is a new OpenDSS release.
- Fix and complement some header code and comments.
- Refactor more of the internal code (huge merge still pending).
- Plotting: callback messages now include bus marker information.
- Add more error-checking to avoid accidental crashes in case of unexpected calls by the user code.
- `Text_CommandBlock`: rewritten, now reuses the internal `DoRedirect` function, which allows block comments and also complements the error backtrace, when necessary.
- `Obj_GetClassIdx`: fixed both header and implementation.
- Build scripts updated. The version in the header is automatically updated on release.
- JSON exports: new flags implemented, and whole behavior adjusted to better exporting all classes, including some problematic cases See [DSS-Extensions — JSON exports](https://github.com/dss-extensions/dss_python/blob/master/docs/examples/JSON.ipynb) for notes and examples.

## Version 0.13.1 (2023-04-01)

Very minor release to address a potential issue we noticed with three of the PVSystem properties.

## Version 0.13.0  (2023-03-28)

Version 0.13.0 was expected to be version 0.12.2. Due to some more large changes from the upstream/official OpenDSS, we decided to increment to 0.13 instead.
Although only officially released on March 2023, most of the changes below were already available in an alpha version release on December 2022.

- Clean-up several files to ease the transition from Pascal to C++; more enum usage, remove redundant internal properties, rename some class members, etc. Some final steps still remain (that work is done in private branches).
- Fixes a couple of minor memory leaks.
- Removed our old *Legacy Models* mechanism. Right now, the API functions still exist, but will have no effect when setting and will throw an error. For a future version, the functions will be removed. This toggle was introduced in 2020, some time after the removal of the legacy models in the official OpenDSS. We believe users had enough time to fully migrate and the extra maintenance burden is not justified anymore.
- Transition some deprecated and buggy properties to throw specific errors, instead of generic messages. Issue: https://github.com/dss-extensions/dss_capi/issues/118
- `Export` command: When the user provides a filename, use it as-is, otherwise could be an invalid path in case-sensitive file systems (e.g. Linux, most likely).
- `Dump` and `Save` commands: in some cases, our internal "hybrid enums" were not being converted correctly for dumps. A few classes had incomplete dump implementations since v0.12.0; some strings needed to be escaped for correct output.
- CtrlQueue: adjust string formatting of items; although this doesn't affect the numeric results, the strings from the queue had some truncated numbers.
- Property system: For compatibility with the official version, allow autoresizing some arrays when given conflicting number of elements through the text interface or scripts.
- `Like` property: Although not recommended and [deprecated in the official OpenDSS](https://sourceforge.net/p/electricdss/discussion/861977/thread/8b59d21eb6/?limit=25#b57c/f668), the sequence of properties filled in the original copy is also copied. If you use `Like`, remember to check if the copy actually worked since some classes are known to not copy every property correctly.
- Plotting and UI: The engine side plotting callback system is now complete. There are fixes for `DaisyPlot` and `GeneralDataPlot`, especially multi-platform handling. Changed how some properties are exposed in the JSON interchange to the callbacks. Implement argument handling and callback dispatch for `DI_Plot`, `CompareCases` and `YearlyCurves`.
- `New` commands: Fix potential issue with null pointers and duplicate names when `DuplicatesAllowed=False`.
- EnergyMeter: Fix error message when the metered element is not a PDElement.
- CIMXML export: Fix issues present since v0.12.0; reported in https://github.com/dss-extensions/OpenDSSDirect.py/issues/121
- Parser: properly error out when given excessive number of elements for matrices; implemented due to the report in https://github.com/dss-extensions/OpenDSSDirect.py/issues/122
- Port most changes from the official OpenDSS up to SVN revision 3595 (OpenDSS v9.6.1.1 + a couple of CIMXML updates); check [OpenDSS v9.6.1.1 README.txt](https://sourceforge.net/p/electricdss/code/3595/tree/trunk/Version8/README.txt) for some complementary info to the list below.
    - Relay, UPFC, UPFCControl changes ported.
    - CIMXML exports: Various updates.
    - RegControl: More log and debug trace entries.
    - LoadMult: Set `SystemYChanged` when changing `LoadMult` **through a DSS script or DSS command** (doesn't affect `Solution_Set_LoadMult`)
    - Port PVSystem, Storage, InvControl, and StorageController changes, including the new grid-forming mode (GFM). For DSS-Extensions, we added a new class InvBasedPCE to avoid some redundancy and make things clearer.
    - Port DynamicExp and related functionality. In our implementation, we also add a new class DynEqPCE to avoid some redundant code (could still be improved). the Generator and the new InvBasePCE derive from this new DynEqPCE. **Note**: the `DynamicEq` functionality from the upstream still seems incomplete and some things are not fully implemented or maybe buggy, so we only ported now to remove the burden of porting this down the line. If you find issues, feel free to report here on DSS-Extensions, but we recommended checking first with the official OpenDSS — if the issue is also found in the official version, prefer to report in the official OpenDSS forum first so everyone gets the fixes and our implementation doesn't diverge too much.
    - CktElement/API: add a few new functions related to state variables.
    - Circuit, Line: port the `LongLineCorrection` flag now that it seems to be fixed upstream. Note that we didn't publish releases with the previous buggy version from the upstream OpenDSS (that applied the long-line correction for everything).
    - LineSpacing: port side-effect from upstream; changing `nconds` now reallocates and doesn't leak previously allocated memory. Not a common operation, so it's not very relevant.
    - CktElement: port code for handling losses in AutoTrans
- Other API updates:
    - DSSContext API: allow `null` pointer for the prime/default instance. This should ease the transition. Issue: https://github.com/dss-extensions/dss_capi/issues/119
    - Error API: add `Error_Set_Description` to allow easier setting an error message from callbacks (this is for advanced usage)
    - Batch and Obj API: 
        - For a couple of fast-path operations, add checks for edit state, automatically issuing `BeginEdit` and `EndEdit` for the objects in the batch.
        - Allow passing strings (object names) instead of pointers for object references
        - Automatically add new elements to the current DSSContext (since we have not yet published a manipulation API)
        - For symmetric matrices, if the user passes only the triangle, follow the same convention as the Text interface. Includes specific fix for (parts of) complex matrices (like the R or X matrices when internally Z is stored). If the user provides full matrices, the previous behavior was correct, no changes required.
    - `Fuses_Reset`: fix C header (remove extra/unused parameter)
    - `Fuses_Get_State` and `Fuses_Get_NormalState`: add missing string copy. Sometimes this could cause memory corruption.
    - `Bus_Get_ZSC012Matrix`: check for nulls
    - `Bus_Get_AllPCEatBus`, `Bus_Get_AllPDEatBus`: faster implementations
    - `Meters_Get_CountBranches`: reimplemented
    - `Monitors_Get_dblHour`: For harmonics solution, return empty array. Previously, it was returning a large array instead of a single element (`[0]`) array. A small issue adjusted for compatibility with the official COM API results.
    - `Reactors_Set_Bus1`: Match the side-effects of the property API for two-terminal reactors. 
    - New `DSS_Set_CompatFlags`/`DSS_Get_CompatFlags` function pair: introduced to address some current and potential future concerns about compatibility of results with the official OpenDSS. See the API docs for more info.
    - New `DSS_Set_EnableArrayDimensions`/`DSS_Get_EnableArrayDimensions`: for Array results in the API, implement optional matrix sizes; when setting `DSS_Set_EnableArrayDimensions(true)`, the array size pointer will be filled with two extra elements to represent the matrix size (if the data is a matrix instead of a plain vector). For complex number, the dimensions are filled in relation to complex elements instead of double/float64 elements even though we currently reuse the double/float64 array interface. Issue: https://github.com/dss-extensions/dss_capi/issues/113

Note that a couple of SVN changes were ignored on purpose since they introduced potential issues, while many other changes and bug-fixes did not affect the DSS C-API version since our implementation is quite different in some places.

# Version 0.12.1

Incremental release to address a bug found right after 0.12.0 was released.

- Fix handling of `Voltexceptionreport` option
- `DSS_ExtractSchema`: include property descriptions, if loaded.
- Examples: add a simple OpenMP example in C using DSSContexts 

# Version 0.12.0

**Includes porting of most official OpenDSS features up to revision 3460.** Check the OpenDSS SVN commits for details.

Since version 0.11 accumulated too many changes for too long (nearly 2 years), making it hard to keep two parallel but increasingly distinct codebases, version 0.12 is a stepping stone to the next big version (planned as 0.13) that will contain all of the 0.11 changes. As such, only some of the 0.11 features are included. The previous 0.10.8 changes are also included here.

This version still maintains basic compatibility with the 0.10.x series of releases, but there are many important changes. Version 0.13 will break API and ABI compatibility since function signatures and datatypes will be extensively adjusted. Still, if you use DSS C-API through one of the projects from DSS-Extensions, we expect that your code will require few or no changes.

- The binary releases now use Free Pascal 3.2.2.
- The library name was changed from `dss_capi_v7` to `dss_capi`. The codebase was cleaned up and reorganized.
- The code was finally unified, merging remaining features from OpenDSS v8+ (with few exceptions). Most of the DSS PM commands and functions were enabled. To achieve this, most of the global variables from the OpenDSS engine were encapsulated in a new class, a DSS Context class. Multi-threaded features are based on DSSContexts, both the original OpenDSS PM features and new extensions.
- Using DSS Contexts, user threads are now possible.
- Initial ARM64/AARCH64 support added. ARM32 building scripts were also added. Support includes Apple M1 support, including parallel/multi-threading features.
- Finally use KLUSolveX (our KLUSolve fork, rewritten and extended), enabling incremental Y updates for transformers and capacitor banks. **Documentation including usage notes and limitations still not written.** This was planned for version v0.13, but momved back to v0.12 to enable ARM32 (armv7l) support and better results in ARM64 (aarch64).
- Experimental callbacks for plotting and message output. Expect initial support in Python soon after DSS C-API v0.12 is released.
- Introduce `AllowChangeDir` mechanism: defaults to enabled state for backwards compatibility. When disabled, the engine will not change the current working directory in any situation. This is exposed through a new pair of functions
`DSS_Set_AllowChangeDir` and `DSS_Get_AllowChangeDir`, besides the environment variable `DSS_CAPI_ALLOW_CHANGE_DIR`.
- New setting to toggle `DOScmd` command. Can be controlled through the environment variable `DSS_CAPI_ALLOW_DOSCMD` or functions `DSS_Get_AllowDOScmd`/`DSS_Set_AllowDOScmd`.
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
- Monitor headers: From the official OpenDSS, since May 2021, the monitor binary stream doesn't include the header anymore. When porting the change to DSS-Extensions, we took the opportunity to rewrite the related code, simplifying it. As such, the implementation in DSS-Extensions deviates from the official one. Extra blank chars are not included, and fields should be more consistent. As a recommendation, if your code needs to be compatible with both implementations, trimming the fields should be enough.
- Error messages: most messages are now more specific and, if running a DSS script from files, include the file names and line numbers.
- Spectrum: To reduce overhead during object edits, now required to exist before the object that uses it. This is consistent with most of the other types in OpenDSS.
- New object and batch APIs for direct manipulation of DSS objects and batches of objects
- New experimental API for loading scripts/data from ZIP files
- New convenience functions to bulk load commands from the API
- User-models: headers updated, and removed support for user-models in `LegacyModels` mode. `LegacyModels` will be removed in v0.13.
- New functions to export the DSS properties of objects as JSON-encoded strings
- The C headers for our library were updated to include the `const` modifier for various of the parameters. A few function declarations were fixed.
- Initial batch of i18n changes.

Due to the high number of IO changes, we recommend checking the performance before and after the upgrade to ensure your use case is not affected negatively. If issues are found, please do report.

**The changelog for version 0.12.0 will be updated if we notice missing items.**

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
    - By default, invalid accesses now result in errors reported through the Error interface. This can be disabled to achieve the previous behavior, more compatible with the official COM implementation — that is, ignore the error, just return a default/invalid value and assume the user has handled it.
    - The mechanism can be toggled by API functions `DSS_Set_ExtendedErrors` and `DSS_Get_ExtendedErrors`, or environment variable `DSS_CAPI_EXTENDED_ERRORS=0` to disable (defaults to enabled state).
- New **Legacy Models** mechanism:
    - OpenDSS 9.0+ dropped the old `PVsystem`, `Storage`, `InvControl`, and `StorageController` models, replacing with the new versions previously known as `PVsystem2`, `Storage2`, `InvControl2` and `StorageController2`.
    - The behavior and parameters from the new models are different — they are better, more complete and versatile models. Check the official OpenDSS docs and examples for further information. 
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

- Disable builds and distribution of v8-only variation — the extra/missing parallel-machine will be completely merged in a mixed (v7+v8) codebase in the coming months.
- This version should be fully API compatible with 0.10.3+.
- `Bus` and `CktElement` API functions reworked with some more checks.
- Updated up to revision 2837 of the official OpenDSS code:
    - Ported changes from SVN (v7 and v8) into DSS C-API v7 variation (v8 was left untouched).
    - 4 new API level functions (`ActiveClass_Get_ActiveClassParent`, `PVSystems_Get_Pmpp`, `PVSystems_Set_Pmpp`, `PVSystems_Get_IrradianceNow`)
    - 4 new components: `PVsystem2`, `Storage2`, `InvControl2`, `StorageController2` — *added for early testing, no dedicated API functions yet*. At the moment, please consider them experimental features subject to change.
    - `CIM100`: several changes
    - `ExpControl`: new `Tresponse` property
    - `ConductorData`, `LineConstants`, `LineGeometry`: new `Capradius` property
    - `XfmrCode`, `Transformer`: new Seasons and Ratings properties
    - `Bus_Get_puVLL` and `Bus_Get_VLL` — see revision 2836 (official SVN). Included an extra fix in DSS C-API to avoid some corner cases.
    - Other small bug fixes like the Full Carson fix — see https://sourceforge.net/p/electricdss/discussion/861976/thread/2de01d0cdb/ and revision 2805 (official SVN)

# Version 0.10.4

- Updated up to revision 2761 of the official OpenDSS code. The changes affect at least the following components: CIMXML export, `Capacitor`, `InvControl`, `LineGeometry`, `PVsystem`, `StorageController`, `Storage`, `Vsource`, `VCCS`.
- This version should be fully compatible with 0.10.3.
- Fixes issue with long paths on Linux, potentially other platforms too.

# Version 0.10.3

- Updated up to revision 2609 of the official OpenDSS code. Most of the new features have been ported and tested successfully.
- The KLUSolve code has been moved to a new repository at https://github.com/dss-extensions/klusolve/ — to avoid confunsion with the upstream project (which hasn't seen any development for a couple of years), we refer to it by "DSS-Extensions KLUSolve". 
- The repository `electricdss-src` was merged into `dss_capi`. That is, the main repository for DSS C-API now contains most of the relevant OpenDSS code side-by-side with the API code. This simplified management and also makes it easier to build the library.
- The default packages use GCC-based DLLs for Windows. If you encounter issues with your application, our [KLUSolve](https://github.com/dss-extensions/klusolve/releases) also distributes MSVC2017 DLLs.
- On Windows, besides the original MSVC import libraries, the default packages now also include GCC import libraries.
- Fixes issue with the line parameters on Linux and macOS due to memory-address aliasing issues.
- Correctly enables DSS_CAPI_EARLY_ABORT by default.
- Extra validation has been added for many functions. This should avoid some crashes due to uninitialized states or user errors. The Error API is used extensively for this — remember to check for errors periodically!
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
- `CktElement_Get_Variable` and `CktElement_Get_Variablei`: fix bug — the second parameter, `Code`, is a pointer to integer, not a simple integer. `Code` is an out parameter that contains an error code.
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
