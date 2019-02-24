# Version 0.10.2

(not released)

- `CtrlQueue`: Add the missing function `CtrlQueue_Push`.


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
