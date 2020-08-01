# Debugging

For debugging the Pascal side of the DSS Extensions, you need to load the debug version of DSS C-API. Starting on DSS C-API version 0.10.6, this debug version is included side-by-side with the optimized version. You can, of course, build it yourself if you prefer. Most users have no interest in Pascal, hence the decision to include the debug binary in some of the projects. As a reminder, **the debug version is slower since the optimizations are disabled**. Remember to toggle back the usage of the release/normal version after you are done debugging.

If you are using DSS_Python or OpenDSSDirect.py, set the environment variable `DSS_EXTENSIONS_DEBUG=1` before starting Python and loading the module. This will instruct the modules to load the debug version. The other interface projects will be updated in the future the same or similar mechanism where possible. With that version loaded, you typically can use a debugger of your preference. Some examples are listed in this document.

To be able to browse the code, clone the main repository on the tag corresponding to the release you are using, for example:

```
git clone --branch 0.10.6 https://github.com/dss-extensions/dss_capi.git
```

To confirm the tag, you can use the low-level function `DSS_Get_Version()`, or `Version` property of the corresponding `DSS` or `Basic` interface, depending on what interface you prefer. Alternatively, the Git commit hash is also included in the version string. When you load a debug binary, the "DEBUG" string is included in the version string.

Recommended reading for more information: 
- https://wiki.freepascal.org/Debugger_Setup
- https://wiki.lazarus.freepascal.org/GDB_Debugger_Tips

## Debugging on Linux / macOS

The debug format is fully supported by the native tools. Either start a dedicated process, or attach to a running one. Just remember to load the debug version.

Most IDEs should allow you to open the Pascal source files and set breakpoints. Debugging on Windows is more complicated and unreliable. As such, more detail is provided in the next section -- you can apply the general information provided there for Linux and macOS. Feel free to open an issue on GitHub if you would like more details.

## Debugging on Windows

### Using Lazarus

Since DSS C-API is a library, the default setup from Lazarus is not ideal on Windows and needs some tweaks. The main reason is that the hosting applications are tipically written in other languages, which the default setup is not always able to support.

To sucessfully run or attach to a process, download a more recent GDB (Free Pascal 3.2 comes with an older version):

https://sourceforge.net/projects/lazarus/files/Lazarus%20Windows%2064%20bits/Alternative%20GDB/GDB%208.2/

Create a dummy project with type "Library" (we could provide one in the future, Lazarus itself is not used to build DSS C-API).

On "Tools" / "Options" / "Debugger" / "General":
- Set the debugger type do "GNU debugger (gdb)".
- Select the executable for the GDB you just downloaded.

You can either attach to a running process, or start a dedicated one. 

For example, for DSS_Python/OpenDSSDirect.py, on "Run" / "Run Parameters":
- Select your "python.exe" as the "Host application". 
- Optionally, you can provide Python script as parameter (e.g. a script that crashes), as well as run it interactively: "-i c:\temp\test.py"
- In the "Environment" tab, add a new user override, variable "DSS_EXTENSIONS_DEBUG" and value "1".

If your script indeed crashes, you can now run (either the play button on the toolbar, or menu "Run" / "Run"). Lazarus should ask you to point to the offending source file if the crash happens in the DSS C-API codebase.

If you want to watch a step-by-step, open the desired Pascal source, set the breakpoint and then run. For example, if we want to debug the `AllNames` property for the `Lines` interface:

- Open the file `src\v7\CAPI_Lines.pas`
- Find the procedure `Lines_Get_AllNames` and set a breakpoint in its body (click left to the line number on the editor to set a breakpoint).
- Create a simple Python script that calls it:

```python
from dss import DSS
DSS.Text.Command = r'redirect "c:\temp\electricdss-tst\Version8\Distrib\IEEETestCases\13Bus\IEEE13Nodeckt.dss"
print(DSS.ActiveCircuit.Lines.AllNames)
```

- Finally click "Run". The execution should stop at your breakpoint.

### Using Visual Studio

Using Microsoft Visual Studio with the native Free Pascal debug symbols (DWARF) is not a great experience.

A quick solution is to use [cv2pdb](https://github.com/rainers/cv2pdb) to convert the symbols to a PDB file suited for better MSVS support. Download `cv2pdb` and run it with the `dss_capi_v7d.dll` as a parameter. This will produce a `dss_capi_v7d.pdb` file, and remove the debug info from the original DLL. We don't distribute the PDB files as the conversion process requires stripping the DLL from the DWARF information required by the other tools.

With the PDB file produced, start Visual Studio. From the UI, you can click "Continue without code" and then start a debug session. From simply debugging, you can start you hosting process (e.g. Python) outside Visual Studio and use "Debug" / "Attach to Process..." to bind with it. 

Besides debugging, you can also use the profiling tools for further analysis.

**Hint:** if you build DSS C-API yourself and cv2pdb is found in your executable path, the PDB will be created in the build process.
