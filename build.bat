@echo off

where /q fpc
if errorlevel 1 (
    if exist c:\lazarus\fpc\3.0.0\bin\x86_64-win64\fpc.exe (
        set "PATH=%PATH%;c:\lazarus\fpc\3.0.0\bin\x86_64-win64"
    ) else (
        echo ERROR: Please put fpc.exe in your executable search path and try again.
        exit /B
    )
)

if not exist .\src\units (
    mkdir .\src\units
) 

if exist ..\electricdss\Source\Common\DSSGlobals.pas (
    fpc -Px86_64 @src\windows.cfg -B src\dss_capi.lpr
    if exist lib\dss_capi.dll (
        where /q dumpbin
        if errorlevel 1 (
            echo WARNING: dumpbin.exe is not in your path. Be sure to run this script on 
            echo          the "x64 Native Tools Command Prompt for VS 2017" or the 
            echo          equivalent for your Visual Studio version.
            exit /B
        )
        dumpbin /exports "lib\dss_capi.dll" > lib\exports.txt
        echo LIBRARY DSS_CAPI > lib\dss_capi.def
        echo EXPORTS >> lib\dss_capi.def
        for /f "skip=19 tokens=4" %%A in (lib\exports.txt) do echo %%A >> lib\dss_capi.def
        lib /def:lib\dss_capi.def /out:lib\dss_capi.lib /machine:X64
        del /s lib\dss_capi.exp
        del /s lib\dss_capi.def
        del /s lib\exports.txt
        copy /Y ..\electricdss\Distrib\x64\klusolve.dll lib\libklusolve.dll
    ) else (
        echo ERROR: DLL file not found. Check previous messages for possible causes.
    )
) else (
    echo ERROR: Please copy or link the OpenDSS source code repository to ..\electricdss and try again.
    exit /B
)
