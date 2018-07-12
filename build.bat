@echo off

where /q fpc
if errorlevel 1 (
    if exist c:\lazarus\fpc\3.0.4\bin\x86_64-win64\fpc.exe (
        set "PATH=%PATH%;c:\lazarus\fpc\3.0.4\bin\x86_64-win64"
    ) else (
        echo ERROR: Please put fpc.exe in your executable search path and try again.
        exit /B
    )
)

if not exist .\build\units_main (
    mkdir .\build\units_main
) 

if not exist .\build\units_pm (
    mkdir .\build\units_pm
) 

if exist ..\electricdss-src\Source\Common\DSSGlobals.pas (
    fpc -Px86_64 @src\main\windows.cfg -B src\main\dss_capi.lpr
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

        REM copy /Y ..\electricdss-src\Distrib\x64\klusolve.dll lib\libklusolve.dll
        echo TODO: COPY KLUSOLVE DLL!
    ) else (
        echo ERROR: DSS_CAPI.DLL file not found. Check previous messages for possible causes.
        exit /B
    )

    fpc -Px86_64 @src\pm\windows.cfg -B src\pm\dsspm_capi.lpr
    if exist lib\dsspm_capi.dll (
        where /q dumpbin
        if errorlevel 1 (
            echo WARNING: dumpbin.exe is not in your path. Be sure to run this script on 
            echo          the "x64 Native Tools Command Prompt for VS 2017" or the 
            echo          equivalent for your Visual Studio version.
            exit /B
        )
        dumpbin /exports "lib\dsspm_capi.dll" > lib\exports.txt
        echo LIBRARY DSSPM_CAPI > lib\dsspm_capi.def
        echo EXPORTS >> lib\dsspm_capi.def
        for /f "skip=19 tokens=4" %%A in (lib\exports.txt) do echo %%A >> lib\dsspm_capi.def
        lib /def:lib\dsspm_capi.def /out:lib\dsspm_capi.lib /machine:X64
        del /s lib\dsspm_capi.exp
        del /s lib\dsspm_capi.def
        del /s lib\exports.txt
        
        REM copy /Y ..\electricdss-src\Distrib\x64\klusolve.dll lib\libklusolve.dll
        echo TODO: COPY KLUSOLVE DLL!
    ) else (
        echo ERROR: DSSPM_CAPI.DLL file not found. Check previous messages for possible causes.
    )
    
) else (
    echo ERROR: Did you forget to clone https://github.com/PMeira/electricdss-src ?
    exit /B
)
