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

if not exist .\build\units_v7_x64 (
    mkdir .\build\units_v7_x64
) 

if not exist .\build\units_v8_x64 (
    mkdir .\build\units_v8_x64
) 

if exist ..\electricdss-src\Source\Common\DSSGlobals.pas (
    fpc -Px86_64 @src\v7\windows-x64.cfg -B src\v7\dss_capi.lpr
    if exist lib\win_x64\v7\dss_capi.dll (
        where /q dumpbin
        if errorlevel 1 (
            echo WARNING: dumpbin.exe is not in your path. Be sure to run this script on 
            echo          the "x64 Native Tools Command Prompt for VS 2017" or the 
            echo          equivalent for your Visual Studio version.
            exit /B
        )
        dumpbin /exports "lib\win_x64\v7\dss_capi.dll" > lib\win_x64\v7\exports.txt
        echo LIBRARY DSS_CAPI > lib\win_x64\v7\dss_capi.def
        echo EXPORTS >> lib\win_x64\v7\dss_capi.def
        for /f "skip=19 tokens=4" %%A in (lib\win_x64\v7\exports.txt) do echo %%A >> lib\win_x64\v7\dss_capi.def
        lib /def:lib\win_x64\v7\dss_capi.def /out:lib\win_x64\v7\dss_capi.lib /machine:X64
        del /s lib\win_x64\v7\dss_capi.exp
        del /s lib\win_x64\v7\dss_capi.def
        del /s lib\win_x64\v7\exports.txt

        REM copy /Y ..\electricdss-src\Distrib\x64\klusolve.dll lib\libklusolve.dll
        echo TODO: COPY KLUSOLVE DLL!
    ) else (
        echo ERROR: v7/DSS_CAPI.DLL file not found. Check previous messages for possible causes.
        exit /B
    )

    fpc -Px86_64 @src\v8\windows-x64.cfg -B src\v8\dss_capi.lpr
    if exist lib\win_x64\v8\dss_capi.dll (
        where /q dumpbin
        if errorlevel 1 (
            echo WARNING: dumpbin.exe is not in your path. Be sure to run this script on 
            echo          the "x64 Native Tools Command Prompt for VS 2017" or the 
            echo          equivalent for your Visual Studio version.
            exit /B
        )
        dumpbin /exports "lib\win_x64\v8\dss_capi.dll" > lib\win_x64\v8\exports.txt
        echo LIBRARY DSS_CAPI > lib\win_x64\v8\dss_capi.def
        echo EXPORTS >> lib\win_x64\v8\dss_capi.def
        for /f "skip=19 tokens=4" %%A in (lib\win_x64\v8\exports.txt) do echo %%A >> lib\win_x64\v8\dss_capi.def
        lib /def:lib\win_x64\v8\dss_capi.def /out:lib\win_x64\v8\dss_capi.lib /machine:X64
        del /s lib\win_x64\v8\dss_capi.exp
        del /s lib\win_x64\v8\dss_capi.def
        del /s lib\win_x64\v8\exports.txt
        
        REM copy /Y ..\electricdss-src\Distrib\x64\klusolve.dll lib\libklusolve.dll
        echo TODO: COPY KLUSOLVE DLL!
    ) else (
        echo ERROR: v8/DSS_CAPI.DLL file not found. Check previous messages for possible causes.
    )
    
) else (
    echo ERROR: Did you forget to clone https://github.com/PMeira/electricdss-src ?
    exit /B
)
