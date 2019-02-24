@echo off
@SETLOCAL

where /q fpc
if errorlevel 1 (
    if exist c:\lazarus\fpc\3.0.4\bin\i386-win32\fpc.exe (
        set "PATH=%PATH%;c:\lazarus\fpc\3.0.4\bin\i386-win32"
    ) else (
        echo ERROR: Please put fpc.exe in your executable search path and try again.
        exit /B 1
    )
)

if not exist .\build\units_v7_x86 (
    mkdir .\build\units_v7_x86
) 

if not exist .\build\units_v8_x86 (
    mkdir .\build\units_v8_x86
) 

if exist ..\electricdss-src\Version7\Source\Common\DSSGlobals.pas (
    fpc -Pi386 @src\v7\windows-x86.cfg -B src\v7\dss_capi_v7.lpr
    if errorlevel 1 exit /B 1
    if exist lib\win_x86\dss_capi_v7.dll (
        where /q dumpbin
        if errorlevel 1 (
            echo WARNING: dumpbin.exe is not in your path. Be sure to run this script on 
            echo          the "x86 Native Tools Command Prompt for VS 2017" or the 
            echo          equivalent for your Visual Studio version.
            exit /B 1
        )
        dumpbin /exports "lib\win_x86\dss_capi_v7.dll" > lib\win_x86\exports.txt
        echo LIBRARY DSS_CAPI_V7 > lib\win_x86\dss_capi_v7.def
        echo EXPORTS >> lib\win_x86\dss_capi_v7.def
        for /f "skip=19 tokens=4" %%A in (lib\win_x86\exports.txt) do echo %%A >> lib\win_x86\dss_capi_v7.def
        lib /def:lib\win_x86\dss_capi_v7.def /out:lib\win_x86\dss_capi_v7.lib /machine:X86
        del /s lib\win_x86\dss_capi_v7.exp
        del /s lib\win_x86\dss_capi_v7.def
        del /s lib\win_x86\exports.txt
    ) else (
        echo ERROR: DSS_CAPI_V7.DLL file not found. Check previous messages for possible causes.
        exit /B 1
    )

    fpc -Pi386 @src\v8\windows-x86.cfg -B src\v8\dss_capi_v8.lpr
    if errorlevel 1 exit /B 1
    if exist lib\win_x86\dss_capi_v8.dll (
        where /q dumpbin
        if errorlevel 1 (
            echo WARNING: dumpbin.exe is not in your path. Be sure to run this script on 
            echo          the "x86 Native Tools Command Prompt for VS 2017" or the 
            echo          equivalent for your Visual Studio version.
            exit /B 1
        )
        dumpbin /exports "lib\win_x86\dss_capi_v8.dll" > lib\win_x86\exports.txt
        echo LIBRARY DSS_CAPI_V8 > lib\win_x86\dss_capi_v8.def
        echo EXPORTS >> lib\win_x86\dss_capi_v8.def
        for /f "skip=19 tokens=4" %%A in (lib\win_x86\exports.txt) do echo %%A >> lib\win_x86\dss_capi_v8.def
        lib /def:lib\win_x86\dss_capi_v8.def /out:lib\win_x86\dss_capi_v8.lib /machine:X86
        del /s lib\win_x86\dss_capi_v8.exp
        del /s lib\win_x86\dss_capi_v8.def
        del /s lib\win_x86\exports.txt
    ) else (
        echo ERROR: DSS_CAPI_V8.DLL file not found. Check previous messages for possible causes.
    )
    
) else (
    echo ERROR: Did you forget to clone https://github.com/PMeira/electricdss-src ?
    exit /B 1
)

SETLOCAL ENABLEEXTENSIONS

IF DEFINED APPVEYOR_REPO_TAG_NAME (
    mkdir release
    mkdir dss_capi
    xcopy /E lib\win_x86 release\dss_capi\lib\win_x86\
    xcopy /E include release\dss_capi\include\
    REM xcopy /E examples release\dss_capi\examples\
    copy LICENSE release\dss_capi\
    copy OPENDSS_LICENSE release\dss_capi\
    copy klusolve\LICENSE release\dss_capi\KLUSOLVE_LICENSE
    cd release
    7z a "dss_capi_%APPVEYOR_REPO_TAG_NAME%_win_x86.zip" dss_capi
    cd ..
    rd /s /q release\dss_capi
    appveyor PushArtifact "c:\dss_capi\release\dss_capi_%APPVEYOR_REPO_TAG_NAME%_win_x86.zip"
)
