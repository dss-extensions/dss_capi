@echo off
@SETLOCAL

where /q fpc
if errorlevel 1 (
    if exist c:\lazarus\fpc\3.2.0\bin\x86_64-win64\fpc.exe (
        set "PATH=%PATH%;c:\lazarus\fpc\3.2.0\bin\x86_64-win64"
    ) else (
        echo ERROR: Please put fpc.exe in your executable search path and try again.
        exit /B 1
    )
)

if not exist .\build\units_v7_x64 (
    mkdir .\build\units_v7_x64
) 

REM if not exist .\build\units_v8_x64 (
    REM mkdir .\build\units_v8_x64
REM ) 


fpc -Px86_64 @src\v7\windows-x64.cfg -B src\v7\dss_capi_v7.lpr
if errorlevel 1 exit /B 1
if exist lib\win_x64\dss_capi_v7.dll (
    where /q dumpbin
    if errorlevel 1 (
        echo WARNING: dumpbin.exe is not in your path. Be sure to run this script on 
        echo          the "x64 Native Tools Command Prompt for VS 2017" or the 
        echo          equivalent for your Visual Studio version.
        exit /B 1
    )
    dumpbin /exports "lib\win_x64\dss_capi_v7.dll" > lib\win_x64\exports.txt
    echo LIBRARY DSS_CAPI_V7 > lib\win_x64\dss_capi_v7.def
    echo EXPORTS >> lib\win_x64\dss_capi_v7.def
    for /f "skip=19 tokens=4" %%A in (lib\win_x64\exports.txt) do echo %%A >> lib\win_x64\dss_capi_v7.def
    lib /def:lib\win_x64\dss_capi_v7.def /out:lib\win_x64\dss_capi_v7.lib /machine:X64
    dlltool --as-flags=--64 -d lib\win_x64\dss_capi_v7.def -m i386:x86-64 -l lib\win_x64\dss_capi_v7.dll.a
    
    del /s lib\win_x64\dss_capi_v7.exp
    del /s lib\win_x64\dss_capi_v7.def
    del /s lib\win_x64\exports.txt
) else (
    echo ERROR: DSS_CAPI_V7.DLL file not found. Check previous messages for possible causes.
    exit /B 1
)

REM fpc -Px86_64 @src\v8\windows-x64.cfg -B src\v8\dss_capi_v8.lpr
REM if errorlevel 1 exit /B 1
REM if exist lib\win_x64\dss_capi_v8.dll (
    REM where /q dumpbin
    REM if errorlevel 1 (
        REM echo WARNING: dumpbin.exe is not in your path. Be sure to run this script on 
        REM echo          the "x64 Native Tools Command Prompt for VS 2017" or the 
        REM echo          equivalent for your Visual Studio version.
        REM exit /B 1
    REM )
    REM dumpbin /exports "lib\win_x64\dss_capi_v8.dll" > lib\win_x64\exports.txt
    REM echo LIBRARY DSS_CAPI_V8 > lib\win_x64\dss_capi_v8.def
    REM echo EXPORTS >> lib\win_x64\dss_capi_v8.def
    REM for /f "skip=19 tokens=4" %%A in (lib\win_x64\exports.txt) do echo %%A >> lib\win_x64\dss_capi_v8.def
    REM lib /def:lib\win_x64\dss_capi_v8.def /out:lib\win_x64\dss_capi_v8.lib /machine:X64
    REM dlltool --as-flags=--64 -d lib\win_x64\dss_capi_v8.def -m i386:x86-64 -l lib\win_x64\dss_capi_v8.dll.a
    
    REM del /s lib\win_x64\dss_capi_v8.exp
    REM del /s lib\win_x64\dss_capi_v8.def
    REM del /s lib\win_x64\exports.txt
REM ) else (
    REM echo ERROR: DSS_CAPI_V8.DLL file not found. Check previous messages for possible causes.
REM )


SETLOCAL ENABLEEXTENSIONS

IF DEFINED APPVEYOR (
    mkdir release
    mkdir dss_capi
    xcopy /E lib\win_x64 release\dss_capi\lib\win_x64\
    xcopy /E include release\dss_capi\include\
    REM xcopy /E examples release\dss_capi\examples\
    copy LICENSE release\dss_capi\
    copy OPENDSS_LICENSE release\dss_capi\
    copy klusolve\LICENSE release\dss_capi\KLUSOLVE_LICENSE
    cd release
    7z a "dss_capi_%APPVEYOR_REPO_TAG_NAME%_win_x64.zip" dss_capi
    cd ..
    rd /s /q release\dss_capi
    appveyor PushArtifact "c:\dss_capi\release\dss_capi_%APPVEYOR_REPO_TAG_NAME%_win_x64.zip"
)
