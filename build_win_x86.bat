@echo off
@SETLOCAL

where /q fpc
if errorlevel 1 (
    if exist c:\lazarus\fpc\3.2.0\bin\i386-win32\fpc.exe (
        set "PATH=%PATH%;c:\lazarus\fpc\3.2.0\bin\i386-win32"
    ) else (
        echo ERROR: Please put fpc.exe in your executable search path and try again.
        exit /B 1
    )
)

if not exist .\build\units_x86 (
    mkdir .\build\units_x86
) 

REM if not exist .\build\units_v8_x86 (
    REM mkdir .\build\units_v8_x86
REM ) 

fpc -Pi386 @src\windows-x86.cfg -B src\dss_capi.lpr
if errorlevel 1 exit /B 1
if exist lib\win_x86\dss_capi.dll (
    where /q dumpbin
    if errorlevel 1 (
        echo WARNING: dumpbin.exe is not in your path. Be sure to run this script on 
        echo          the "Developer Command Prompt for VS 2019" or the 
        echo          equivalent for your Visual Studio version.
        exit /B 1
    )
    dumpbin /exports "lib\win_x86\dss_capi.dll" > lib\win_x86\exports.txt
    echo LIBRARY DSS_CAPI > lib\win_x86\dss_capi.def
    echo EXPORTS >> lib\win_x86\dss_capi.def
    for /f "skip=19 tokens=4" %%A in (lib\win_x86\exports.txt) do echo %%A >> lib\win_x86\dss_capi.def
    lib /def:lib\win_x86\dss_capi.def /out:lib\win_x86\dss_capi.lib /machine:X86
    dlltool -d lib\win_x86\dss_capi.def -m i386 -l lib\win_x86\dss_capi.dll.a
    
    del /s lib\win_x86\dss_capi.exp
    del /s lib\win_x86\dss_capi.def
    del /s lib\win_x86\exports.txt
) else (
    echo ERROR: DSS_CAPI.DLL file not found. Check previous messages for possible causes.
    exit /B 1
)

fpc -Pi386 @src\windows-x86-dbg.cfg -B src\dss_capid.lpr
if errorlevel 1 exit /B 1
if exist lib\win_x86\dss_capid.dll (
    where /q dumpbin
    if errorlevel 1 (
        echo WARNING: dumpbin.exe is not in your path. Be sure to run this script on 
        echo          the "Developer Command Prompt for VS 2019" or the 
        echo          equivalent for your Visual Studio version.
        exit /B 1
    )
    dumpbin /exports "lib\win_x86\dss_capid.dll" > lib\win_x86\exports.txt
    echo LIBRARY DSS_CAPID > lib\win_x86\dss_capid.def
    echo EXPORTS >> lib\win_x86\dss_capid.def
    for /f "skip=19 tokens=4" %%A in (lib\win_x86\exports.txt) do echo %%A >> lib\win_x86\dss_capid.def
    lib /def:lib\win_x86\dss_capid.def /out:lib\win_x86\dss_capid.lib /machine:X86
    dlltool -d lib\win_x86\dss_capid.def -m i386 -l lib\win_x86\dss_capid.dll.a
    
    del /s lib\win_x86\dss_capid.exp
    del /s lib\win_x86\dss_capid.def
    del /s lib\win_x86\exports.txt
) else (
    echo ERROR: DSS_CAPID.DLL file not found. Check previous messages for possible causes.
    exit /B 1
)

SETLOCAL ENABLEEXTENSIONS

IF DEFINED APPVEYOR (
    mkdir release
    mkdir dss_capi
    xcopy /E lib\win_x86 release\dss_capi\lib\win_x86\
    xcopy /E include release\dss_capi\include\
    REM xcopy /E examples release\dss_capi\examples\
    copy LICENSE release\dss_capi\
    copy OPENDSS_LICENSE release\dss_capi\
    copy klusolvex\LICENSE release\dss_capi\KLUSOLVEX_LICENSE
    cd release
    7z a "dss_capi_%APPVEYOR_REPO_TAG_NAME%_win_x86.zip" dss_capi
    cd ..
    rd /s /q release\dss_capi
    appveyor PushArtifact "c:\dss_capi\release\dss_capi_%APPVEYOR_REPO_TAG_NAME%_win_x86.zip"
)
