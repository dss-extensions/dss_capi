@echo on
@SETLOCAL

if not exist lib\win_x64 ( mkdir lib\win_x64 )
python src\classic_to_ctx.py

where /q fpc
if errorlevel 1 (
    if exist c:\lazarus\fpc\3.2.2\bin\x86_64-win64\fpc.exe (
        set "PATH=%PATH%;c:\lazarus\fpc\3.2.2\bin\x86_64-win64"
    ) else (
        echo ERROR: Please put fpc.exe in your executable search path and try again.
        exit /B 1
    )
)

IF DEFINED DSS_CAPI_BUILD_CMAKE (
    cmake . -DDSS_EXTENSIONS=ON -DBUILD_SHARED_LIBS=ON -DCMAKE_BUILD_TYPE=Release -B build/cmake -DUSE_SYSTEM_EIGEN=OFF -DUSE_SYSTEM_SUITESPARSE=OFF -DBUILD_KLUSOLVEX=OFF
    cmake --build build/cmake --config Release -j

    REM TODO: if we decide to build OpenDSS-C here, share any downloads from build/cmake to build/cmake-debug

    cmake . -DDSS_EXTENSIONS=ON -DBUILD_SHARED_LIBS=ON -DCMAKE_BUILD_TYPE=Debug -B build/cmake-debug -DUSE_SYSTEM_EIGEN=OFF -DUSE_SYSTEM_SUITESPARSE=OFF -DBUILD_KLUSOLVEX=OFF
    cmake --build build/cmake-debug --config Debug -j
)

rd /s /q build\units_x64
mkdir .\build\units_x64
fpc -Px86_64 @src\windows-x64.cfg -B src\altdss_capi.pas
if errorlevel 1 exit /B 1
if exist lib\win_x64\altdss_capi.dll (
    where /q dumpbin
    if errorlevel 1 (
        echo WARNING: dumpbin.exe is not in your path. Be sure to run this script on 
        echo          the "Developer Command Prompt for VS 2019" or the 
        echo          equivalent for your Visual Studio version.
        exit /B 1
    )
    dumpbin /exports "lib\win_x64\altdss_capi.dll" > lib\win_x64\exports.txt
    echo LIBRARY ALTDSS_CAPI > lib\win_x64\altdss_capi.def
    @echo off
    echo EXPORTS >> lib\win_x64\altdss_capi.def
    for /f "skip=19 tokens=4" %%A in (lib\win_x64\exports.txt) do echo %%A >> lib\win_x64\altdss_capi.def
    @echo on
    lib /def:lib\win_x64\altdss_capi.def /out:lib\win_x64\altdss_capi.lib /machine:X64
    dlltool --as-flags=--64 -d lib\win_x64\altdss_capi.def -m i386:x86-64 -l lib\win_x64\altdss_capi.dll.a
    
    del /s lib\win_x64\altdss_capi.exp
    del /s lib\win_x64\altdss_capi.def
    del /s lib\win_x64\exports.txt
) else (
    echo ERROR: ALTDSS_CAPI.DLL file not found. Check previous messages for possible causes.
    exit /B 1
)

rd /s /q build\units_x64_dbg
mkdir .\build\units_x64_dbg
fpc -Px86_64 @src\windows-x64-dbg.cfg -B src\altdss_capid.pas
if errorlevel 1 exit /B 1
if exist lib\win_x64\altdss_capid.dll (
    where /q dumpbin
    if errorlevel 1 (
        echo WARNING: dumpbin.exe is not in your path. Be sure to run this script on 
        echo          the "Developer Command Prompt for VS 2019" or the 
        echo          equivalent for your Visual Studio version.
        exit /B 1
    )
    dumpbin /exports "lib\win_x64\altdss_capid.dll" > lib\win_x64\exports.txt
    echo LIBRARY ALTDSS_CAPID > lib\win_x64\altdss_capid.def
    echo EXPORTS >> lib\win_x64\altdss_capid.def
    @echo off
    for /f "skip=19 tokens=4" %%A in (lib\win_x64\exports.txt) do echo %%A >> lib\win_x64\altdss_capid.def
    @echo on
    lib /def:lib\win_x64\altdss_capid.def /out:lib\win_x64\altdss_capid.lib /machine:X64
    dlltool --as-flags=--64 -d lib\win_x64\altdss_capid.def -m i386:x86-64 -l lib\win_x64\altdss_capid.dll.a
    
    del /s lib\win_x64\altdss_capid.exp
    del /s lib\win_x64\altdss_capid.def
    del /s lib\win_x64\exports.txt
    
    where /q cv2pdb
    if errorlevel 1 (
        echo WARNING: cv2pdb not found, PDB file will not be created.
    ) else (
        echo Creating PDB file...
        cv2pdb lib\win_x64\altdss_capid.dll
    )
) else (
    echo ERROR: ALTDSS_CAPID.DLL file not found. Check previous messages for possible causes.
    exit /B 1
)

SETLOCAL ENABLEEXTENSIONS

IF DEFINED CI (
    REM Build our COM DLL (DSSExtensions.DLL)
    msbuild src\COM\COM_DSSExtensions.vcxproj /p:Configuration=Release /p:Platform=x64 /p:OutputPath=%cd%\lib\win_x64 /p:RegisterOutput=false

    REM Pack outputs
    mkdir release
    mkdir dss_capi
    xcopy /E lib\win_x64 release\dss_capi\lib\win_x64\
    xcopy /E include release\dss_capi\include\
    REM xcopy /E examples release\dss_capi\examples\
    copy LICENSE release\dss_capi\
    copy OPENDSS_LICENSE release\dss_capi\
    copy klusolvex\LICENSE release\dss_capi\KLUSOLVE_LICENSE
    cd release
    7z a "dss_capi_%DSS_CAPI_VERSION%_win_x64.zip" dss_capi
    cd ..
    rd /s /q release\dss_capi
)
