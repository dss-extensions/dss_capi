#!/bin/bash

set -e -x

mkdir -p lib/darwin_x64/
python3 src/classic_to_ctx.py

export LDFLAGS=-L`pwd`/lib/darwin_x64/

if [[ "x${DSS_CAPI_BUILD_CMAKE}" == "x1" ]]; then
    cmake . ${DSS_EXTENSIONS_EXTRA_CMAKE_FLAGS} -DDSS_EXTENSIONS=ON -DBUILD_SHARED_LIBS=ON -DCMAKE_BUILD_TYPE=Release -B build/cmake -DUSE_SYSTEM_EIGEN=OFF -DUSE_SYSTEM_SUITESPARSE=OFF -DBUILD_KLUSOLVEX=OFF
    cmake --build build/cmake --config Release -j

    #TODO: if we decide to build OpenDSS-C here, share any downloads from build/cmake to build/cmake-debug

    cmake . ${DSS_EXTENSIONS_EXTRA_CMAKE_FLAGS} -DDSS_EXTENSIONS=ON -DBUILD_SHARED_LIBS=ON -DCMAKE_BUILD_TYPE=Debug -B build/cmake-debug -DUSE_SYSTEM_EIGEN=OFF -DUSE_SYSTEM_SUITESPARSE=OFF -DBUILD_KLUSOLVEX=OFF
    cmake --build build/cmake-debug --config Debug -j
fi

FPC_FLAGS=
if [[ "x${DSS_CAPI_BUILD_INC}" != "x1" ]]; then
    rm -rf build/units_x64 build/units_x64_dbg
    FPC_FLAGS=-B
fi

if [[ "x${DSS_CAPI_BUILD_DBG}" != "x1" ]]; then
    mkdir -p build/units_x64 
    fpc -Px86_64 @src/darwin-x64.cfg ${FPC_FLAGS} src/altdss_capi.pas
    # Make the lib look in the same folder for KLUSolveX
    DSS_CAPI_LIB="lib/darwin_x64/libaltdss_capi.dylib"
    CURRENT_LIBKLUSOLVE=`otool -L "$DSS_CAPI_LIB" | grep libklusolvex | cut -f 1 -d ' ' | sed $'s/^[ \t]*//'`
    NEW_LIBKLUSOLVE="@loader_path/./libklusolvex.dylib"
    install_name_tool -change "$CURRENT_LIBKLUSOLVE" "$NEW_LIBKLUSOLVE" "$DSS_CAPI_LIB"
    install_name_tool -id "@loader_path/./libaltdss_capi.dylib" "$DSS_CAPI_LIB"
fi

mkdir -p build/units_x64_dbg
fpc -Px86_64 @src/darwin-x64-dbg.cfg ${FPC_FLAGS} src/altdss_capid.pas

# Make the lib look in the same folder for KLUSolveX
DSS_CAPI_LIB="lib/darwin_x64/libaltdss_capid.dylib"
CURRENT_LIBKLUSOLVE=`otool -L "$DSS_CAPI_LIB" | grep libklusolvex | cut -f 1 -d ' ' | sed $'s/^[ \t]*//'`
NEW_LIBKLUSOLVE="@loader_path/./libklusolvex.dylib"
install_name_tool -change "$CURRENT_LIBKLUSOLVE" "$NEW_LIBKLUSOLVE" "$DSS_CAPI_LIB"
install_name_tool -id "@loader_path/./libaltdss_capi.dylib" "$DSS_CAPI_LIB"

if [[ "x${DSS_CAPI_BUILD_DBG}" != "x1" ]]; then
    mkdir -p release/dss_capi/lib
    cp -R lib/darwin_x64 release/dss_capi/lib/darwin_x64
    cp -R include release/dss_capi/
    # cp -R examples release/dss_capi/
    cp LICENSE release/dss_capi/
    cp OPENDSS_LICENSE release/dss_capi/
    if [ -d "klusolvex" ]; then
        cp klusolvex/LICENSE release/dss_capi/KLUSOLVE_LICENSE
    else  
        cp ../klusolvex/LICENSE release/dss_capi/KLUSOLVE_LICENSE
    fi
    cd release
    tar zcf "dss_capi_${DSS_CAPI_VERSION}_darwin_x64.tar.gz" dss_capi
    cd ..
    rm -rf release/dss_capi
fi