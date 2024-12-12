#!/bin/bash

# Tested with a phone (arm7l)
# Tested with a Raspberry Pi 4 Model B with the official 32-bit OS

set -e -x

mkdir -p lib/linux_arm32/
python3 src/classic_to_ctx.py

if [[ "x${DSS_CAPI_BUILD_CMAKE}" == "x1" ]]; then
    cmake . -DDSS_EXTENSIONS=ON -DBUILD_SHARED_LIBS=ON -DCMAKE_BUILD_TYPE=Release -B build/cmake -DUSE_SYSTEM_EIGEN=OFF -DUSE_SYSTEM_SUITESPARSE=OFF -DBUILD_KLUSOLVEX=ON
    cmake --build build/cmake --config Release -j

    #TODO: if we decide to build OpenDSS-C here, share any downloads from build/cmake to build/cmake-debug

    cmake . -DDSS_EXTENSIONS=ON -DBUILD_SHARED_LIBS=ON -DCMAKE_BUILD_TYPE=Debug  -B build/cmake-debug -DUSE_SYSTEM_EIGEN=OFF -DUSE_SYSTEM_SUITESPARSE=OFF -DBUILD_KLUSOLVEX=ON
    cmake --build build/cmake-debug --config Debug -j
fi

FPC_FLAGS=
if [[ "x${DSS_CAPI_BUILD_INC}" != "x1" ]]; then
    rm -rf build/units_arm32 build/units_arm32_dbg
    FPC_FLAGS=-B
fi

if [[ "x${DSS_CAPI_BUILD_DBG}" != "x1" ]]; then
    mkdir -p build/units_arm32
    fpc @src/linux-arm32.cfg ${FPC_FLAGS} src/altdss_capi.pas
fi

mkdir -p build/units_arm32_dbg
fpc @src/linux-arm32-dbg.cfg ${FPC_FLAGS} src/altdss_capid.pas

if [[ "x${DSS_CAPI_BUILD_DBG}" != "x1" ]]; then
    mkdir -p release/dss_capi/lib
    cp -R lib/linux_arm32 release/dss_capi/lib/linux_arm32
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
    tar zcf "dss_capi_${DSS_CAPI_VERSION}_linux_arm32.tar.gz" dss_capi
    cd ..
    rm -rf release/dss_capi

    ls release
fi
