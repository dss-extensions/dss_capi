#!/bin/bash

set -e -x

mkdir  -p lib/linux_x64/
python3 src/classic_to_ctx.py

FPC_FLAGS=
if [[ "x${DSS_CAPI_BUILD_INC}" != "x1" ]]; then
    rm -rf build/units_x64 build/units_x64_dbg build/oddie
    FPC_FLAGS=-B
fi

if [[ "x${DSS_CAPI_BUILD_DBG}" != "x1" ]]; then
    mkdir -p build/units_x64
    fpc -Px86_64 @src/linux-x64.cfg ${FPC_FLAGS} src/dss_capi.lpr
fi

mkdir -p build/units_x64_dbg
fpc -Px86_64 @src/linux-x64-dbg.cfg ${FPC_FLAGS} src/dss_capid.lpr

if [[ "x${DSS_CAPI_BUILD_ODDIE}" == "x1" ]]; then
    mkdir -p build/oddie
    cd build/oddie
    cmake -DCMAKE_BUILD_TYPE=Release ../../src/altdss_oddie
    cmake --build . --config Release
    cd ../..
fi

if [[ "x${DSS_CAPI_BUILD_DBG}" != "x1" ]]; then
    mkdir -p release/dss_capi/lib
    cp -R lib/linux_x64 release/dss_capi/lib/linux_x64
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
    tar zcf "dss_capi_${DSS_CAPI_VERSION}_linux_x64.tar.gz" dss_capi
    cd ..
    rm -rf release/dss_capi

    ls release
fi
