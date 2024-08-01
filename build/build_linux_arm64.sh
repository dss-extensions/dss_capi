#!/bin/bash

# Tested with a Raspberry Pi 4 Model B
set -e -x

mkdir  -p lib/linux_arm64/
python3 src/classic_to_ctx.py

FPC_FLAGS=
if [[ "x${DSS_CAPI_BUILD_INC}" != "x1" ]]; then
    rm -rf build/units_arm64 build/units_arm64_dbg
    FPC_FLAGS=-B
fi

if [[ "x${DSS_CAPI_BUILD_DBG}" != "x1" ]]; then
    mkdir -p build/units_arm64
    fpc @src/linux-arm64.cfg ${FPC_FLAGS} src/dss_capi.lpr
fi

mkdir -p build/units_arm64_dbg
fpc @src/linux-arm64-dbg.cfg ${FPC_FLAGS} src/dss_capid.lpr

if [[ "x${DSS_CAPI_BUILD_DBG}" != "x1" ]]; then
    mkdir -p release/dss_capi/lib
    cp -R lib/linux_arm64 release/dss_capi/lib/linux_arm64
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
    tar zcf "dss_capi_${DSS_CAPI_VERSION}_linux_arm64.tar.gz" dss_capi
    cd ..
    rm -rf release/dss_capi

    ls release
fi