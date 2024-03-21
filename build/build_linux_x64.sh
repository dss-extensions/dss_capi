#!/bin/bash

set -e -x

mkdir  -p lib/linux_x64/
python3 src/classic_to_ctx.py

rm -rf build/units_x64
mkdir build/units_x64
fpc -Px86_64 @src/linux-x64.cfg -B src/dss_capi.lpr

rm -rf build/units_x64
mkdir build/units_x64
fpc -Px86_64 @src/linux-x64-dbg.cfg -B src/dss_capid.lpr

if [[ "x${DSS_CAPI_BUILD_ODDIE}" == "x1" ]]; then
    rm -rf build/oddie 
    mkdir build/oddie
    cd build/oddie
    cmake -DCMAKE_BUILD_TYPE=Release ../../src/altdss_oddie
    cmake --build . --config Release
    cd ../..
fi

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
