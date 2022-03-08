#!/bin/bash

set -e -x

python3 src/classic_to_ctx.py

export LDFLAGS=-L`pwd`/lib/darwin_arm64/

rm -rf build/units_arm64
mkdir build/units_arm64
fpc -Paarch64 @src/darwin-arm64.cfg -B src/dss_capi.lpr

# Make the lib look in the same folder for KLUSolveX
DSS_CAPI_LIB="lib/darwin_arm64/libdss_capi.dylib"
CURRENT_LIBKLUSOLVE=`otool -L "$DSS_CAPI_LIB" | grep libklusolvex | cut -f 1 -d ' ' | sed $'s/^[ \t]*//'`
NEW_LIBKLUSOLVE="@loader_path/./libklusolvex.dylib"
install_name_tool -change "$CURRENT_LIBKLUSOLVE" "$NEW_LIBKLUSOLVE" "$DSS_CAPI_LIB"
install_name_tool -id "@loader_path/./libdss_capi.dylib" "$DSS_CAPI_LIB"

rm -rf build/units_arm64
mkdir build/units_arm64
fpc -Paarch64 @src/darwin-arm64-dbg.cfg -B src/dss_capid.lpr

# Make the lib look in the same folder for KLUSolveX
DSS_CAPI_LIB="lib/darwin_arm64/libdss_capid.dylib"
CURRENT_LIBKLUSOLVE=`otool -L "$DSS_CAPI_LIB" | grep libklusolvex | cut -f 1 -d ' ' | sed $'s/^[ \t]*//'`
NEW_LIBKLUSOLVE="@loader_path/./libklusolvex.dylib"
install_name_tool -change "$CURRENT_LIBKLUSOLVE" "$NEW_LIBKLUSOLVE" "$DSS_CAPI_LIB"
install_name_tool -id "@loader_path/./libdss_capi.dylib" "$DSS_CAPI_LIB"

mkdir -p release/dss_capi/lib
cp -R lib/darwin_arm64 release/dss_capi/lib/darwin_arm64
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
tar zcf "dss_capi_${DSS_CAPI_VERSION}_darwin_arm64.tar.gz" dss_capi
cd ..
rm -rf release/dss_capi
