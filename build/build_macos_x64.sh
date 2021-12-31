#!/bin/bash

set -e -x

python3 src/classic_to_ctx.py

export LDFLAGS=-L`pwd`/lib/darwin_x64/

rm -rf build/units_x64
mkdir build/units_x64
fpc -Px86_64 @src/darwin-x64.cfg -B src/dss_capi.lpr

# Make the lib look in the same folder for KLUSolveX
DSS_CAPI_LIB="lib/darwin_x64/libdss_capi.dylib"
CURRENT_LIBKLUSOLVE=`otool -L "$DSS_CAPI_LIB" | grep libklusolvex | cut -f 1 -d ' ' | sed $'s/^[ \t]*//'`
NEW_LIBKLUSOLVE="@loader_path/./libklusolvex.dylib"
install_name_tool -change "$CURRENT_LIBKLUSOLVE" "$NEW_LIBKLUSOLVE" "$DSS_CAPI_LIB"
install_name_tool -id "@loader_path/./libdss_capi.dylib" "$DSS_CAPI_LIB"

rm -rf build/units_x64
mkdir build/units_x64
fpc -Px86_64 @src/darwin-x64-dbg.cfg -B src/dss_capid.lpr

# Make the lib look in the same folder for KLUSolveX
DSS_CAPI_LIB="lib/darwin_x64/libdss_capid.dylib"
CURRENT_LIBKLUSOLVE=`otool -L "$DSS_CAPI_LIB" | grep libklusolvex | cut -f 1 -d ' ' | sed $'s/^[ \t]*//'`
NEW_LIBKLUSOLVE="@loader_path/./libklusolvex.dylib"
install_name_tool -change "$CURRENT_LIBKLUSOLVE" "$NEW_LIBKLUSOLVE" "$DSS_CAPI_LIB"
install_name_tool -id "@loader_path/./libdss_capi.dylib" "$DSS_CAPI_LIB"

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
