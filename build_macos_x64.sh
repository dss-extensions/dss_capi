if [ ! -d "build/units_v7_x64" ]; then
    mkdir build/units_v7_x64
fi
fpc -Px86_64 @src/v7/darwin-x64.cfg -B src/v7/dss_capi_v7.lpr

# Make the lib look in the same folder for KLUSolve
DSS_CAPI_LIB="lib/darwin_x64/v7/libdss_capi_v7.dylib"
CURRENT_LIBKLUSOLVE=`otool -L "$DSS_CAPI_LIB" | grep libklusolve | cut -f 1 -d ' ' | sed $'s/^[ \t]*//'`
NEW_LIBKLUSOLVE="@loader_path/./libklusolve.dylib"
install_name_tool -change "$CURRENT_LIBKLUSOLVE" "$NEW_LIBKLUSOLVE" "$DSS_CAPI_LIB"


if [ ! -d "build/units_v8_x64" ]; then
    mkdir build/units_v8_x64
fi
fpc -Px86_64 @src/v8/darwin-x64.cfg -B src/v8/dss_capi_v8.lpr


DSS_CAPI_LIB="lib/darwin_x64/v8/libdss_capi_v8.dylib"
CURRENT_LIBKLUSOLVE=`otool -L "$DSS_CAPI_LIB" | grep libklusolve | cut -f 1 -d ' ' | sed $'s/^[ \t]*//'`
NEW_LIBKLUSOLVE="@loader_path/./libklusolve.dylib"
install_name_tool -change "$CURRENT_LIBKLUSOLVE" "$NEW_LIBKLUSOLVE" "$DSS_CAPI_LIB"
