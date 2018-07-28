if [ ! -d "build/units_v7_x64" ]; then
    mkdir build/units_v7_x64
fi
fpc -Px86_64 @src/v7/darwin-x64.cfg -B src/v7/dss_capi_v7.lpr

if [ ! -d "build/units_v8_x64" ]; then
    mkdir build/units_v8_x64
fi
fpc -Px86_64 @src/v8/darwin-x64.cfg -B src/v8/dss_capi_v8.lpr