if [ ! -d "build/units_main" ]; then
    mkdir build/units_main
fi
fpc -Px86_64 @src/main/linux.cfg -B src/main/dss_capi.lpr

if [ ! -d "build/units_pm" ]; then
    mkdir build/units_pm
fi
fpc -Px86_64 @src/pm/linux.cfg -B src/pm/dsspm_capi.lpr