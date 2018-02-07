if [ ! -d "src/units" ]; then
    mkdir src/units
fi
fpc -Px86_64 @src/linux.cfg -B src/dss_capi.lpr