# Tested with arm7l
set -e -x

if [ ! -d "build/units_arm32" ]; then
    mkdir build/units_arm32
fi
fpc @src/linux-arm32.cfg -B src/dss_capi.lpr
bash custom_link.sh lib/linux_arm32
fpc @src/linux-arm32-dbg.cfg -B src/dss_capid.lpr
bash custom_link.sh lib/linux_arm32

mkdir -p release/dss_capi/lib
cp -R lib/linux_arm32 release/dss_capi/lib/linux_arm32
cp -R include release/dss_capi/
# cp -R examples release/dss_capi/
cp LICENSE release/dss_capi/
cp OPENDSS_LICENSE release/dss_capi/
cp klusolve/LICENSE release/dss_capi/KLUSOLVE_LICENSE
cd release
tar zcf "dss_capi_${TRAVIS_TAG}_linux_arm32.tar.gz" dss_capi
cd ..
rm -rf release/dss_capi

ls release
