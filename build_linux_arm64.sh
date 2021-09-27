# Tested with a Raspberry Pi 4 Model B
set -e -x

python3 src/classic_to_ctx.py

if [ ! -d "build/units_arm64" ]; then
    mkdir build/units_arm64
fi
fpc @src/linux-arm64.cfg -B src/dss_capi.lpr
bash custom_link.sh lib/linux_arm64
fpc @src/linux-arm64-dbg.cfg -B src/dss_capid.lpr
bash custom_link.sh lib/linux_arm64

mkdir -p release/dss_capi/lib
cp -R lib/linux_arm64 release/dss_capi/lib/linux_arm64
cp -R include release/dss_capi/
# cp -R examples release/dss_capi/
cp LICENSE release/dss_capi/
cp OPENDSS_LICENSE release/dss_capi/
if [ -d "klusolve" ]; then
    cp klusolve/LICENSE release/dss_capi/KLUSOLVE_LICENSE
else  
    cp ../klusolve/LICENSE release/dss_capi/KLUSOLVE_LICENSE
fi
cd release
tar zcf "dss_capi_${TRAVIS_TAG}_linux_arm64.tar.gz" dss_capi
cd ..
rm -rf release/dss_capi

ls release
