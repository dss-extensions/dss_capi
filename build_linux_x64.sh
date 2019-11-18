set -e -x

if [ ! -d "build/units_v7_x64" ]; then
    mkdir build/units_v7_x64
fi
fpc -Px86_64 @src/linux-x64.cfg -B src/dss_capi_v7.lpr
bash custom_link.sh lib/linux_x64

mkdir -p release/dss_capi/lib
cp -R lib/linux_x64 release/dss_capi/lib/linux_x64
cp -R include release/dss_capi/
# cp -R examples release/dss_capi/
cp LICENSE release/dss_capi/
cp OPENDSS_LICENSE release/dss_capi/
cp klusolve/LICENSE release/dss_capi/KLUSOLVE_LICENSE
cd release
tar zcf "dss_capi_${TRAVIS_TAG}_linux_x64.tar.gz" dss_capi
cd ..
rm -rf release/dss_capi

ls release
