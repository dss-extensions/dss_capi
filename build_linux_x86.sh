set -e -x

if [ ! -d "build/units_v7_x86" ]; then
    mkdir build/units_v7_x86
fi
fpc -Pi386 @src/v7/linux-x86.cfg -B src/v7/dss_capi_v7.lpr
bash custom_link.sh lib/linux_x86

# if [ ! -d "build/units_v8_x86" ]; then
    # mkdir build/units_v8_x86
# fi
# fpc -Pi386 @src/v8/linux-x86.cfg -B src/v8/dss_capi_v8.lpr
# bash custom_link.sh lib/linux_x86

mkdir -p release/dss_capi/lib
cp -R lib/linux_x86 release/dss_capi/lib/linux_x86
cp -R include release/dss_capi/
# cp -R examples release/dss_capi/
cp LICENSE release/dss_capi/
cp OPENDSS_LICENSE release/dss_capi/
cp klusolve/LICENSE release/dss_capi/KLUSOLVE_LICENSE
cd release
tar zcf "dss_capi_${TRAVIS_TAG}_linux_x86.tar.gz" dss_capi
cd ..
rm -rf release/dss_capi

ls release
