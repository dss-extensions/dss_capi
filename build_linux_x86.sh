set -e -x

python3 src/classic_to_ctx.py

if [ ! -d "build/units_x86" ]; then
    mkdir build/units_x86
fi
fpc -Pi386 @src/linux-x86.cfg -B src/dss_capi.lpr
bash custom_link.sh lib/linux_x86
fpc -Pi386 @src/linux-x86-dbg.cfg -B src/dss_capid.lpr
bash custom_link.sh lib/linux_x86

mkdir -p release/dss_capi/lib
cp -R lib/linux_x86 release/dss_capi/lib/linux_x86
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
tar zcf "dss_capi_${GITHUB_REF}_linux_x86.tar.gz" dss_capi
cd ..
rm -rf release/dss_capi

ls release
