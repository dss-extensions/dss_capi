set -e -x

cd /io

if [ ! -f /io/dss_capi/lib/linux_x86/libklusolve.so ]; then
    export SUITESPARSE_SRC=`readlink -f ./SuiteSparse`

    # Build KLUSolve
    cd /io
    rm -rf dss_capi/klusolve/build
    ls -lR dss_capi/klusolve
    mkdir /io/dss_capi/klusolve/build
    ln -s SuiteSparse dss_capi/klusolve/build/
    cd /io/dss_capi/klusolve/build
    cmake -DUSE_SYSTEM_SUITESPARSE=OFF -DCMAKE_CXX_COMPILER_ARG1=-m32 -DCMAKE_C_COMPILER_ARG1=-m32 ..
    cmake --build . --config Release
fi

# Build DSS C-API
cd /io/dss_capi
bash make_metadata.sh
bash build_linux_x86.sh
