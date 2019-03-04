set -e -x

# Build DSS C-API
cd /io/dss_capi
bash make_metadata.sh
bash build_linux_x86.sh
