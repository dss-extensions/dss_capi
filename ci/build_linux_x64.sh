set -e -x

# Install Free Pascal
cd ..
wget https://sourceforge.net/projects/freepascal/files/Linux/3.0.4/fpc-3.0.4.x86_64-linux.tar/download -Ofpc.tar -q
tar xf fpc.tar
cd fpc-3.0.4.x86_64-linux
echo > fpc_install_options.txt
echo n >> fpc_install_options.txt
echo n >> fpc_install_options.txt
echo n >> fpc_install_options.txt
sh ./install.sh < fpc_install_options.txt
export PATH=$PATH:~/fpc-3.0.4/bin

cd ../dss_capi

# Download DSS-Extensions KLUSolve
wget "${KLUSOLVE_URL}" -Oklusolve.tar.gz -q
tar zxf klusolve.tar.gz
cp -r klusolve/lib/* ./lib/

# Build DSS C-API
bash make_metadata.sh
bash build_linux_x64.sh
