if [ -n "$TRAVIS_TAG" ]; then
    export DSS_CAPI_VERSION=$TRAVIS_TAG
elif [ -n "$APPVEYOR_REPO_TAG_NAME" ]; then
    export DSS_CAPI_VERSION=$APPVEYOR_REPO_TAG_NAME
else
    export DSS_CAPI_VERSION=`grep DSS_CAPI_VERSION include/dss_capi.h | grep -o '".*"' | tr -d '"'`
fi

export DSS_CAPI_REV=`git rev-parse HEAD`
export DSS_CAPI_SVN_REV=`git log | grep -m 1 -E "trunk@[0-9]+" -o | grep -E "[0-9]+" -o`

# echo 'Updating src/CAPI/CAPI_Metadata.pas...'
# cat src/CAPI/CAPI_Metadata.pas

echo "UNIT CAPI_metadata;" > src/CAPI/CAPI_Metadata.pas
echo "INTERFACE" >> src/CAPI/CAPI_Metadata.pas
echo "" >> src/CAPI/CAPI_Metadata.pas
echo "Const" >> src/CAPI/CAPI_Metadata.pas
echo "   DSS_CAPI_VERSION='${DSS_CAPI_VERSION}';" >> src/CAPI/CAPI_Metadata.pas
echo "   DSS_CAPI_REV='${DSS_CAPI_REV}';" >> src/CAPI/CAPI_Metadata.pas
echo "   DSS_CAPI_SVN_REV='${DSS_CAPI_SVN_REV}';" >> src/CAPI/CAPI_Metadata.pas
echo "" >> src/CAPI/CAPI_Metadata.pas
echo "IMPLEMENTATION" >> src/CAPI/CAPI_Metadata.pas
echo "" >> src/CAPI/CAPI_Metadata.pas
echo "END." >> src/CAPI/CAPI_Metadata.pas

echo 'Updated src/CAPI/CAPI_Metadata.pas'
echo '// --->'
cat src/CAPI/CAPI_Metadata.pas
echo '// <---'
