#!/bin/bash
# Expose copies of some functions for backwards compatibility
if [ "$#" -ne 1 ]; then
    echo "Please pass the output path as parameter to this script"
    exit 1
fi
sed -i 's/\(      LoadShapes_Set_Sinterval;\)/\1\n      LoadShapes_Set_SInterval;/' $1/link.res 
sed -i 's/\(      LoadShapes_Get_sInterval;\)/\1\n      LoadShapes_Get_SInterval;/' $1/link.res 
sed -i 's/\(^VERSION$\)/LoadShapes_Set_SInterval = LoadShapes_Set_Sinterval;\nLoadShapes_Get_SInterval = LoadShapes_Get_sInterval;\n\1/' $1/link.res 

$1/ppas.sh
