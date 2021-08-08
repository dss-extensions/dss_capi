#!/bin/bash
# Expose copies of some functions for backwards compatibility
if [ "$#" -ne 1 ]; then
    echo "Please pass the output path as parameter to this script"
    exit 1
fi

resfile=$(ls "$1"/*.res)

if [[ $1 == *"linux"* ]]; then
    sed -i 's/\(      LoadShapes_Set_Sinterval;\)/\1\n      LoadShapes_Set_SInterval;/' "$resfile"
    sed -i 's/\(      LoadShapes_Get_sInterval;\)/\1\n      LoadShapes_Get_SInterval;/' "$resfile"
    sed -i 's/\(^VERSION$\)/LoadShapes_Set_SInterval = LoadShapes_Set_Sinterval;\nLoadShapes_Get_SInterval = LoadShapes_Get_sInterval;\n\1/' "$resfile"
else
    echo '-alias_list' >> "$resfile"
    echo 'src/darwin_alias_list.txt' >> "$resfile"
    cut -d " " -f 2 'src/darwin_alias_list.txt' >> "$1"/linksyms.fpc
fi

"$1"/ppas.sh
rm -f "$1"/ppas.sh "$resfile" "$1"/linksyms.fpc

