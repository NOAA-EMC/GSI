#!/bin/bash

ANN_DIR=/net/orbit095l/disk2/pub/mspps/avn
SCRIPTS_DIR=/home/pub/wchen/mirs_working/mspps/scripts
IDL=/usr/local/bin/idl

avn_files=(`ls ${ANN_DIR}/avn_[t-v]*.bin`)

for file in ${avn_files[*]} ; do

echo $file
cd ${SCRIPTS_DIR}
${IDL} << EOF
.r swap_endian_avn.pro
swap_endian_avn, $file
exit
EOF
  
done
