#!/bin/ksh
set -x

cd /mnt/lfs3/projects/rtwbl/mhu/GSI_r1181/util/EnKF/enspreproc_regional.fd/run

# Loop through each member
  no_member=17
  ensmem=1
  while [[ $ensmem -le $no_member ]];do

     print "\$ensmem is $ensmem"
     ensmemid=`printf %4.4i $ensmem`

# get background for each member
     cp wrf_inout wrfinput_d01.mem${ensmemid}

# next member
     (( ensmem += 1 ))

  done

cp /mnt/lfs3/projects/rtwbl/mhu/GSI_r1181/util/EnKF/initialens_regional.fd/initialens.x .

./initialens.x ${no_member}

exit 0
