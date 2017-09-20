#!/bin/sh
# -------------------------------------
# Wrapper script for efso cleanup
# for a provided time period
# -------------------------------------
set -ux
ndate=/scratch4/NCEPDEV/da/save/Michael.Lueken/nwprod/util/exec/ndate
basedir=/scratch4/NCEPDEV/da/noscrub/David.Groff/prnthdjf_fcst
#basedir=/scratch4/NCEPDEV/da/noscrub/David.Groff/test_efsofcst
# range of dates based on arguments
bdate=$1
edate=$2
cdate=$bdate

# Experiment name assignment
#ename=$3

while [[ $cdate -le $edate ]]; do
  if [ ! -d $basedir/$cdate ] ; then
    adate=`$ndate +6 $cdate`
    cdate=$adate
    continue
  fi

  # clean up that is remove efso input files
  ./efso_cleanup.sh ${cdate}
  while [ ! -f complete_cleanup_${cdate} ]; do
    sleep 5
  done
 
  adate=`$ndate +6 $cdate`
  cdate=$adate
done
