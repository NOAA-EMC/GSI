#!/bin/sh
set -ax

#
# confirm_data.sh
#
# Given a suffix and an $IMGDATE, determine if data for the next cycle
# is available and complete for this source.  Return (echo) either YES
# or NO.
#

function usage {
  echo "Usage:  confirm_data.sh suffix imgdate[yyyymmddcc]"
  echo "            Suffix is data source identifier that matches data in "
  echo "              the $TANKDIR/stats directory."
  echo "            The imgdate is the intended plotting data."
}


nargs=$#
if [[ $nargs -ne 2 ]]; then
   usage
   exit 1
fi


suffix=$1
pdate=$2
CYA=`echo $pdate|cut -c9-10`
PDY=`echo $pdate|cut -c1-8`

angle_cnt=`ls -la ${TANKDIR}/radmon.${PDY}/angle.*${pdate}.ieee_d* | wc -l`
bcoef_cnt=`ls -la ${TANKDIR}/radmon.${PDY}/bcoef.*${pdate}.ieee_d* | wc -l`
bcor_cnt=`ls -la ${TANKDIR}/radmon.${PDY}/bcor.*${pdate}.ieee_d* | wc -l`
time_cnt=`ls -la ${TANKDIR}/radmon.${PDY}/time.*${pdate}.ieee_d* | wc -l`

###########################################
# if all cnts -gt 0 and equal then proceed
###########################################
proceed="NO"

if [[ $angle_cnt -gt 0 ]]; then
   if [[ $angle_cnt -eq bcoef_cnt && $angle_cnt -eq $bcor_cnt && $angle_cnt -eq $time_cnt ]]; then
      proceed="YES"
   fi
fi

echo $proceed
exit
