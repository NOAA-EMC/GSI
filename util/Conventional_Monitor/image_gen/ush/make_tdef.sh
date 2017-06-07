#!/bin/sh

#------------------
#  make_tdef.sh
#------------------
PDATE=$1
NUM_CYCLES=$2
HR_INTERVAL=$3

yr=`echo ${PDATE} | cut -c1-4`
mo=`echo ${PDATE} | cut -c5-6`
da=`echo ${PDATE} | cut -c7-8`
hr=`echo ${PDATE} | cut -c9-10`

case $mo in
   01) month=jan;;
   02) month=feb;;
   03) month=mar;;
   04) month=apr;;
   05) month=may;;
   06) month=jun;;
   07) month=jul;;
   08) month=aug;;
   09) month=sep;;
   10) month=oct;;
   11) month=nov;;
   12) month=dec;;
   *) echo "month error $mo"
      exit 1;;
esac

tdef=" tdef ${NUM_CYCLES} linear ${hr}z${da}${month}${yr} ${HR_INTERVAL}hr"

echo ${tdef}
exit
