#!/bin/ksh

#----------------------------------------------------------------------
#
#  This is a generic data extraction leader script.
#
#  The MinMon_DE.sh script may be called directly, or this script,
#    which calls MinMon_DE.sh can be used to overwrite specific
#    values in MinMon_DE.sh and/or parm files.
#
#  Set MINMON_SUFFIX, RUN to match your data source and run 
#    value.
#
#  COMIN is the location of the gsistat file repository, which
#    contains the specific $RUN.$PDATE subdirectories.
#
#  Use pdate to run a specific cycle.  If commented out then the
#    M_TANKverf directory will be examined and the pdate will
#    be the last cycle run + 6 hrs. 
#    
#----------------------------------------------------------------------

MINMON_SUFFIX=testmm
RUN=gfs
COMIN=/scratch1/NCEPDEV/da/Edward.Safford/noscrub/test_data

#pdate=2016030700


#----------------------------------------------------------------------
#  No changes should be necessary below this point.
#----------------------------------------------------------------------

. ../../parm/MinMon.ver
. ../../parm/MinMon_config

if [[ $RUN == "gfs" ]]; then
   export jobfile=${HOMEgfs}/jobs/JGFS_VMINMON
else
   export jobfile=${HOMEgdas}/jobs/JGDAS_VMINMON
fi

echo NDATE = $NDATE

echo "LOGdir = ${LOGdir}"
if [[ ! -e ${LOGdir} ]]; then
   mkdir -p ${LOGdir}
fi

if [[ $pdate == "" ]]; then
   ldate=`${M_DE_SCRIPTS}/find_cycle.pl --dir ${M_TANKverf}/stats/${MINMON_SUFFIX} --cyc 1 --run ${RUN}`
   pdate=`${NDATE} +06 $ldate`
fi

pdy=`echo $pdate|cut -c1-8`
cyc=`echo $pdate|cut -c9-10`

export gsistat=${COMIN}/${RUN}.${pdy}/${cyc}/${RUN}.t${cyc}z.gsistat

if [[ ! -e $gsistat ]]; then
   echo " unable to locate $gsistat"
fi

echo pdate   = $pdate
echo gsistat = $gsistat


${M_DE_SCRIPTS}/MinMon_DE.sh ${MINMON_SUFFIX} -p ${pdate} -r ${RUN} \
	1>$LOGdir/MinMon_DE.log 2>$LOGdir/MinMon_DE.err


