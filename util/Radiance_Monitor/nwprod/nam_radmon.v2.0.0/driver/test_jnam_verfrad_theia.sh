#!/bin/ksh

#PBS -N nam_verfrad
#PBS -l procs=1,walltime=00:20
#PBS -o nam_verfrad.o${JOB_ID}
#PBS -e nam_verfrad.o${JOB_ID}
#PBS -m be
#PBS -A glbss 

set -x

export PDATE=2016062709
export NDATE=/home/Edward.Safford/bin/ndate

#############################################################
# Specify whether the run is production or development
#############################################################
export PDY=`echo $PDATE | cut -c1-8`
export cyc=`echo $PDATE | cut -c9-10`
export job=nam_verfrad.${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=para
export UTILROOT=/scratch4/NCEPDEV/da/save/Michael.Lueken/nwprod/util
export DATAROOT=/scratch4/NCEPDEV/da/noscrub/Edward.Safford/test_data
export COMROOT=/scratch4/NCEPDEV/da/noscrub/Edward.Safford/com


#############################################################
# Specify versions
#############################################################
#export gdas_ver=v13.0.0
export global_shared_ver=v13.0.0
export grib_util_ver=v1.0.1
export prod_util_ver=v1.0.2
export util_shared_ver=v1.0.2
export nam_radmon_ver=v2.0.0
export radmon_shared_ver=v2.0.4


#############################################################
# Load modules
#############################################################
#. /usrx/local/Modules/3.2.9/init/ksh

rm -f ${DATAROOT}/startmsg
rm -f ${DATAROOT}/prep_step
rm -f ${DATAROOT}/postmsg
rm -f ${DATAROOT}/setup
rm -f ${DATAROOT}/err_chk

ln -s ${UTILROOT}/ush/startmsg.sh ${DATAROOT}/startmsg
ln -s ${UTILROOT}/ush/prep_step.sh ${DATAROOT}/prep_step
ln -s ${UTILROOT}/ush/postmsg.sh ${DATAROOT}/postmsg
ln -s ${UTILROOT}/ush/setup.sh ${DATAROOT}/setup
ln -s ${UTILROOT}/ush/err_chk.sh ${DATAROOT}/err_chk

export PATH=${PATH}:${DATAROOT}

echo "PATH is defined as:"
echo $PATH

which startmsg
which prep_step
which postmsg
which setup
which err_chk

#module use /nwprod2/modulefiles
#module load grib_util/$grib_util_ver
#module load prod_util/$prod_util_ver
#module load util_shared/$util_shared_ver

#module unload ics/12.1
#module load ics/15.0.3

#module list


#############################################################
# WCOSS environment settings
#############################################################
#export POE=YES


#############################################################
# Set user specific variables
#############################################################
export RADMON_SUFFIX=testrad
export NWTEST=/scratch4/NCEPDEV/da/noscrub/Edward.Safford/RadMon_595/util/Radiance_Monitor/nwprod
export HOMEnam=${NWTEST}/nam_radmon.${nam_radmon_ver}
export JOBregional=${HOMEnam}/jobs
export HOMEradmon=${NWTEST}/radmon_shared.${radmon_shared_ver}
export COM_IN=${DATAROOT}
export TANKverf=${COMROOT}/${RADMON_SUFFIX}


#############################################################
# Execute job
#############################################################
ACCOUNT=glbss
jobname=nam_verfrad
output=./nam_verfrad.out
SUB=qsub

$SUB -A $ACCOUNT -l procs=1,walltime=0:10:00 -N ${jobname} -V -o ${output} -e ${output} $JOBregional/JNAM_VERFRAD

exit

