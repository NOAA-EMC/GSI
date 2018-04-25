#!/bin/ksh

#PBS -o gdas_verfozn.log
#PBS -e gdas_verfozn.err
#PBS -N gdas_verfozn
#PBS -A fv3-cpu
#PBS -l procs=1,walltime=0:10:00
#PBS -V

set -x

export PDATE=${PDATE:-2017072600}
export NET=${NET:-gfs}
export RUN=${RUN:-gdas}

#------------------------------------------------------------
# Specify whether the run is production or development
#
export PDY=`echo $PDATE | cut -c1-8`
export cyc=`echo $PDATE | cut -c9-10`
export job=${job:-gdas_verfozn.${cyc}}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=${envir:-test}
export DATAROOT=${DATAROOT:-/scratch4/NCEPDEV/da/noscrub/Edward.Safford/test_data}
export COMROOT=${COMROOT:-/scratch4/NCEPDEV/stmp3/${LOGNAME}/com}
export OZN_WORK_DIR=${OZN_WORK_DIR:-/scratch4/NCEPDEV/stmp3/${LOGNAME}/oznmon.${pid}}

#------------------------------------------------------------
# Specify versions
#
export gdas_oznmon_ver=v2.0.0
export shared_oznmon_ver=v2.0.0


#------------------------------------------------------------
# Add nwpara tools to path
#
NWPROD=${NWPROD:-/scratch4/NCEPDEV/global/save/glopara/nwpara/util}
NWPRODush=${NWPRODush:=${NWPROD}/ush}
NWPRODexec=${NWPRODexec:=${NWPROD}/exec}
export PATH=${PATH}:${NWPRODush}:${NWPRODexec}

#------------------------------------------------------------
# Set user specific variables
#

export OZNMON_SUFFIX=${OZNMON_SUFFIX:-testozn}
export NWTEST=${NWTEST:-/scratch4/NCEPDEV/da/noscrub/${LOGNAME}/comgsi/util/Ozone_Monitor/nwprod}
export HOMEgdas_ozn=${HOMEgdas_ozn:-${NWTEST}/gdas_oznmon.${gdas_oznmon_ver}}
export JOBGLOBAL=${JOBGLOBAL:-${HOMEgdas_ozn}/jobs}
export HOMEoznmon=${HOMEoznmon:-${NWTEST}/oznmon_shared.${shared_oznmon_ver}}
export COM_IN=${COM_IN:-${DATAROOT}}
export OZN_TANKDIR=${OZN_TANKDIR:-${COMROOT}/${OZNMON_SUFFIX}}

export SUB=${SUB:-/apps/torque/default/bin/qsub}
export NDATE=${NDATE:-ndate}



#------------------------------------------------------------
#  theia specific hacks for no prod_utils module & no setpdy.sh script
#
export MY_MACHINE=theia
prevday=`$NDATE -24 $PDATE`
export PDYm1=`echo $prevday | cut -c1-8`
ln -s ${NWPRODush}/startmsg.sh ${COMROOT}/startmsg
ln -s ${NWPRODush}/postmsg.sh ${COMROOT}/postmsg
ln -s ${NWPRODush}/prep_step.sh ${COMROOT}/prep_step
ln -s ${NWPRODush}/err_chk.sh ${COMROOT}/err_chk
export PATH=$PATH:${COMROOT}
export utilscript=${utilscript:-${NWPRODush}}		# err_chk calls postmsg.sh
							#   directly so need to override
							#   utilscript location for theia
#------------------------------------------------------------
# Execute job
#
$JOBGLOBAL/JGDAS_VERFOZN

exit

