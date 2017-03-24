#!/bin/ksh

#PBS -o gdas_vcmon.log
#PBS -e gdas_vcmon.err
#PBS -N gdas_vcmon
#PBS -A glbss
#PBS -l procs=1,walltime=0:15:00
#PBS -V

set -x

export PDATE=${PDATE:-2017030600}

#############################################################
# Specify whether the run is production or development
#############################################################
export PDY=`echo $PDATE | cut -c1-8`
export CYC=`echo $PDATE | cut -c9-10`
export job=gfs_vcmon.${CYC}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=para
export DATAROOT=${DATAROOT:-/scratch4/NCEPDEV/da/noscrub/Edward.Safford/test_data}
export COMROOT=${COMROOT:-/scratch4/NCEPDEV/stmp3/$LOGNAME/com}


#############################################################
# Specify versions
#############################################################
export gdas_ver=v14.1.0
export global_shared_ver=v14.1.0
export gdas_cmon_ver=v1.0.0
export cmon_shared_ver=v1.0.0


#############################################################
# Add nwpara tools to path
#############################################################
export NWPROD=${NWPROD:-/scratch4/NCEPDEV/global/save/glopara/nwpara/util}
NWPRODush=${NWPRODush:=${NWPROD}/ush}
NWPRODexec=${NWPRODexec:=${NWPROD}/exec}
export PATH=${PATH}:${NWPRODush}:${NWPRODexec}


#############################################################
# Set user specific variables
#############################################################
export CMON_SUFFIX=${CMON_SUFFIX:-testcmon}
export NWTEST=${NWTEST:-/scratch4/NCEPDEV/da/noscrub/${LOGNAME}/CMon_486/util/Conventional_Monitor/nwprod}
export HOMEgdascmon=${HOMEgdascmon:-${NWTEST}/gdas_cmon.${gdas_cmon_ver}}
export JOBGLOBAL=${JOBGLOBAL:-${HOMEgdascmon}/jobs}
export HOMEcmon=${HOMEcmon:-${NWTEST}/cmon_shared.${cmon_shared_ver}}
export COM_IN=${COM_IN:-${DATAROOT}}

export C_TANKDIR=${C_TANKDIR:-${COMROOT}/${CMON_SUFFIX}}
if [[ ! -d $C_TANKDIR ]]; then
   mkdir -p ${C_TANKDIR}
fi

export CMON_WORK_DIR=${CMON_WORK_DIR:-/scratch4/NCEPDEV/stmp3/$LOGNAME/cmon_work}
if [[ ! -d $CMON_WORK_DIR ]]; then
   mkdir -p ${CMON_WORK_DIR}
fi

export CMON_LOG_DIR=${CMON_LOG_DIR:-${COMROOT}/logs/jlogfiles}
if [[ ! -d $CMON_LOG_DIR ]]; then
   mkdir -p ${CMON_LOG_DIR}
fi

export jlogfile=${jlogfile:-${CMON_LOG_DIR}/jlogfile.${jobname}.${pid}}


#######################################################################
#  theia specific hacks for no prod_utils module & no setpdy.sh script
#######################################################################
export NDATE=${NDATE:-/scratch4/NCEPDEV/global/save/glopara/nwpara/util/exec/ndate}
export MY_MACHINE=theia
prevday=`$NDATE -24 $PDATE`
export PDYm1=`echo $prevday | cut -c1-8`
ln -s ${NWPRODush}/startmsg.sh ${COMROOT}/startmsg
ln -s ${NWPRODush}/postmsg.sh ${COMROOT}/postmsg
ln -s ${NWPRODush}/prep_step.sh ${COMROOT}/prep_step
ln -s ${NWPRODush}/err_chk.sh ${COMROOT}/err_chk
export PATH=$PATH:${COMROOT}
export utilscript=${utilscript:-${NWPRODush}}      # err_chk calls postmsg.sh
                                                   #  directly so need to override
                                                   #  utilscript location

#############################################################
# Execute job
#############################################################
$JOBGLOBAL/JGDAS_VCMON

exit

