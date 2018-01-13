#!/bin/ksh

#PBS -o nam_verfrad.log
#PBS -e nam_verfrad.err
#PBS -N nam_verfrad
#PBS -A glbss 
#PBS -l procs=1,walltime=05:00
#PBS -V

set -x

export PDATE=${PDATE:-2017020606}

#############################################################
# Specify whether the run is production or development
#############################################################
export PDY=`echo $PDATE | cut -c1-8`
export cyc=`echo $PDATE | cut -c9-10`
export job=nam_verfrad.${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=para
export DATAROOT=${DATAROOT:-/scratch4/NCEPDEV/da/noscrub/Edward.Safford/test_data}
export COMROOT=${COMROOT:-/scratch4/NCEPDEV/da/noscrub/Edward.Safford/com}


#############################################################
# Specify versions
#############################################################
export global_shared_ver=v14.1.0
export nam_radmon_ver=v2.0.0
export radmon_shared_ver=v2.0.4


#############################################################
# Add nwpara tools to path
#############################################################
NWPROD=${NWPROD:-/scratch4/NCEPDEV/global/save/glopara/nwpara/util}
NWPRODush=${NWPRODush:=${NWPROD}/ush}
NWPRODexec=${NWPRODexec:=${NWPROD}/exec}
export PATH=${PATH}:${NWPRODush}:${NWPRODexec}

#############################################################
# Set user specific variables
#############################################################
export RADMON_SUFFIX=${RADMON_SUFFIX:-testrad}
export NWTEST=${NWTEST:-/scratch4/NCEPDEV/da/noscrub/Edward.Safford/RadMon_622/util/Radiance_Monitor/nwprod}
export HOMEnam=${HOMEnam:-${NWTEST}/nam_radmon.${nam_radmon_ver}}
export JOBregional=${JOBregional:-${HOMEnam}/jobs}
export HOMEradmon=${HOMEradmon:-${NWTEST}/radmon_shared.${radmon_shared_ver}}
export COM_IN=${COM_IN:-${DATAROOT}}
export TANKverf=${TANKverf:-${COMROOT}/${RADMON_SUFFIX}}

export SUB=${SUB:-/apps/torque/default/bin/qsub}
export NDATE=${NDATE:-ndate}

#######################################################################
#  theia specific hacks for no prod_utils module & no setpdy.sh script
#######################################################################
export MY_MACHINE=theia
prevday=`$NDATE -24 $PDATE`
export PDYm1=`echo $prevday | cut -c1-8`
ln -s ${NWPRODush}/startmsg.sh ${COMROOT}/startmsg
ln -s ${NWPRODush}/postmsg.sh ${COMROOT}/postmsg
ln -s ${NWPRODush}/prep_step.sh ${COMROOT}/prep_step
ln -s ${NWPRODush}/err_chk.sh ${COMROOT}/err_chk
export PATH=$PATH:${COMROOT}
export utilscript=${utilscript:-${NWPRODush}}           # err_chk calls postmsg.sh
                                                        #   directly so need to override
                                                        #   utilscript location for theia


#############################################################
# to execute job on theia
# enter this command:
#    qsub < test_jnam_verfrad_theia.sh 
#
#############################################################


$JOBregional/JNAM_VERFRAD

exit

