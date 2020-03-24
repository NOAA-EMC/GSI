#!/bin/ksh

#SBATCH -o gdas_verfozn.o%j
#SBATCH -J gdas_verfozn
#SBATCH --ntasks=1 --mem=5g
#SBATCH --time=10
#SBATCH --account=fv3-cpu
#SBATCH -D .

set -x

export OZNMON_NEW_HDR=${OZN_NEW_HDR:-0}

#export PDATE=${PDATE:-2019083100}	# netcdf
export PDATE=${PDATE:-2018091706}	# bin
export NET=${NET:-gfs}
export RUN=${RUN:-gdas}

export PDY=`echo $PDATE | cut -c1-8`
export cyc=`echo $PDATE | cut -c9-10`
export job=${job:-gdas_verfozn.${cyc}}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=${envir:-test}
export DATAROOT=${DATAROOT:-/scratch1/NCEPDEV/da/Edward.Safford/noscrub/test_data}
export COMROOT=${COMROOT:-/scratch2/NCEPDEV/stmp3/${LOGNAME}/com}
export OZN_WORK_DIR=${OZN_WORK_DIR:-/scratch2/NCEPDEV/stmp3/${LOGNAME}/oznmon.${pid}}

#------------------------------------------------------------
# Specify versions
#
export gdas_oznmon_ver=v2.0.0
export shared_oznmon_ver=v2.0.0


#------------------------------------------------------------
# Add nwpara tools to path
#
#NWPROD=${NWPROD:-/scratch4/NCEPDEV/global/save/glopara/nwpara/util}
#NWPRODush=${NWPRODush:=${NWPROD}/ush}
#NWPRODexec=${NWPRODexec:=${NWPROD}/exec}
#export PATH=${PATH}:${NWPRODush}:${NWPRODexec}

#------------------------------------------------------------
# Set user specific variables
#

export OZNMON_SUFFIX=${OZNMON_SUFFIX:-testozn}
export NWTEST=${NWTEST:-/scratch1/NCEPDEV/da/${LOGNAME}/noscrub/ProdGSI/util/Ozone_Monitor/nwprod}
export HOMEgdas_ozn=${HOMEgdas_ozn:-${NWTEST}/gdas_oznmon.${gdas_oznmon_ver}}
export PARMgdas_ozn=${PARMgdas_ozn:-${HOMEgdas_ozn}/parm}
export FIXgdas_ozn=${FIXgdas_ozn:-${HOMEgdas_ozn}/fix}

export HOMEgfs_ozn=${HOMEgfs_ozn:-${HOMEgdas_ozn}}
export PARMgfs_ozn=${PARMgfs_ozn:-${PARMgdas_ozn}}
export FIXgfs_ozn=${FIXgfs_ozn:-${FIXgdas_ozn}}

export JOBGLOBAL=${JOBGLOBAL:-${HOMEgdas_ozn}/jobs}
export HOMEoznmon=${HOMEoznmon:-${NWTEST}/oznmon_shared.${shared_oznmon_ver}}
export COM_IN=${COM_IN:-${DATAROOT}}
export OZN_TANKDIR=${OZN_TANKDIR:-${COMROOT}/${OZNMON_SUFFIX}}

export SUB=${SUB:-/apps/slurm/default/bin/sbatch}
export NDATE=${NDATE:-/home/Edward.Safford/bin/ndate}



#------------------------------------------------------------
#  theia specific hacks for no prod_utils module & no setpdy.sh script
#
export MY_MACHINE=hera
prevday=`$NDATE -24 $PDATE`
export PDYm1=`echo $prevday | cut -c1-8`
#ln -s ${NWPRODush}/startmsg.sh ${COMROOT}/startmsg
#ln -s ${NWPRODush}/postmsg.sh ${COMROOT}/postmsg
#ln -s ${NWPRODush}/prep_step.sh ${COMROOT}/prep_step
#ln -s ${NWPRODush}/err_chk.sh ${COMROOT}/err_chk
export PATH=$PATH:${COMROOT}
#export utilscript=${utilscript:-${NWPRODush}}		# err_chk calls postmsg.sh
							#   directly so need to override
							#   utilscript location for theia
#------------------------------------------------------------
# Execute job
#
$JOBGLOBAL/JGDAS_VERFOZN

exit

