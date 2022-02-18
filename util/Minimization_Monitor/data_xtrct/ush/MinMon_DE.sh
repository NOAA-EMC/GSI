#!/bin/bash

#  MinMon data extraction script

#--------------------------------------------------------------------
#  usage
#--------------------------------------------------------------------
function usage {
  echo "Usage:  MinMon_DE.sh suffix [-p|--pdate pdate -r|--run gdas|gfs]"
  echo "            Suffix is the indentifier for this data source."
  echo "            -p | --pdate yyyymmddcc to specify the cycle to be processed"
  echo "              if unspecified the last available date will be processed"
  echo "            -r | --run   the gdas|gfs run to be processed"
  echo "              use only if data in TANKdir stores both runs"
  echo "            -c | --comin the base directory to the gsistat files"
  echo "              This does not include any date-derived subdirectories."
  echo " "
}

#--------------------------------------------------------------------
#  MinMon_DE.sh begins here
#--------------------------------------------------------------------

set -x

nargs=$#
if [[ $nargs -lt 1 || $nargs -gt 7 ]]; then
   usage
   exit 1
fi


#-----------------------------------------------
#  Process command line arguments
#

while [[ $# -ge 1 ]]
do
   key="$1"
   echo $key

   case $key in
      -p|--pdate)
         export PDATE="$2"
         shift # past argument
      ;;
      -r|--run)
         RUN="$2"
         shift # past argument
      ;;
      -c|--comin)			# comin is base location of dir with gsistat files
         COMIN="$2"			
         shift # past argument 
      ;;
      *)
         #any unspecified key is MINMON_SUFFIX
         export MINMON_SUFFIX=$key
      ;;
   esac

   shift
done

#----------------------------
# set defaults for arguments
#
if [[ $RUN = "" ]]; then
   RUN=gdas
fi
export RUN=$RUN

if [[ $COMIN = "" ]]; then
   COMIN=/gpfs/dell1/nco/ops/com/gfs/prod
fi

echo MINMON_SUFFIX = $MINMON_SUFFIX
echo RUN           = $RUN
echo COMIN         = $COMIN


#-----------------------------------
#  source config and settings files 
#
this_dir=`dirname $0`
top_parm=${this_dir}/../../parm

minmon_config=${minmon_config:-${top_parm}/MinMon_config}
if [[ ! -e ${minmon_config} ]]; then
   echo "Unable to locate ${minmon_config} file"
   exit 3
fi

. ${minmon_config}
if [[ $? -ne 0 ]]; then
   echo "Error detected while sourcing ${minmon_config} file"
   exit $?
fi


minmon_user_settings=${minmon_user_settings:-${top_parm}/MinMon_user_settings}
if [[ ! -e ${minmon_user_settings} ]]; then
   echo "Unable to locate ${minmon_user_settings} file"
   exit 4
fi

. ${minmon_user_settings}
if [[ $? -ne 0 ]]; then
   echo "Error detected while sourcing ${minmon_user_settings} file"
   exit $?
fi


if [[ ${RUN} = "gdas" ]]; then
   export HOMEgfs=${HOMEgdas}
fi

NEWtank=${M_TANKverf}/stats/${MINMON_SUFFIX}
if [[ $GLB_AREA -eq 0 ]]; then
   NEWtank=${M_TANKverf}/stats/regional/${MINMON_SUFFIX}
fi

#---------------------------------------------------------
#  Determine next cycle
#    If PDATE wasn't supplied as an argument then call 
#    find_cycle.pl to determine the last processed cycle, 
#    and set PDATE to the next cycle
#
if [[ ${#PDATE} -le 0 ]]; then  
   echo "PDATE not specified:  setting PDATE using last cycle"
   date=`${M_DE_SCRIPTS}/find_cycle.pl --run $RUN --cyc 1 --dir ${NEWtank}`
   export PDATE=`$NDATE +6 $date`
else
   echo "PDATE was specified:  $PDATE"
fi

export PDY=`echo $PDATE|cut -c1-8`
export cyc=`echo $PDATE|cut -c9-10`

prev_cycle=`$NDATE -6 $PDATE`
p_pdy=`echo $prev_cycle|cut -c1-8`
p_cyc=`echo $prev_cycle|cut -c9-10`

#--------------------------------
#  expand M_TANKverf and define
#  M_TANKverfM1 (previous cycle)
#
export M_TANKverf=${NEWtank}/${RUN}.${PDY}/${cyc}/minmon
export M_TANKverfM1=${NEWtank}/${RUN}.${p_pdy}/${p_cyc}/minmon

#-----------------------------------------
# Expand COMIN with date specific subdirs
#
comin_base=${COMIN}
COMIN=${comin_base}/${RUN}.${PDY}/${cyc}/atmos
if [[ ! -d $COMIN ]]; then
   COMIN=${comin_base}/${RUN}.${PDY}/${cyc}
   if [[ ! -d $COMIN ]]; then
      COMIN=${comin_base}/${RUN}.${PDY}
   fi
fi
export COMIN=$COMIN 

export gsistat=${COMIN}/${RUN}.t${cyc}z.gsistat
if [[ ! -e $gsistat ]]; then
   echo "Unable to locate $gsistat, exiting MinMon_DE.sh"
   exit 5
fi


if [[ ! -d ${LOGdir} ]]; then
   mkdir -p ${LOGdir}
fi

lfile=${LOGdir}/DE.${PDY}.${cyc}
export logfile="${lfile}.log"
echo  "logfile = $logfile"
if [[ -e $logfile ]]; then
   rm -f $logfile
fi

export job=${job:-DE.${RUN}}
export DATAROOT=${DATA_IN:-${WORKDIR}}
export COMROOT=${COMROOT:-/com2}
export jobid=${jobid:-${job}.${PDY}.${pid}}

jobname=minmon_de_${MINMON_SUFFIX}

if [[ $RUN = "gfs" ]]; then
   jobfile=${jobfile:-${HOMEgfs}/jobs/JGFS_ATMOS_VMINMON}
else
   jobfile=${jobfile:-${HOMEgdas}/jobs/JGDAS_ATMOS_VMINMON}
fi

echo "MY_MACHINE = $MY_MACHINE"
echo "SUB        = $SUB"
echo "JOB_QUEUE  = $JOB_QUEUE"
echo "PROJECT    = $PROJECT"
echo "jobname    = $jobname" 
echo "jobfile    = $jobfile" 


if [[ $MY_MACHINE = "wcoss_d" ]]; then
   $SUB -P $PROJECT -q $JOB_QUEUE -o ${logfile} -M 50 \
        -R affinity[core] -W 0:10 -J ${jobname} $jobfile

elif [[ $MY_MACHINE = "cray" ]]; then
   $SUB -q $JOB_QUEUE -P $PROJECT -o ${logfile} -M 80 \
        -R "select[mem>80] rusage[mem=80]" -W 0:10 -J ${jobname} $jobfile

elif [[ $MY_MACHINE = "hera" ]]; then
   $SUB --account=${ACCOUNT} --time=05 -J ${job} -D . \
        -o ${logfile} \
        --ntasks=1 --mem=5g \
        ${jobfile}

elif [[ $MY_MACHINE = "wcoss2" ]]; then
   $SUB -e ${logfile} -N gdas_vminmon -q ${JOB_QUEUE} -V \
        -l select=1:mem=400M -l walltime=05:00 -A GFS-DEV \
        ${jobfile}
fi


echo "end MinMon_DE.sh"
