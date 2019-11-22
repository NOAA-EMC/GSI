#!/bin/ksh

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
  echo " "
}

#--------------------------------------------------------------------
#  MinMon_DE.sh begins here
#--------------------------------------------------------------------

set -x

nargs=$#
if [[ $nargs -lt 1 || $nargs -gt 5 ]]; then
   usage
   exit 1
fi


#-----------------------------------------------
#  Process command line arguments
#
RUN=gdas

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
      *)
         #any unspecified key is MINMON_SUFFIX
         export MINMON_SUFFIX=$key
      ;;
   esac

   shift
done

export RUN=$RUN

this_dir=`dirname $0`


if [[ $COMOUT = "" ]]; then
  export RUN_ENVIR="dev"
else 
  export RUN_ENVIR="para"
fi

echo MINMON_SUFFIX = $MINMON_SUFFIX
echo RUN_ENVIR = $RUN_ENVIR

top_parm=${this_dir}/../../parm

minmon_version_file=${minmon_version:-${top_parm}/MinMon.ver}
if [[ -s ${minmon_version_file} ]]; then
   . ${minmon_version_file}
   echo "able to source ${minmon_version_file}"
else
   echo "Unable to source ${minmon_version_file} file"
   exit 2
fi

echo "MINMON_CONFIG = $MINMON_CONFIG"
minmon_config=${minmon_config:-${top_parm}/MinMon_config}
if [[ -s ${minmon_config} ]]; then
   . ${minmon_config}
   echo "able to source ${minmon_config}"
else
   echo "Unable to source ${minmon_config} file"
   exit 3
fi


minmon_user_settings=${minmon_user_settings:-${top_parm}/MinMon_user_settings}
if [[ -s ${minmon_user_settings} ]]; then
   . ${minmon_user_settings}
   echo "able to source ${minmon_user_settings}"
else
   echo "Unable to source ${minmon_user_settings} file"
   exit 4
fi


#--------------------------------------------------------------------
#  Check setting of RUN_ONLY_ON_DEV and possible abort if on prod and
#  not permitted to run there.
#--------------------------------------------------------------------

if [[ RUN_ONLY_ON_DEV -eq 1 ]]; then
   is_prod=`${M_DE_SCRIPTS}/onprod.sh`
   if [[ $is_prod = 1 ]]; then
      exit 10
   fi
fi


if [[ ${RUN} = "gdas" ]]; then
   export HOMEgfs=${HOMEgdas}
fi

##########################################
#  expand M_TANKverf for this MINMON_SUFFIX
##########################################
NEWtank=${M_TANKverf}/stats/${MINMON_SUFFIX}
if [[ $GLB_AREA -eq 0 ]]; then
   NEWtank=${M_TANKverf}/stats/regional/${MINMON_SUFFIX}
fi

export M_TANKverf=$NEWtank
echo "M_TANKverf = $M_TANKverf"


##############################################################
#  Determine next cycle
#    If PDATE wasn't an argument then call find_cycle.pl
#    to determine the last processed cycle, and set PDATE to
#    the next cycle
#
#  NOTE:  Need to make gdas the default value to $run
##############################################################
if [[ ${#PDATE} -le 0 ]]; then  
   echo "PDATE not specified:  setting PDATE using last cycle"
   date=`${M_DE_SCRIPTS}/find_cycle.pl --run gdas --cyc 1 --dir ${M_TANKverf}`
   export PDATE=`$NDATE +6 $date`
else
   echo "PDATE was specified:  $PDATE"
fi

export PDY=`echo $PDATE|cut -c1-8`
export cyc=`echo $PDATE|cut -c9-10`
echo "PDY, cyc = $PDY, $cyc "


if [[ ! -d ${LOGdir} ]]; then
   mkdir -p ${LOGdir}
fi

lfile=${LOGdir}/DE.${PDY}.${cyc}
export pid=${pid:-$$}
export jlogfile=${lfile}.o${pid}
export m_jlogfile="${lfile}.log"
echo  "m_jlogfile = $m_jlogfile"

#############################################################

export job=${job:-DE.${RUN}}
export jobid=${jobid:-${job}.${PDY}.${pid}}
export envir=prod
export DATAROOT=${DATA_IN:-${WORKDIR}}
export COMROOT=${COMROOT:-/com2}

echo "MY_MACHINE = $MY_MACHINE"


jobname=minmon_de_${MINMON_SUFFIX}

rm -f $m_jlogfile

echo "SUB        = $SUB"
echo "JOB_QUEUE  = $JOB_QUEUE"
echo "PROJECT    = $PROJECT"
echo "jobname    = $jobname" 

if [[ $GLB_AREA -eq 0 ]]; then
   jobfile=${jobfile:-${HOMEnam}/jobs/JNAM_VMINMON}
else
   if [[ $RUN = "gfs" ]]; then
      jobfile=${jobfile:-${HOMEgfs}/jobs/JGFS_VMINMON}
   else
      jobfile=${jobfile:-${HOMEgdas}/jobs/JGDAS_VMINMON}
   fi
fi

if [[ $MY_MACHINE = "wcoss" || $MY_MACHINE = "wcoss_d" ]]; then
   $SUB -P $PROJECT -q $JOB_QUEUE -o ${m_jlogfile} -M 50 -R affinity[core] -W 0:10 -J ${jobname} $jobfile

elif [[ $MY_MACHINE = "cray" ]]; then
   $SUB -q $JOB_QUEUE -P $PROJECT -o ${m_jlogfile} -M 80 -R "select[mem>80] rusage[mem=80]" -W 0:10 -J ${jobname} $jobfile

elif [[ $MY_MACHINE = "hera" ]]; then
   $SUB --account=${ACCOUNT} --time=05 -J ${job} -D . \
        -o ${LOGdir}/DE.${PDY}.${cyc}.log \
        --ntasks=1 --mem=5g \
        ${jobfile}

fi


echo "end MinMon_DE.sh"
