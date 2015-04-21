#!/bin/sh

#  Gmon data extraction script

#--------------------------------------------------------------------
#  usage
#--------------------------------------------------------------------
function usage {
  echo "Usage:  VrfyRad_glbl.sh suffix [pdate]"
  echo "            Suffix is the indentifier for this data source."
  echo "            Pdate is the full YYYYMMDDHH cycle to run.  This param is optional"
}

#--------------------------------------------------------------------
#  GmonDE.sh begins here
#--------------------------------------------------------------------
nargs=$#
if [[ $nargs -lt 1 || $nargs -gt 2 ]]; then
   usage
   exit 1
fi

this_file=`basename $0`
this_dir=`dirname $0`

#--------------------------------------------------------------------
#  Eventually remove RUN_ENVIR argument but allow for it to possibly be
#  present as $2 to ensure backward compatibility.
#  
#  if $COMOUT is defined then assume we're in a parallel.
#--------------------------------------------------------------------
export SUFFIX=$1
export RUN_ENVIR=""

if [[ $nargs -ge 2 ]]; then
   if [[ $2 = "dev" || $2 = "para" ]]; then
      export RUN_ENVIR=$2;
   else
      export PDATE=$2;
      echo "PDATE set to $PDATE"
   fi

   if [[ $nargs -eq 3 ]]; then
      export PDATE=$3;
      echo "PDATE set to $PDATE"
   fi
fi

if [[ $RUN_ENVIR = "" ]]; then
  export RUN_ENVIR="para"
  if [[ $COMOUT = "" ]]; then
     export RUN_ENVIR="dev"
  fi
fi

echo SUFFIX = $SUFFIX
echo RUN_ENVIR = $RUN_ENVIR

top_parm=${this_dir}/../../parm

gmon_version_file=${gmon_version:-${top_parm}/gmon.ver}
if [[ -s ${gmon_version_file} ]]; then
   . ${gmon_version_file}
   echo "able to source ${gmon_version_file}"
else
   echo "Unable to source ${gmon_version_file} file"
   exit 2
fi

gmon_config=${gmon_config:-${top_parm}/gmon_config}
if [[ -s ${gmon_config} ]]; then
   . ${gmon_config}
   echo "able to source ${gmon_config}"
else
   echo "Unable to source ${gmon_config} file"
   exit 3
fi


gmon_user_settings=${gmon_user_settings:-${top_parm}/gmon_user_settings}
if [[ -s ${gmon_user_settings} ]]; then
   . ${gmon_user_settings}
   echo "able to source ${gmon_user_settings}"
else
   echo "Unable to source ${gmon_user_settings} file"
   exit 3
fi

################################
#  Need to determine next cycle
################################

export PDY=20150416
#export cycle=00
#export CYCLE=00
export cyc=00


###################################
#  expand TANKverf for this SUFFIX
###################################
NEWtank=${TANKverf}/stats/${SUFFIX}/gsistat
export TANKverf=$NEWtank
echo "TANKverf = $TANKverf"

#export envir=prod
export jlogfile=${LOGdir}/gmon.DE.${SUFFIX}.${PDY}.${cyc}.log
echo "jlogfile = $jlogfile"
export jobname=gmon_de_${SUFFIX}

rm -f $jlogfile
rm -rf $DATA_IN

echo "MY_MACHINE = $MY_MACHINE"
echo "SUB        = $SUB"
echo "JOB_QUEUE  = $JOB_QUEUE"
echo "PROJECT    = $PROJECT"
echo "jobname    = $jobname" 

if [[ $MY_MACHINE = "wcoss" ]]; then
      $SUB -q $JOB_QUEUE -P $PROJECT -o ${jlogfile} -M 50 -R affinity[core] -W 0:10 -J ${jobname} $HOMEgdasgmon/jobs/JGDAS_VERFGNM
fi


