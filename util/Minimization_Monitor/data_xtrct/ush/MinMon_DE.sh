#!/bin/sh

#  Gmon data extraction script

#--------------------------------------------------------------------
#  usage
#--------------------------------------------------------------------
function usage {
  echo "Usage:  MinMonDE.sh suffix [pdate]"
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
#  RUN_ENVIR:
#    if $COMOUT is defined then assume we're in a parallel, else
#    it's dev.
#--------------------------------------------------------------------
export SUFFIX=$1
export RUN_ENVIR=""

if [[ $nargs -ge 2 ]]; then
   export PDATE=$2;
   echo "PDATE set to $PDATE"
fi

if [[ $COMOUT = "" ]]; then
  export RUN_ENVIR="dev"
else 
  export RUN_ENVIR="para"
fi

echo SUFFIX = $SUFFIX
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

###################################
#  expand TANKverf for this SUFFIX
###################################
NEWtank=${TANKverf}/stats/${SUFFIX}/gsistat
export TANKverf=$NEWtank
echo "TANKverf = $TANKverf"


##############################################################
#  Determine next cycle
#    If PDATE wasn't an argument then pull the last processed 
#    date from the latest $SUFFIX_minmon.gnorm_data.txt file 
#    and advance one cycle.
##############################################################
if [[ ${#PDATE} -le 0 ]]; then  
   lastdir=`ls -1d ${TANKverf}/${SUFFIX}_minmon.* | tail -1`
   lastln=`cat $lastdir/${SUFFIX}.gnorm_data.txt | tail -1`
   date=`echo $lastln | gawk '{split($0,a,","); print a[1] a[2] a[3] a[4]}'`
   export PDATE=`$NDATE +6 $date`
fi

export PDY=`echo $PDATE|cut -c1-8`
export cyc=`echo $PDATE|cut -c9-10`
echo "PDY, cyc = $PDY, $cyc "

export jlogfile="${jlogfile}${SUFFIX}.${PDY}.${cyc}.log"
echo  "jlogfile = $jlogfile"
export jobname=minmon_de_${SUFFIX}

rm -f $jlogfile
rm -rf $DATA_IN

echo "MY_MACHINE = $MY_MACHINE"
echo "SUB        = $SUB"
echo "JOB_QUEUE  = $JOB_QUEUE"
echo "PROJECT    = $PROJECT"
echo "jobname    = $jobname" 

if [[ $MY_MACHINE = "wcoss" ]]; then
      $SUB -q $JOB_QUEUE -P $PROJECT -o ${jlogfile} -M 50 -R affinity[core] -W 0:10 -J ${jobname} $HOMEgdasgmon/jobs/JGDAS_VMINMON
fi


