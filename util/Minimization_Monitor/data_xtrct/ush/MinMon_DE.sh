#!/bin/sh

#  MinMon data extraction script

#--------------------------------------------------------------------
#  usage
#--------------------------------------------------------------------
function usage {
  echo "Usage:  MinMonDE.sh suffix run_envir [pdate]"
  echo "            Suffix is the indentifier for this data source."
  echo "            run_envir is either 'dev' or 'para'."
  echo "            Pdate is the full YYYYMMDDHH cycle to run.  This param is optional"
}

#--------------------------------------------------------------------
#  MinMon_DE.sh begins here
#--------------------------------------------------------------------
nargs=$#
if [[ $nargs -lt 2 || $nargs -gt 3 ]]; then
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
export MINMON_SUFFIX=$1
export RUN_ENVIR=$2

if [[ $nargs -ge 2 ]]; then
   export PDATE=$3;
   echo "PDATE set to $PDATE"
fi

#if [[ $COMOUT = "" ]]; then
#  export RUN_ENVIR="dev"
#else 
#  export RUN_ENVIR="para"
#fi

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
#    the next cycle.
##############################################################
if [[ ${#PDATE} -le 0 ]]; then  
   echo "PDATE not specified:  setting PDATE using last cycle"
   date=`${M_DE_SCRIPTS}/find_cycle.pl GDAS 1 ${M_TANKverf}`
   export PDATE=`$NDATE +6 $date`
else
   echo "PDATE was specified:  $PDATE"
fi

export PDY=`echo $PDATE|cut -c1-8`
export cyc=`echo $PDATE|cut -c9-10`
echo "PDY, cyc = $PDY, $cyc "

export m_jlogfile="${m_jlogfile}${MINMON_SUFFIX}.${PDY}.${cyc}.log"
echo  "m_jlogfile = $m_jlogfile"
jobname=minmon_de_${MINMON_SUFFIX}

rm -f $m_jlogfile
rm -rf $DATA_IN

echo "MY_MACHINE = $MY_MACHINE"
echo "SUB        = $SUB"
echo "JOB_QUEUE  = $JOB_QUEUE"
echo "PROJECT    = $PROJECT"
echo "jobname    = $jobname" 

if [[ $MY_MACHINE = "wcoss" ]]; then
   export PERL5LIB="/usrx/local/pm5/lib64/perl5:/usrx/local/pm5/share/perl5"
   $SUB -q $JOB_QUEUE -P $PROJECT -o ${m_jlogfile} -M 50 -R affinity[core] -W 0:10 -J ${jobname} $HOMEgdasgmon/jobs/JGDAS_VMINMON
fi

echo "end MinMon_DE.sh"
