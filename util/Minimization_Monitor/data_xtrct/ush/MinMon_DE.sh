#!/bin/ksh

#  MinMon data extraction script

#--------------------------------------------------------------------
#  usage
#--------------------------------------------------------------------
function usage {
  echo "Usage:  MinMonDE.sh suffix [pdate]"
  echo "            Suffix is the indentifier for this data source."
  echo "            Pdate is the full YYYYMMDDHH cycle to run.  This param is optional"
}

#--------------------------------------------------------------------
#  MinMon_DE.sh begins here
#--------------------------------------------------------------------
nargs=$#
if [[ $nargs -lt 1 || $nargs -gt 2 ]]; then
   usage
   exit 1
fi

this_file=`basename $0`
this_dir=`dirname $0`

export MINMON_SUFFIX=$1

if [[ $nargs -ge 2 ]]; then
   export PDATE=$2;
   echo "PDATE set to $PDATE"
fi

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
#    the next cycle
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

mdate=`$NDATE -24 $PDATE`
m1=`echo $mdate|cut -c1-8`

export M_TANKverfM0=${M_TANKverf}/minmon.${PDY}
export M_TANKverfM1=${M_TANKverf}/minmon.${m1}

lfile=${m_jlogfile}${MINMON_SUFFIX}.${PDY}.${cyc}
export pid=${pid:-$$}
export jlogfile=${lfile}.o${pid}
export m_jlogfile="${lfile}.log"
echo  "m_jlogfile = $m_jlogfile"

#############################################################
export job=gdas_vminmon.${cyc}
export jobid=${job}.${pid}
export envir=prod
export DATAROOT=${DATA_IN:-${STMP_USER}}
export COMROOT=${COMROOT:-/com}
export COMIN=${COMIN:-${COMROOT}/gfs/${envir}}
#############################################################
# Load modules
#############################################################
if [[ $MY_MACHINE = "wcoss" ]]; then
   . /usrx/local/Modules/3.2.9/init/ksh

   grib_util_ver=v1.0.0
   prod_util_ver=v1.0.1
   util_shared_ver=v1.0.1

   module use /nwprod2/modulefiles
   module load grib_util/$grib_util_ver
   module load prod_util/$prod_util_ver
   module load util_shared/$util_shared_ver

   module unload ics/12.1
   module load ics/15.0.3
elif [[ $MY_MACHINE = "cray" ]]; then
   . $MODULESHOME/init/ksh
   prod_util_ver=1.0.3
   export util_shared_ver=v1.0.2
   export gdas_minmon_ver=v1.0.0
   export minmon_shared_ver=v1.0.0
   module load prod_util/${prod_util_ver}
   module load prod_envir
   module load pm5
fi

module list


jobname=minmon_de_${MINMON_SUFFIX}

rm -f $m_jlogfile
rm -rf $DATA_IN

echo "MY_MACHINE = $MY_MACHINE"
echo "SUB        = $SUB"
echo "JOB_QUEUE  = $JOB_QUEUE"
echo "PROJECT    = $PROJECT"
echo "jobname    = $jobname" 

jobfile=${jobfile:-${HOMEgdas}/jobs/JGDAS_VMINMON}

if [[ $MY_MACHINE = "wcoss" ]]; then
   export PERL5LIB="/usrx/local/pm5/lib64/perl5:/usrx/local/pm5/share/perl5"
   echo "submitting job $jobname"

   $SUB -q $JOB_QUEUE -P $PROJECT -o ${m_jlogfile} -M 50 -R affinity[core] -W 0:10 -J ${jobname} $jobfile

elif [[ $MY_MACHINE = "cray" ]]; then
   $SUB -q $JOB_QUEUE -P $PROJECT -o ${m_jlogfile} -M 80 -R "select[mem>80] rusage[mem=80]" -W 0:10 -J ${jobname} $jobfile

fi


#module unload /nwprod2/modulefiles/prod_util/v1.0.2

echo "end MinMon_DE.sh"
