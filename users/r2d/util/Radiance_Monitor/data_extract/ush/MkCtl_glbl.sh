#!/bin/sh

#--------------------------------------------------------------------
#  MkCtl_glbl.sh
#
#    This script generates the control files for a given suffix 
#    (source), using the JGDAS_VERFRAD job.  The resulting
#    control files are stored in $TANKverf.
#    
#    This script is designed to be run manually, and should only be
#    necessary if the user had previously overriden the default 
#    settings and switched off the control file generation done by
#    the VrfyRad_*.sh scripts.
#
#--------------------------------------------------------------------

function usage {
  echo "Usage:  MkCtl_glbl.sh suffix"
  echo "            File name for MkCtl_glbl.sh may be full or relative path"
  echo "            Suffix is the indentifier for this data source"
}

set -ax
echo start MkCtl_glbl.sh

nargs=$#
if [[ $nargs -ne 1 ]]; then
   usage
   exit 1
fi

this_file=`basename $0`
this_dir=`dirname $0`

export SUFFIX=$1
jobname=make_ctl_${SUFFIX}

echo SUFFIX = $SUFFIX
echo RUN_ENVIR = $RUN_ENVIR

#--------------------------------------------------------------------
# Set environment variables
#--------------------------------------------------------------------

top_parm=${this_dir}/../../parm
export RADMON_CONFIG=${RADMON_CONFIG:-${top_parm}/RadMon_config}
if [[ -s ${RADMON_VERSION} ]]; then
   . ${RADMON_VERSION}
else
   echo "Unable to source ${RADMON_VERSION} file"
   exit 2
fi

if [[ -s ${RADMON_CONFIG} ]]; then
   . ${RADMON_CONFIG}
else
   echo "Unable to source ${RADMON_CONFIG} file"
   exit 2
fi

if [[ -s ${RADMON_USER_SETTINGS} ]]; then
   . ${RADMON_USER_SETTINGS}
else
   echo "Unable to source ${RADMON_USER_SETTINGS} file"
   exit 2
fi

. ${DE_PARM}/parm/data_extract_config

#--------------------------------------------------------------------
# Get the area (glb/rgn) for this suffix
#--------------------------------------------------------------------
area=${RAD_AREA}
echo $area

if [[ $area = glb ]]; then
   export RAD_AREA=glb
elif [[ $area = rgn ]]; then
   export RAD_AREA=rgn
else
  echo "area = $area -- must be either glb or rgn"
  exit 3 
fi

mkdir -p $TANKverf
mkdir -p $LOGdir

export MAKE_CTL=1
export MAKE_DATA=0
export RUN_ENVIR=dev

#---------------------------------------------------------------
# Get date of cycle to process.  Start with the last processed
# date in the $TANKverf and work backwards until we find a
# valid radstat file or hit the limit on $ctr. 
#---------------------------------------------------------------
PDATE=`${DE_SCRIPTS}/find_cycle.pl 1 ${TANKverf}`
export DATDIR=$RADSTAT_LOCATION
   
ctr=0
need_radstat=1
while [[ $need_radstat -eq 1 && $ctr -lt 10 ]]; do

   sdate=`echo $PDATE|cut -c1-8`
   export CYA=`echo $PDATE|cut -c9-10`
   testdir=${DATDIR}/gdas.$sdate

   #---------------------------------------------------------------
   # Locate required files or reset PDATE and try again.
   #---------------------------------------------------------------
   if [[ -s $testdir/gdas1.t${CYA}z.radstat ]]; then

      export biascr=${testdir}/gdas1.t${CYA}z.abias
      export radstat=${testdir}/gdas1.t${CYA}z.radstat
      need_radstat=0
   elif [[ -s $testdir/radstat.gdas.${PDATE} ]]; then
      export biascr=$DATDIR/biascr.gdas.${PDATE}  
      export radstat=$DATDIR/radstat.gdas.${PDATE}
      need_radstat=0
   else
      export PDATE=`$NDATE -06 $PDATE`
      ctr=$(( $ctr + 1 ))
   fi
done

export PDY=`echo $PDATE|cut -c1-8`
export CYC=`echo $PDATE|cut -c9-10`

#--------------------------------------------------------------------
#  Process if radstat file exists.
#--------------------------------------------------------------------
data_available=0
if [[ -s ${radstat} ]]; then
   data_available=1

   export MP_SHARED_MEMORY=yes
   export MEMORY_AFFINITY=MCM

   export envir=prod
   export RUN_ENVIR=dev
   
   export cyc=$CYC
   export job=gdas_mkctl_${PDY}${cyc}
   export SENDSMS=NO
   export DATA_IN=${STMP_USER}
   export DATA=${STMP_USER}/radmon
   export jlogfile=${STMP_USER}/jlogfile

   export VERBOSE=YES
   export satype_file=${TANKverf}/info/SATYPE.txt


   #------------------------------------------------------------------
   #   Submit data processing jobs.
   #------------------------------------------------------------------
   if [[ $MY_MACHINE = "wcoss" ]]; then
      $SUB -q $JOB_QUEUE -P $PROJECT -o $LOGdir/mk_ctl.${PDY}.${cyc}.log -M 40 -R affinity[core] -W 0:10 -J ${jobname} $HOMEradmon/jobs/JGDAS_VERFRAD
   elif [[ $MY_MACHINE = "zeus" ]]; then
      $SUB -A $ACCOUNT -l walltime=0:05:00 -V -j oe -o $LOGdir/make_ctl.${PDY}.${cyc}.log $HOMEradmon/jobs/JGDAS_VERFRAD
   fi


fi

#--------------------------------------------------------------------
# Clean up and exit
#--------------------------------------------------------------------

exit_value=0
if [[ ${data_available} -ne 1 ]]; then
   exit_value=5
   echo No data available for ${SUFFIX}
fi

echo end MkCtl_glbl.sh
exit ${exit_value}

