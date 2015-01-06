#!/bin/sh

#--------------------------------------------------------------------
#  VrfyRad_rgnl
#
#  Extract data for regional source.  
#--------------------------------------------------------------------
set -ax
echo start VrfyRad_rgnl.sh



#--------------------------------------------------------------------
#  useage function
#--------------------------------------------------------------------
function usage {
  echo "Usage:  VrfyRad_rgnl.sh suffix [pdate] "
  echo "            File name for VrfyRad_rgnl.sh can be full or relative path"
  echo "            Suffix is the indentifier for this data source."
  echo "            Pdate is the full YYYYMMDDHH cycle to run.  This param is optional"
}


#--------------------------------------------------------------------
#  VrfyRad_rgnl.sh begins here
#--------------------------------------------------------------------
nargs=$#
if [[ $nargs -lt 1 || $nags -gt 3 ]]; then
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
   fi

   if [[ $nargs -eq 3 ]]; then
      export PDATE=$3;
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


#--------------------------------------------------------------------
# Set environment variables
#--------------------------------------------------------------------
export RAD_AREA=rgn
export MAKE_CTL=${MAKE_CTL:-1}
export MAKE_DATA=${MAKE_DATE:-1}

if [[ ${RUN_ENVIR} = para || ${RUN_ENVIR} = prod ]]; then
   this_dir=${VRFYRAD_DIR}
fi


top_parm=${this_dir}/../../parm
export RADMON_VERSION=${RADMON_VERSION:-${top_parm}/radmon.ver}
if [[ -s ${RADMON_VERSION} ]]; then
   . ${RADMON_VERSION}
else
   echo "Unable to source ${RADMON_VERSION} file"
   exit 2
fi

export RADMON_CONFIG=${RADMON_CONFIG:-${top_parm}/RadMon_config}

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
   exit 3 
fi

. ${DE_PARM}/data_extract_config


#--------------------------------------------------------------------
#  Check setting of RUN_ONLY_ON_DEV and possible abort if on prod and
#  not permitted to run there.
#--------------------------------------------------------------------

if [[ RUN_ONLY_ON_DEV -eq 1 ]]; then
   is_prod=`${DE_SCRIPTS}/onprod.sh`
   if [[ $is_prod = 1 ]]; then
      exit 10
   fi
fi


#--------------------------------------------------------------------

mkdir -p $TANKverf
mkdir -p $LOGdir

jobname=$DATA_EXTRACT_JOBNAME

#--------------------------------------------------------------------
# Check status of monitoring job.  Are any earlier verf jobs still
# running?  If so, exit this script and wait for job to finish.
#
# If we're good to go, clean out the $LOADLQ directory and proceed.
#--------------------------------------------------------------------

if [[ ${RUN_ENVIR} = dev ]]; then
   if [[ $MY_MACHINE = "wcoss" ]]; then
      total=`bjobs -l | grep ${jobname} | wc -l`
   elif [[ $MY_MACHINE = "zeus" ]]; then
      total=0
      line=`qstat -u ${LOGNAME} | grep ${jobname}`
      test=`echo $line | gawk '{print $10}'`

      total=`echo $line | grep ${jobname} | wc -l`
      if [[ $test = "C" && $total -eq "1" ]]; then
         total=0
      fi
   fi

   if [[ $total -gt 0 ]]; then
      exit 4
   fi
fi

tmpdir=${WORKverf_rad}/check_rad${SUFFIX}
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

#------------------------------------------------------------------
#  define data file sources depending on $RUN_ENVIR
#
#  need to idenfity correct output location(s) for binary files
#------------------------------------------------------------------
if [[ $RUN_ENVIR = dev ]]; then

   #--------------------------------------------------------------------
   # Get date of cycle to process.  Use the specified date from the 
   #   command line, if provided.
   #
   #   If no date was provided then determine the last processed date
   #   ($pdate) and add 06 hrs to determine the next cycle ($qdate).  
   #   Also run the find_ndas_radstat.pl script to determine the 
   #   earliest date for which a radstat file exists ($fdate).  
   #   Sometimes there are breaks in radstat file availability.  If the 
   #   next cycle is for a date less than the next available date, then 
   #   we have to use the next available date.
   #--------------------------------------------------------------------
   export DATDIR=${PTMP_USER}/regional
   export com=${RADSTAT_LOCATION}

   if [[ $PDATE = "" ]]; then
      pdate=`${DE_SCRIPTS}/find_cycle.pl 1 ${TANKverf}`
      if [[ ${#pdate} -ne 10 ]]; then
         echo "ERROR:  Unable to locate any previous cycle's data files"
         echo "        Re-run this script with a specified starting cycle"
         exit 5
      fi

      qdate=`${NDATE} +06 $pdate`

      fdate=`${DE_SCRIPTS}/find_ndas_radstat.pl 0 $com`
      echo $fdate

      if [[ $qdate -ge $fdate ]]; then
         export PDATE=$qdate
      else 
         export PDATE=$fdate
      fi
   fi 
   sdate=`echo $PDATE|cut -c1-8`
   export CYA=`echo $PDATE|cut -c9-10`

   #---------------------------------------------------------------
   # Locate required files.
   #---------------------------------------------------------------
   echo $PDATE
   DATEM12=`${NDATE} +12 $PDATE`
   echo $DATEM12 

   PDY00=`echo $PDATE | cut -c 1-8` 
   HH00=`echo $PDATE | cut -c 9-10`
   PDY12=`echo $DATEM12 | cut -c 1-8`
   HH12=`echo $DATEM12 | cut -c 9-10`

   radstat=$com/ndas.$PDY12/ndas.t${HH12}z.radstat.tm12
   biascr=$com/ndas.$PDY12/ndas.t${HH12}z.satbiasc.tm12

   echo RADSTAT = $radstat
   echo BIASCR  = $biascr

   /bin/sh ${DE_SCRIPTS}/getbestndas_radstat.sh ${PDATE} ${DATDIR} ${com}


elif [[ ${RUN_ENVIR} = para ]]; then

   #---------------------------------------------------------------
   # Locate required files.
   #---------------------------------------------------------------
   
   export DATDIR=${PTMP_USER}/regional
   export com=`dirname ${COMOUT}`
   export PDATE=${CDATE}

   sdate=`echo ${PDATE}|cut -c1-8`
   export CYA=`echo ${PDATE}|cut -c9-10`

   /bin/sh ${DE_SCRIPTS}/getbestndas_radstat.sh $PDATE $DATDIR $com

else
   echo RUN_ENVIR = $RUN_ENVIR
   exit 1
fi

export biascr=$DATDIR/satbias.${PDATE}
export radstat=$DATDIR/radstat.${PDATE}

echo "via getbestndas_radstat.sh:"
echo RADSTAT = $radstat
echo BIASCR  = $biascr

#--------------------------------------------------------------------
# If data is available, export variables, and submit driver for
# radiance monitoring jobs.
#--------------------------------------------------------------------

data_available=0

if [ -s $radstat -a -s $biascr ]; then
   data_available=1

   export MP_SHARED_MEMORY=yes
   export MEMORY_AFFINITY=MCM
   export envir=prod

   export PDY=`echo $PDATE|cut -c1-8`
   export cyc=`echo $PDATE|cut -c9-10`

   export job=ndas_vrfyrad_${PDY}${cyc}
   export SENDSMS=${SENDSMS:-NO}
   export DATA_IN=${WORKverf_rad}
   export DATA=${DATA:-${STMP_USER}/radmon_regional}
   export jlogfile=${WORKverf_rad}/jlogfile_${SUFFIX}

   export VERBOSE=${VERBOSE:-YES}
  
 
   if [[ $cyc = "00" ]]; then
      mkdir -p ${TANKverf}/radmon.${PDY}
      prev_day=`${NDATE} -06 $PDATE | cut -c1-8`
      cp ${TANKverf}/radmon.${prev_day}/gdas_radmon_satype.txt ${TANKverf}/radmon.${PDY}/.
   fi

   #------------------------------------------------------------------
   #   Override the default base_file declaration if there is an
   #   available base file for this source.
   #------------------------------------------------------------------
   if [[ -s ${TANKverf}/info/radmon_base.tar.Z || -s ${TANKverf}/info/radmon_base.tar ]]; then
      export base_file=${TANKverf}/info/radmon_base.tar
   fi


   #------------------------------------------------------------------
   #   Submit data processing jobs.

   logfile=$LOGdir/data_extract.${SUFFIX}.${PDY}.${cyc}.log

   if [[ $MY_MACHINE = "wcoss" ]]; then
      $SUB -q $JOB_QUEUE -P $PROJECT -M 40 -R affinity[core] -o ${logfile} -W 0:10 -J ${jobname} $HOMEgdasradmon/jobs/JGDAS_VERFRAD
   elif [[ $MY_MACHINE = "zeus" ]]; then
      $SUB -A $ACCOUNT -l procs=1,walltime=0:05:00 -N ${jobname} -V -j oe -o ${logfile} ${HOMEgdasradmon}/jobs/JGDAS_VERFRAD
   fi

fi

#--------------------------------------------------------------------
# Clean up and exit
#--------------------------------------------------------------------
#cd $tmpdir
#cd ../
#rm -rf $tmpdir

exit_value=0
if [[ ${data_available} -ne 1 ]]; then
   echo No data available for ${SUFFIX}
   exit_value=6
fi

echo end VrfyRad_rgn.sh
exit ${exit_value}
