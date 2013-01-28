#!/bin/sh

#--------------------------------------------------------------------
#  VrfyRad_glbl.sh
#
#  Verification and data extraction script for global (GDAS) radiance
#  diagnostic data.
#
#  This script verifies data is available and submits the 
#  JGDAS_VRFYRAD.sms.prod job, which performs the data extraction 
#  and validation checks. 
#--------------------------------------------------------------------

function usage {
  echo "Usage:  VrfyRad_glbl.sh suffix run_envir [pdate]"
  echo "            File name for VrfyRad_glbl.sh may be full or relative path"
  echo "            Suffix is the indentifier for this data source."
  echo "            The run_envir may be dev, para, or prod." 
  echo "            Pdate is the full YYYYMMDDHH cycle to run.  This param is optional"
}

set -ax
echo start VrfyRad_glbl.sh

nargs=$#
if [[ $nargs -lt 2 || $nargs -gt 3 ]]; then
   usage
   exit 1
fi


this_file=`basename $0`
this_dir=`dirname $0`

export SUFFIX=$1
export RUN_ENVIR=$2

if [[ $nargs -eq 3 ]]; then
   export PDATE=$3
fi

echo SUFFIX = $SUFFIX
echo RUN_ENVIR = $RUN_ENVIR

if [[ $RUN_ENVIR != "dev" && $RUN_ENVIR != "prod" && $RUN_ENVIR != "para" ]]; then
  echo  ${RUN_ENVIR} does not match dev, para, or prod.
  echo
  usage
  exit 1
fi


#--------------------------------------------------------------------
# Set environment variables
#--------------------------------------------------------------------
export RAD_AREA=glb
export MAKE_CTL=${MAKE_CTL:-1}
export MAKE_DATA=${MAKE_DATA:-1}

if [[ $RUN_ENVIR = para || $RUN_ENVIR = prod ]]; then
   this_dir=${VRFYRAD_DIR}
fi


top_parm=${this_dir}/../../parm

if [[ -s ${top_parm}/RadMon_config ]]; then
   . ${top_parm}/RadMon_config
else
   echo "Unable to source RadMon_config file in ${top_parm}"
   exit 2
fi

echo endian = $LITTLE_ENDIAN

. ${RADMON_DATA_EXTRACT}/parm/data_extract_config
. ${PARMverf_rad}/glbl_conf


mkdir -p $TANKDIR
mkdir -p $LOGDIR


jobname=${DATA_EXTRACT_JOBNAME}
#--------------------------------------------------------------------
# Check status of monitoring job.  Are any earlier verf jobs still 
# running?  If so, exit this script and wait for job to finish.  
#
# If we're good to go clean out the $LOADLQ directory and proceed.
#--------------------------------------------------------------------

if [[ $RUN_ENVIR = dev ]]; then
   if [[ $MY_MACHINE = "ccs" ]]; then

      total=`llq -u ${LOGNAME} -f %jn | grep ${jobname} | wc -l`
   elif [[ $MY_MACHINE = "wcoss" ]]; then
      total=`bjobs -l | grep ${jobname} | wc -l`
   elif [[ $MY_MACHINE = "zeus" ]]; then
      total=`qstat -u ${LOGNAME} | grep ${jobname} | wc -l`
   fi

   if [[ $total -gt 0 ]]; then
      exit 3
   fi

fi


#------------------------------------------------------------------
#  define data file sources depending on $RUN_ENVIR
#
#  need to idenfity correct output location(s) for binary files
#------------------------------------------------------------------
if [[ $RUN_ENVIR = dev ]]; then

   #--------------------------------------------------------------------
   # Get and export settings for $SUFFIX.
   #--------------------------------------------------------------------
   export USER_CLASS=`${USHverf_rad}/query_data_map.pl ${DATA_MAP} ${SUFFIX} user_class`
   export ACCOUNT=`${USHverf_rad}/query_data_map.pl ${DATA_MAP} ${SUFFIX} account`
   export USE_ANL=`${USHverf_rad}/query_data_map.pl ${DATA_MAP} ${SUFFIX} use_anl`
   export DO_DIAG_RPT=`${USHverf_rad}/query_data_map.pl ${DATA_MAP} ${SUFFIX} do_diag_rpt`
   export DO_DATA_RPT=`${USHverf_rad}/query_data_map.pl ${DATA_MAP} ${SUFFIX} do_data_rpt`
   export RUN_ENVIR=`${USHverf_rad}/query_data_map.pl ${DATA_MAP} ${SUFFIX} run_envir`
   export USE_MAIL=`${USHverf_rad}/query_data_map.pl ${DATA_MAP} ${SUFFIX} use_mail`
   export MAIL_TO=`${USHverf_rad}/query_data_map.pl ${DATA_MAP} ${SUFFIX} mail_to`
   export MAIL_CC=`${USHverf_rad}/query_data_map.pl ${DATA_MAP} ${SUFFIX} mail_cc`

   #---------------------------------------------------------------
   # Get date of cycle to process.
   #---------------------------------------------------------------
   if [[ $PDATE = "" ]]; then
      pdate=`${USHverf_rad}/find_last_cycle.pl ${TANKDIR}`
      if [[ ${#pdate } -ne 10 ]]; then
         echo "ERROR:  Unable to locate any previous cycle's data files"
         echo "        Re-run this script with a specified starting cycle"
         exit 4
      fi
      qdate=`${NDATE} +06 $pdate`
      export PDATE=${qdate}
   fi

   export PDY=`echo $PDATE|cut -c1-8`
   export CYC=`echo $PDATE|cut -c9-10`
 
   export DATDIR=`${USHverf_rad}/query_data_map.pl ${DATA_MAP} ${SUFFIX} radstat_location`

   #---------------------------------------------------------------
   # Locate required files.             
   #---------------------------------------------------------------
   if [[ -d ${DATDIR}/gdas.$PDY ]]; then
      export DATDIR=${DATDIR}/gdas.${PDY}

      export biascr=$DATDIR/gdas1.t${CYC}z.abias  
      export satang=$DATDIR/gdas1.t${CYC}z.satang
      export radstat=$DATDIR/gdas1.t${CYC}z.radstat
   else
      export biascr=$DATDIR/biascr.gdas.${PDATE}  
      export satang=$DATDIR/satang.gdas.${PDATE}
      export radstat=$DATDIR/radstat.gdas.${PDATE}
   fi

elif [[ $RUN_ENVIR = para ]]; then

   export USER_CLASS=`${USHverf_rad}/query_data_map.pl ${DATA_MAP} ${SUFFIX} user_class`
   export ACCOUNT=`${USHverf_rad}/query_data_map.pl ${DATA_MAP} ${SUFFIX} account`
   export USE_ANL=`${USHverf_rad}/query_data_map.pl ${DATA_MAP} ${SUFFIX} use_anl`
   export DO_DIAG_RPT=`${USHverf_rad}/query_data_map.pl ${DATA_MAP} ${SUFFIX} do_diag_rpt`
   export DO_DATA_RPT=`${USHverf_rad}/query_data_map.pl ${DATA_MAP} ${SUFFIX} do_data_rpt`
   export RUN_ENVIR=`${USHverf_rad}/query_data_map.pl ${DATA_MAP} ${SUFFIX} run_envir`
   export USE_MAIL=`${USHverf_rad}/query_data_map.pl ${DATA_MAP} ${SUFFIX} use_mail`
   export MAIL_TO=`${USHverf_rad}/query_data_map.pl ${DATA_MAP} ${SUFFIX} mail_to`
   export MAIL_CC=`${USHverf_rad}/query_data_map.pl ${DATA_MAP} ${SUFFIX} mail_cc`

   #---------------------------------------------------------------
   # Locate required files.             
   #---------------------------------------------------------------
   export DATDIR=$COMOUT 
   export PDATE=$CDATE
   export PDY=`echo $PDATE|cut -c1-8`
   export CYC=`echo $PDATE|cut -c9-10`

   export biascr=$DATDIR/biascr.gdas.${CDATE}  
   export satang=$DATDIR/satang.gdas.${CDATE}
   export radstat=$DATDIR/radstat.gdas.${CDATE}

   echo biascr  = $biascr
   echo satang  = $satang
   echo radstat = $radstat

else
   export DATDIR=$COMOUT 
   export PDATE=$CDATE
   export PDY=`echo $PDATE|cut -c1-8`
   export CYC=`echo $PDATE|cut -c9-10`

   export biascr=$DATDIR/biascr.gdas.${CDATE}  
   export satang=$DATDIR/satang.gdas.${CDATE}
   export radstat=$DATDIR/radstat.gdas.${CDATE}

   echo biascr  = $biascr
   echo satang  = $satang
   echo radstat = $radstat

fi


#--------------------------------------------------------------------
# If data is available, export variables, and submit driver for
# radiance monitoring jobs.
#--------------------------------------------------------------------
data_available=0

if [[ -e ${radstat} ]]; then
   data_available=1                                         

   export MP_SHARED_MEMORY=yes
   export MEMORY_AFFINITY=MCM
   export envir=prod
   
   export cyc=$CYC
   export job=gdas_vrfyrad_${PDY}${cyc}
   export SENDSMS=${SENDSMS:-NO}
   export DATA_IN=${WORKverf_rad}
   export DATA=${DATA:-$STMP/$LOGNAME/radmon}
   export jlogfile=${WORKverf_rad}/jlogfile_${SUFFIX}
   export TANKverf=${MY_TANKDIR}/stats/${SUFFIX}

   export VERBOSE=${VERBOSE:-YES}
  

   if [[ $CYC = "00" ]]; then
      mkdir -p ${TANKverf}/radmon.${PDY}
      prev_day=`${NDATE} -06 $PDATE | cut -c1-8`
      cp ${TANKverf}/radmon.${prev_day}/gdas_radmon_satype.txt ${TANKverf}/radmon.${PDY}/.
   fi

   if [[ -s ${TANKverf}/info/radmon_base.tar.Z || -s ${TANKverf}/info/radmon_base.tar ]]; then
      export base_file=${TANKverf}/info/radmon_base.tar 
   fi

   export JOBNAME=${jobname}

   export listvar=MP_SHARED_MEMORY,MEMORY_AFFINITY,envir,RUN_ENVIR,PDY,cyc,job,SENDSMS,DATA_IN,DATA,jlogfile,HOMEgfs,TANKverf,USE_MAIL,MAIL_TO,MAIL_CC,VERBOSE,radstat,satang,biascr,USE_ANL,base_file,LITTLE_ENDIAN,PTMP,STMP,JOBNAME,Z,COMPRESS,UNCOMPRESS,TIMEX,MY_MACHINE,NDATE,DO_DIAG_RPT,DO_DATA_RPT,listvar

   #------------------------------------------------------------------
   #   Submit data processing jobs.
   #------------------------------------------------------------------
   if [[ $MY_MACHINE = "ccs" ]]; then
      $SUB -a $ACCOUNT -e $listvar -j ${jobname} -q dev -g ${USER_CLASS} -t 0:05:00 -o $LOGDIR/data_extract.${PDY}.${cyc}.log  $HOMEgfs/jobs/JGDAS_VRFYRAD.sms.prod

   elif [[ $MY_MACHINE = "wcoss" ]]; then
      $SUB -q $ACCOUNT -o $LOGDIR/data_extract.${PDY}.${cyc}.log -W 0:10 -J ${jobname} $HOMEgfs/jobs/JGDAS_VRFYRAD.sms.prod

   elif [[ $MY_MACHINE = "zeus" ]]; then
      $SUB -A $ACCOUNT -l procs=1,walltime=0:10:00 -N ${jobname} -v $listvar -o $LOGDIR/data_extract.${PDY}.${CYC}.log -e $LOGDIR/error_file.${PDY}.${CYC}.log $HOMEgfs/jobs/JGDAS_VRFYRAD.sms.prod
   fi
  
 
   rc=`${USHverf_rad}/update_data_map.pl ${DATA_MAP} ${SUFFIX} prodate ${PDATE}`
   if [[ $rc != 0 ]]; then
      echo "ERROR:  Attempt to update $DATA_MAP $PDATE failed"
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

echo end VrfyRad_glbl.sh
exit ${exit_value}

