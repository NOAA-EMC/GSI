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
  echo "Usage:  VrfyRad_glbl.sh suffix run_envir"
  echo "            File name for VrfyRad_glbl.sh may be full or relative path"
  echo "            Suffix is the indentifier for this data source."
  echo "            The run_envir may be dev, para, or prod." 
}

set -ax
echo start VrfyRad_glbl.sh

nargs=$#
if [[ $nargs -ne 2 ]]; then
   usage
   exit 1
fi

this_file=`basename $0`
this_dir=`dirname $0`

export SUFFIX=$1
export RUN_ENVIR=$2
jobname=data_extract_${SUFFIX}

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


. ${RADMON_DATA_EXTRACT}/parm/data_extract_config
. ${PARMverf_rad}/glbl_conf

echo IMGNDIR = $IMGNDIR

mkdir -p $TANKDIR
mkdir -p $LOGSverf_rad


#--------------------------------------------------------------------
# Check status of monitoring job.  Are any earlier verf jobs still 
# running?  If so, exit this script and wait for job to finish.  
#
# If we're good to go clean out the $LOADLQ directory and proceed.
#--------------------------------------------------------------------

if [[ $RUN_ENVIR = dev ]]; then
   count=`ls ${LOADLQ}/${jobname}* | wc -l`
   complete=`grep "COMPLETED" ${LOADLQ}/${jobname}* | wc -l`

   total=`expr $count - $complete`

   if [[ $total -gt 0 ]]; then
      exit 3
   else
      rm -f ${LOADLQ}/${jobname}*
   fi
fi

export SCRIPTS=$USHverf_rad
export MAKE_CTL=1
export MAKE_DATA=1
#export USE_ANL=0
export USE_ANL=`${SCRIPTS}/get_anl.sh ${SUFFIX} ${DATA_MAP}`

#------------------------------------------------------------------
#  define data file sources depending on $RUN_ENVIR
#
#  need to idenfity correct output location(s) for binary files
#------------------------------------------------------------------
if [[ $RUN_ENVIR = dev ]]; then

   #---------------------------------------------------------------
   # Get date of cycle to process.
   #---------------------------------------------------------------
   pdate=`${SCRIPTS}/get_prodate.sh ${SUFFIX} ${DATA_MAP}`
   qdate=`${NDATE} +06 $pdate`
   export PDATE=${qdate}

   export PDY=`echo $PDATE|cut -c1-8`
   export CYC=`echo $PDATE|cut -c9-10`
 
   export DATDIR=`${SCRIPTS}/get_datadir.sh ${SUFFIX} ${DATA_MAP}`

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
   export MAKE_CTL=0

   export biascr=$DATDIR/biascr.gdas.${CDATE}  
   export satang=$DATDIR/satang.gdas.${CDATE}
   export radstat=$DATDIR/radstat.gdas.${CDATE}

   echo biascr  = $biascr
   echo satang  = $satang
   echo radstat = $radstat

fi


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
   export job=gdas_vrfyrad_${PDY}${cyc}
   export SENDSMS=NO
   export DATA_IN=/stmp/wx20es
   export DATA=/stmp/$LOGNAME/radmon
   export jlogfile=/stmp/wx20es/jlogfile
   export TANKverf=/u/$LOGNAME/nbns/stats/${SUFFIX}
   export LOGDIR=/ptmp/$LOGNAME/logs/radopr

   export VERBOSE=YES
   export satype_file=${TANKverf}/info/SATYPE.txt
   if [[ -s ${TANKverf}/info/radmon_base.tar.Z ]]; then
      export base_file=${TANKverf}/info/radmon_base.tar 
   fi

   export listvar=MP_SHARED_MEMORY,MEMORY_AFFINITY,envir,RUN_ENVIR,PDY,cyc,job,SENDSMS,DATA_IN,DATA,jlogfile,HOMEgfs,TANKverf,MAIL_TO,MAIL_CC,VERBOSE,radstat,satang,biascr,USE_ANL,satype_file,base_file,listvar

   #------------------------------------------------------------------
   #   Submit data processing jobs.
   #------------------------------------------------------------------
   $SUB -a $ACOUNT -e $listvar -j ${jobname} -q dev -g ${USER_CLASS} -t 0:05:00 -o $LOGDIR/data_extract.${PDY}.${cyc}.log  $HOMEgfs/jobs/JGDAS_VRFYRAD.sms.prod

   ${SCRIPTS}/set_prodate.sh $SUFFIX ${DATA_MAP} ${PDATE}
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

