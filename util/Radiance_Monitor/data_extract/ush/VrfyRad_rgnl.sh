#!/bin/sh

#--------------------------------------------------------------------
#  VrfyRad_rgnl
#
#  Extract data for regional source.  
#--------------------------------------------------------------------

function usage {
  echo "Usage:  VrfyRad_rgnl.sh suffix run_envir"
  echo "            File name for VrfyRad_rgnl.sh can be full or relative path"
  echo "            Suffix is the indentifier for this data source."
  echo "            The run_envir maybe dev, para, or prod."
}

set -ax
echo start TEST_rgnl.sh

nargs=$#
if [[ $nargs -ne 2 ]]; then
   usage
   exit 1
fi


this_file=`basename $0`
this_dir=`dirname $0`

SUFFIX=$1
RUN_ENVIR=$2

echo SUFFIX    = $SUFFIX
echo RUN_ENVIR = $RUN_ENVIR
echo VRFYRAD_DIR = $VRFYRAD_DIR

if [[ $RUN_ENVIR != "dev" && $RUN_ENVIR != "prod" && $RUN_ENVIR != "para" ]]; then
  echo  ${RUN_ENVIR} does not match dev, para, or prod.
  echo
  usage
  exit 1
fi

jobname=data_extract_${SUFFIX}

#--------------------------------------------------------------------
# Set environment variables
#--------------------------------------------------------------------
export RAD_AREA=rgn
export MAKE_CTL=1
export MAKE_DATA=1

if [[ ${RUN_ENVIR} = para || ${RUN_ENVIR} = prod ]]; then
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
. ${PARMverf_rad}/rgnl_conf

mkdir -p $TANKDIR
mkdir -p $LOGDIR


#--------------------------------------------------------------------
# Check status of monitoring job.  Are any earlier verf jobs still
# running?  If so, exit this script and wait for job to finish.
#
# If we're good to go, clean out the $LOADLQ directory and proceed.
#--------------------------------------------------------------------

if [[ ${RUN_ENVIR} = dev ]]; then
   count=`ls ${LOADLQ}/${jobname}* | wc -l`
   complete=`grep "COMPLETED" ${LOADLQ}/${jobname}* | wc -l`
   running=`expr $count - $complete`

   if [[ $running -gt 0 ]]; then
      exit 3
   else
      rm -f ${LOADLQ}/${jobname}*
   fi
fi

#------------------------------------------------------------------
#  define data file sources depending on $RUN_ENVIR
#
#  need to idenfity correct output location(s) for binary files
#------------------------------------------------------------------
if [[ $RUN_ENVIR = dev ]]; then


   #--------------------------------------------------------------------
   # Get date of cycle to process.
   #--------------------------------------------------------------------

   pdate=`${SCRIPTS}/get_prodate.sh ${SUFFIX} ${DATA_MAP}`
   qdate=`${NDATE} +06 $pdate`
   export PDATE=${qdate}
 
   sdate=`echo $PDATE|cut -c1-8`
   export CYA=`echo $PDATE|cut -c9-10`

   #---------------------------------------------------------------
   # Locate required files.
   #---------------------------------------------------------------

   export DATDIR=${PTMP_USER}/regional
   export com=`${SCRIPTS}/get_datadir.sh ${SUFFIX} ${DATA_MAP}`

   /bin/sh ${SCRIPTS}/getbestndas_radstat.sh ${PDATE} ${DATDIR} ${com}


elif [[ ${RUN_ENVIR} = para || ${RUN_ENVIR} = prod ]]; then

   #---------------------------------------------------------------
   # Locate required files.
   #---------------------------------------------------------------
   
   export DATDIR=${PTMP_USER}/regional
   export com=`dirname ${COMOUT}`
   export PDATE=${CDATE}

   sdate=`echo ${PDATE}|cut -c1-8`
   export CYA=`echo ${PDATE}|cut -c9-10`

   /bin/sh $SCRIPTS/getbestndas_radstat.sh $PDATE $DATDIR $com

else
   echo RUN_ENVIR = $RUN_ENVIR
   exit 1
fi

tmpdir=${WORKverf_rad}/check_rad${SUFFIX}_${PDATE}
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

export biascr=$DATDIR/satbias.${PDATE}
export satang=$DATDIR/satang.${PDATE}
export radstat=$DATDIR/radstat.${PDATE}

#--------------------------------------------------------------------
# If data is available, export variables, and submit driver for
# radiance monitoring jobs.
#--------------------------------------------------------------------

data_available=0

#if [ -s $DATDIR/radstat.$PDATE -a -s $DATDIR/satang.$PDATE ]; then
#   if [ -s $DATDIR/satbias.$PDATE ]; then
if [ -s $radstat -a -s $satang -a -s $biascr ]; then
#   if [ -s $biascr ]; then
      data_available=1

      export MP_SHARED_MEMORY=yes
      export MEMORY_AFFINITY=MCM

      export envir=prod
      export RUN_ENVIR=dev
      export USE_ANL=0

      export PDY=`echo $PDATE|cut -c1-8`
      export CYC=`echo $PDATE|cut -c9-10`
      export cyc=$CYC

      export job=ndas_vrfyrad_${PDY}${cyc}
      export SENDSMS=NO
      export DATA_IN=/stmp/wx20es
      export DATA=/stmp/$LOGNAME/radmon_regional
      export jlogfile=/stmp/wx20es/jlogfile_${SUFFIX}
      export HOMEgfs=/global/save/wx20es/RadMon/util/Radiance_Monitor/nwprod
      export TANKverf=/u/$LOGNAME/nbns/stats/regional/${SUFFIX}
      export LOGDIR=/ptmp/$LOGNAME/logs/radnrx
      export USER_CLASS=dev

      export VERBOSE=YES
      export satype_file=${TANKverf}/info/SATYPE.txt

      #--------------------------------------------------------------------
      # Export listvar
      export listvar=MP_SHARED_MEMORY,MEMORY_AFFINITY,envir,RUN_ENVIR,PDY,cyc,job,SENDSMS,DATA_IN,DATA,jlogfile,HOMEgfs,TANKverf,MAIL_TO,MAIL_CC,VERBOSE,radstat,satang,biascr,USE_ANL,satype_file,DO_DIAG_RPT,DO_DATA_RPT,RAD_AREA,listvar

      #------------------------------------------------------------------
      #   Submit data processing jobs.

      $SUB -a $ACOUNT -e $listvar -j ${jobname} -q dev -g ${USER_CLASS} -t 0:05:00 -o ${LOGDIR}/data_extract.${SUFFIX}.${PDY}.${cyc}.log -v ${HOMEgfs}/jobs/JGDAS_VRFYRAD.sms.prod

      ${SCRIPTS}/set_prodate.sh $SUFFIX ${DATA_MAP} ${PDATE}

#   fi
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
   exit_value=5
fi

echo end TEST_rgnl.sh
exit ${exit_value}
