#!/bin/sh

#--------------------------------------------------------------------
#  MkCtl_rgnl.sh
#
#    This script generates the control files for a given suffix
#    (source), using the JGDAS_VRFYRAD.sms.prod job.  The resulting
#    control files are stored in $TANKDIR.
#
#    This script is designed to be run manually, and should only be
#    necessary if the user had previously overriden the default
#    settings and switched off the control file generation done by
#    the VrfyRad_*.sh scripts.
#
#--------------------------------------------------------------------

function usage {
  echo "Usage:  MkCtl_rgnl.sh suffix"
  echo "            File name for MkCtl_rgnl.sh may be full or relative path"
  echo "            Suffix is the indentifier for this data source, and should"
  echo "             correspond to an entry in the ../../parm/data_map file."
}


set -ax
echo start MkCtl_rgnl.sh

nargs=$#
if [[ $nargs -ne 1 ]]; then
   usage
   exit 1
fi


this_file=`basename $0`
this_dir=`dirname $0`

SUFFIX=$1
export RUN_ENVIR=dev

echo SUFFIX    = $SUFFIX
echo RUN_ENVIR = $RUN_ENVIR


jobname=make_ctl_${SUFFIX}

#--------------------------------------------------------------------
# Set environment variables
#--------------------------------------------------------------------
export RAD_AREA=rgn
export MAKE_CTL=1
export MAKE_DATA=0

top_parm=${this_dir}/../../parm

if [[ -s ${top_parm}/RadMon_config ]]; then
   . ${top_parm}/RadMon_config
else
   echo "Unable to source RadMon_config file in ${top_parm}"
   exit 2 
fi

if [[ -s ${top_parm}/RadMon_user_settings ]]; then
   . ${top_parm}/RadMon_user_settings
else
   echo "Unable to source RadMon_user_settings file in ${top_parm}"
   exit 2 
fi

. ${RADMON_DATA_EXTRACT}/parm/data_extract_config
. ${PARMverf_rad}/rgnl_conf

mkdir -p $TANKDIR
mkdir -p $LOGDIR


tmpdir=${WORKverf_rad}/check_rad${SUFFIX}
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

#--------------------------------------------------------------------
# Get date of cycle to process.  Start with the last time in the 
# data_map file and work backwards until we find a diag file to use
# or run out of the $ctr.
#--------------------------------------------------------------------
export PDATE=`${SCRIPTS}/find_last_cycle.pl ${TANKDIR}` 

#---------------------------------------------------------------
# Locate required files.
#---------------------------------------------------------------
export DATDIR=${PTMP_USER}/regional
export com=$RADSTAT_LOCATION

biascr=$DATDIR/satbias.${PDATE}
satang=$DATDIR/satang.${PDATE}
radstat=$DATDIR/radstat.${PDATE}

ctr=0
need_radstat=1
while [[ $need_radstat -eq 1 && $ctr -lt 10 ]]; do

   sdate=`echo $PDATE|cut -c1-8`
   export CYA=`echo $PDATE|cut -c9-10`
   /bin/sh ${USHverf_rad}/getbestndas_radstat.sh ${PDATE} ${DATDIR} ${com}

   if [ -s $radstat -a -s $satang -a -s $biascr ]; then
      need_radstat=0
   else
      export PDATE=`$NDATE -06 $PDATE`
      ctr=$(( $ctr + 1 ))
   fi

done


export biascr=$biascr
export satang=$satang
export radstat=$radstat

#--------------------------------------------------------------------
# If data is available, export variables, and submit driver for
# radiance monitoring jobs.
#--------------------------------------------------------------------

data_available=0

if [ -s $radstat -a -s $satang -a -s $biascr ]; then
   data_available=1

   export MP_SHARED_MEMORY=yes
   export MEMORY_AFFINITY=MCM

   export envir=prod
   export RUN_ENVIR=dev
   export USE_ANL=0

   export PDY=`echo $PDATE|cut -c1-8`
   export CYC=`echo $PDATE|cut -c9-10`
   export cyc=$CYC

   export job=ndas_make_ctl_${PDY}${cyc}
   export SENDSMS=NO
   export DATA_IN=${WORKverf_rad}
   export DATA=${WORKverf_rad}/radmon_regional
   export jlogfile=${WORKverf_rad}/jlogfile_${SUFFIX}
   export TANKverf=${MY_TANKDIR}/stats/regional/${SUFFIX}
   export LOGDIR=$PTMP/$LOGNAME/logs/radnrx
   export USER_CLASS=dev
   export DO_DIAG_RPT=0
   export DO_DATA_RPT=0

   export VERBOSE=YES
   export satype_file=${TANKverf}/info/SATYPE.txt
   if [[ -s ${TANKverf}/info/radmon_base.tar.Z ]]; then
      export base_file=${TANKverf}/info/radmon_base.tar
   fi

   #--------------------------------------------------------------------
   # Export listvar
   export listvar=MP_SHARED_MEMORY,MEMORY_AFFINITY,envir,RUN_ENVIR,PDY,cyc,job,SENDSMS,DATA_IN,DATA,jlogfile,HOMEgfs,TANKverf,MAIL_TO,MAIL_CC,VERBOSE,radstat,satang,biascr,USE_ANL,satype_file,base_file,DO_DIAG_RPT,DO_DATA_RPT,RAD_AREA,MAKE_DATA,MAKE_CTL,listvar

   #------------------------------------------------------------------
   #   Submit data processing jobs.

   $SUB -a $ACCOUNT -e $listvar -j ${jobname} -q dev -g ${USER_CLASS} -t 0:05:00 -o ${LOGDIR}/make_ctl.${SUFFIX}.${PDY}.${cyc}.log -v ${HOMEgfs}/jobs/JGDAS_VRFYRAD.sms.prod

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

echo end MkCtl_rgnl.sh
exit ${exit_value}
