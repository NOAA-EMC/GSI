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
echo start VrfyRad_rgnl.sh

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
# If we're good to go clean out the $LOADLQ directory and proceed.
#--------------------------------------------------------------------

if [[ ${RUN_ENVIR} = dev ]]; then
   count=`ls ${LOADLQ}/verf*_$SUFFIX* | wc -l`
   complete=`grep "COMPLETED" ${LOADLQ}/verf*_$SUFFIX* | wc -l`
   running=`expr $count - $complete`

   if [[ $running -ne 0 ]]; then
      exit 3
   else
      rm -f ${LOADLQ}/verf*_${SUFFIX}*
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
   cd ${WORKDIR}
   cd ..
   rm -rf ${WORKDIR}
   exit 0
fi

tmpdir=${WORKverf_rad}/check_rad${SUFFIX}_${PDATE}
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

#--------------------------------------------------------------------
# If data is available, export variables, and submit driver for
# radiance monitoring jobs.
#--------------------------------------------------------------------

data_available=0
if [ -s $DATDIR/radstat.$PDATE -a -s $DATDIR/satang.$PDATE ]; then
   if [ -s $DATDIR/satbias.$PDATE ]; then
      data_available=1

      #------------------------------------------------------------------
      # Set up WORKDIR.

      mkdir -p $WORKDIR
      cd $WORKDIR


      #--------------------------------------------------------------------
      # Copy data files file to local data directory.  Untar radstat file.  
      # Change DATDIR definition.

      datdirl=${WORKDIR}/datrad_regional.$SUFFIX
      rm -rf $datdirl
      mkdir -p $datdirl

      export CYA=`echo $PDATE|cut -c9-10`
      $NCP $DATDIR/radstat.$PDATE $datdirl/radstat.$PDATE
      $NCP $DATDIR/satbias.$PDATE   $datdirl/biascr.$PDATE
      $NCP $DATDIR/satang.$PDATE   $datdirl/satang.$PDATE

      cd $datdirl
      tar -xvf radstat.$PDATE
      rm radstat.$PDATE
#      rm -f *anl*

      #------------------------------------------------------------------
      #  Get SATYPE.  This is the list of sat/instrument sources.
      #    If USE_STATIC_SATYPE == 1, find the SATYPE.txt file in
      #      $TANKDIR.
      #    If USE_STATIC_SATYPE == 0, get the SATYPE list from
      #      available sources in the $radstat.
      #------------------------------------------------------------------

      radstat_satype=`ls -l d*ges* | sed -e 's/_/ /g;s/\./ /' | awk '{ print $10 "_" $11 }'`
      echo $radstat_satype


      USE_STATIC_SATYPE=`${SCRIPTS}/get_satype.sh ${SUFFIX} ${DATA_MAP}`
      USE_STATIC_SATYPE=${USE_STATIC_SATYPE:-0}
      echo $USE_STATIC_SATYPE

      if [[ $DO_DIAG_RPT -eq 1 && $USE_STATIC_SATYPE -eq 0 ]]; then
         echo
         echo "WARNING:  Diag report was requested (DO_DIAG_RPT = 1) but not using STATIC_SATYPE (USE_STATIC_SATYPE = 0)."
         echo "WARNING:  USE_STATIC_SATYPE should be set to 1 in the data_map file for this suffix."
         echo
      fi

      TANKDIR_INFO=${TANKDIR}/info
      STATIC_SATYPE_FILE=${TANKDIR_INFO}/SATYPE.txt

      if [[ ${USE_STATIC_SATYPE} -eq 0 ]]; then
         export SATYPE=""
         export SATYPE=${radstat_satype}
      else
         #-------------------------------------------------------------
         #  Find $TANKDIR_INFO.  If it doesn't exist, create it.
         #-------------------------------------------------------------
         if [[ ! -d ${TANKDIR_INFO} ]]; then
            echo " Directory $TANKDIR_INFO not found.  Adding it now."
            mkdir ${TANKDIR_INFO}
         fi

         #-------------------------------------------------------------
         #  Find $STATIC_SATYPE_FILE.  If it doesn't exist, create it
         #  using the $radstat_satype.
         #-------------------------------------------------------------
         if [[ ! -s ${STATIC_SATYPE_FILE} ]]; then
            echo " Directory $STATIC_SATYPE_FILE not found.  Adding it now using radstat file contents."
            export SATYPE=$radstat_satype
            echo $SATYPE > ${STATIC_SATYPE_FILE}
         else
            #-------------------------------------------------------------
            #  Found  $STATIC_SATYPE_FILE.
            #-------------------------------------------------------------
            echo "Located $STATIC_SATYPE_FILE, loading SATYPE from it now."
            SATYPE=""
            SATYPE=`cat ${STATIC_SATYPE_FILE}`
            echo $SATYPE

            #-------------------------------------------------------------
            #  Update the SATYPE if USE_STATIC_SATYPE is true and a new
            #  sat/instrument is found in $radstat_satype.
            #-------------------------------------------------------------
            satype_changes=0
            new_satype=$SATYPE
            for type in ${radstat_satype}; do
               test=`echo $SATYPE | grep $type | wc -l`

               if [[ $test -eq 0 ]]; then
                  echo "Found $type in radstat file but not in SATYPE list.  Adding it now."
                  satype_changes=1
                  new_satype="$new_satype $type"
               fi

               if [[ $satype_changes -eq 1 ]]; then
                  SATYPE=$new_satype
                  rm -f ${STATIC_SATYPE_FILE}
                  echo $SATYPE > ${STATIC_SATYPE_FILE}
               fi
            done
         fi
      fi

      export DATDIR=$datdirl


      #--------------------------------------------------------------------
      # Export variables
      export listvar=MAKE_CTL,MAKE_DATA,RAD_AREA,MAIL_TO,MAIL_CC,DISCLAIMER,DO_DIAG_RPT,PDATE,NDATE,DATDIR,TANKDIR,LOADLQ,EXEDIR,LOGDIR,WORKDIR,SCRIPTS,USER,USER_CLASS,SUB,SUFFIX,SATYPE,NCP,ACOUNT,DATA_MAP,listvar


      #------------------------------------------------------------------
      #   Submit data processing jobs.

      rm $LOGDIR/angle.log
      $SUB -a $ACOUNT -e $listvar -j verf_angle_${SUFFIX} -q dev -g ${USER_CLASS}  -t 0:10:00 -o $LOGDIR/angle.log $SCRIPTS/verf_angle.sh

      rm $LOGDIR/bcoef.log
      $SUB -a $ACOUNT -e $listvar -j verf_bcoef_${SUFFIX} -q dev  -g ${USER_CLASS}  -t 0:10:00 -o $LOGDIR/bcoef.log $SCRIPTS/verf_bcoef.sh

      rm $LOGDIR/bcor.log
      $SUB -a $ACOUNT -e $listvar -j verf_bcor_${SUFFIX} -q dev  -g ${USER_CLASS} -t 0:10:00 -o $LOGDIR/bcor.log $SCRIPTS/verf_bcor.sh

      rm $LOGDIR/time.log
      $SUB -a $ACOUNT -e $listvar -j verf_time_${SUFFIX} -q dev -g ${USER_CLASS}  -t 0:10:00 -o $LOGDIR/time.log $SCRIPTS/verf_time.sh

      rm $LOGDIR/update.log
      $SUB -a $ACOUNT -e $listvar -j verf_update_${SUFFIX} -q dev  -g ${USER_CLASS} -t 0:10:00 -o $LOGDIR/update.log $SCRIPTS/verf_update.sh


   fi
fi

#--------------------------------------------------------------------
# Clean up and exit
#--------------------------------------------------------------------
cd $tmpdir
cd ../
rm -rf $tmpdir

exit_value=0
if [[ ${data_available} -ne 1 ]]; then
   echo No data available for ${SUFFIX}
   exit_value=5
fi

echo end VrfyRad_rgnl.sh
exit ${exit_value}
