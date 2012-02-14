#!/bin/sh

#--------------------------------------------------------------------
#  MkCtl_rgnl
#
#  Create control files for regional (NDAS) radiance diagnostic data.
#  The control files will be placed in the $TANKDIR subdirectories.
#--------------------------------------------------------------------

function usage {
  echo "Usage:  MkCtl_rgnl.sh suffix run_envir"
  echo "            File name for MkCtl_rgnl.sh can be full or relative path"
  echo "            Suffix is the indentifier for this data source."
  echo "            The run_envir maybe dev, para."
}

set -ax
echo start MkCtl_rgnl.sh

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

#--------------------------------------------------------------------
# Set environment variables
#--------------------------------------------------------------------
export RAD_AREA=rgn
export MAKE_CTL=1
export MAKE_DATA=0

if [[ $RUN_ENVIR = para || $RUN_ENVIR = prod ]]; then
   this_dir=${VRFYRAD_DIR}
fi


top_parm=${this_dir}/../../parm

if [[ -s ${top_parm}/RadMon_config ]]; then
   . ${top_parm}/RadMon_config
else
   echo "Unable to source RadMon_config file in ${top_parm}"
   exit
fi

. ${RADMON_DATA_EXTRACT}/parm/data_extract_config
. ${PARMverf_rad}/rgnl_conf

#--------------------------------------------------------------------
#  Empty out the MAIL variables.  If we're just building control
#  files we don't need any mail notifications.
#--------------------------------------------------------------------
export MAIL_TO=
export MAIL_CC=


mkdir -p $TANKDIR
mkdir -p $LOGDIR

tmpdir=${WORKverf_rad}/check_rad${SUFFIX}
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

need_radstat=1

#------------------------------------------------------------------
#  define data file sources depending on $RUN_ENVIR
#
#  need to idenfity correct output location(s) for binary files
#------------------------------------------------------------------
if [[ $RUN_ENVIR = dev ]]; then

   export com=`get_datadir ${SUFFIX} ${RADMON_PARM}/data_map`

   export DATDIR=${PTMP_USER}/regional

   #--------------------------------------------------------------------
   # Get date of cycle to process.
   #--------------------------------------------------------------------
   export PDATE=`get_prodate ${SUFFIX} ${RADMON_PARM}/data_map`

   ctr=0
   while [[ $need_radstat -eq 1 && $ctr -lt 10 ]]; do

      sdate=`echo $PDATE|cut -c1-8`
      export CYA=`echo $PDATE|cut -c9-10`

      #---------------------------------------------------------------
      # Locate required files or reset PDATE and try again.
      #---------------------------------------------------------------

      /bin/sh $SCRIPTS/getbestndas_radstat.sh $PDATE $DATDIR $com

      if [[ -s $DATDIR/radstat.$PDATE ]]; then
         need_radstat=0
      else
         export PDATE=`ndate -06 $PDATE`
         ctr=$(( $ctr + 1 ))
      fi

   done

elif [[ $RUN_ENVIR = para ]]; then

   
   export DATDIR=${PTMP_USER}/regional
   export com=$COMOUT
   export PDATE=$CDATE

   sdate=`echo $PDATE|cut -c1-8`
   export CYA=`echo $PDATE|cut -c9-10`
   ctr=0

   #---------------------------------------------------------------
   # Locate required files or reset PDATE and try again.
   #---------------------------------------------------------------
   while [[ $need_radstat -eq 1 && $ctr -lt 10 ]]; do
      /bin/sh $SCRIPTS/getbestndas_radstat.sh $PDATE $DATDIR $com

      if [[ -s $DATDIR/radstat.$PDATE ]]; then
         need_radstat=0
      else
         export PDATE=`ndate -06 $PDATE`
         ctr=$(( $ctr + 1 ))
      fi

   done

else
   export MAKE_CTL=0
   echo RUN_ENVIR = $RUN_ENVIR
   cd ${WORKDIR}
   cd ..
   rm -rf ${WORKDIR}
   exit 0
fi


#--------------------------------------------------------------------
# If data is available, export variables, and submit driver for
# radiance monitoring jobs.
#--------------------------------------------------------------------

if [ -s $DATDIR/radstat.$PDATE -a -s $DATDIR/satang.$PDATE ];then
   if [ -s $DATDIR/satbias.$PDATE ]; then


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
      rm -f *anl*

      export DATDIR=$datdirl


      #--------------------------------------------------------------------
      # Export variables
      export listvar=MAKE_CTL,MAKE_DATA,RAD_AREA,MAIL_TO,MAIL_CC,DISCLAIMER,DO_DIAG_RPT,PDATE,NDATE,DATDIR,TANKDIR,LOADLQ,EXEDIR,LOGDIR,WORKDIR,SCRIPTS,USER,USER_CLASS,SUB,SUFFIX,SATYPE,NCP,ACOUNT,listvar


      #------------------------------------------------------------------
      #   Submit data processing jobs.

      rm $LOGDIR/angle.log
      $SUB -a $ACOUNT -e $listvar -j MkCtl_angle_${SUFFIX} -q dev -g ${USER_CLASS}  -t 0:10:00 -o $LOGDIR/mkctl_angle.log $SCRIPTS/verf_angle.sh

      rm $LOGDIR/bcoef.log
      $SUB -a $ACOUNT -e $listvar -j MkCtl_bcoef_${SUFFIX} -q dev  -g ${USER_CLASS}  -t 0:10:00 -o $LOGDIR/mkctl_bcoef.log $SCRIPTS/verf_bcoef.sh

      rm $LOGDIR/bcor.log
      $SUB -a $ACOUNT -e $listvar -j MkCtl_bcor_${SUFFIX} -q dev  -g ${USER_CLASS} -t 0:10:00 -o $LOGDIR/mkctl_bcor.log $SCRIPTS/verf_bcor.sh

      rm $LOGDIR/time.log
      $SUB -a $ACOUNT -e $listvar -j MkCtl_time_${SUFFIX} -q dev -g ${USER_CLASS}  -t 0:10:00 -o $LOGDIR/mkctl_time.log $SCRIPTS/verf_time.sh

   fi
else
   echo unable to locate a radstat file, check starting PDATE or DATDIR assignment.
fi

#--------------------------------------------------------------------
# Clean up and exit
#--------------------------------------------------------------------
cd $tmpdir
cd ../
rm -rf $tmpdir

echo end MkCtl_rgnl.sh
exit
