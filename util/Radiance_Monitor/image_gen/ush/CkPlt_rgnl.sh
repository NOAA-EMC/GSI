#!/bin/sh
#--------------------------------------------------------------------
#
#  CkPlt_rgnl.sh
#
#  Check for new data and if found call scripts to plot image
#  files and transfer new images to web server.
#
#  Note:  this does not generate any data files.  Those must be
#  already created for this script to function correctly.
#
#--------------------------------------------------------------------

function usage {
  echo "Usage:  CkPlt_rgnl.sh suffix [plot_date]"
  echo "            File name for CkPlt_rgnl.sh may be full or relative path"
  echo "            Suffix is data source identifier that matches data in"
  echo "              TANKDIR/stats directory."
  echo "            Plot_date, format YYYYMMDDHH is optional.  If included the plot"
  echo "              will be for the specified cycle, provided data files are available."
  echo "              If not included, the plot cycle will be the last processed cycle."
}


set -ax
echo start CkPlt_rgnl.sh



nargs=$#
if [[ $nargs -lt 1 || $nargs -gt 2 ]]; then
   usage
   exit 1
fi


this_file=`basename $0`
this_dir=`dirname $0`

SUFFIX=$1
echo SUFFIX    = ${SUFFIX}

#--------------------------------------------------------------------
#  Set plot_time if it is included as an argument.
#--------------------------------------------------------------------
plot_time=
if [[ $nargs -eq 2 ]]; then
   plot_time=$2
fi


#--------------------------------------------------------------------
# Set environment variables
#--------------------------------------------------------------------
export RAD_AREA=rgn
export PLOT_ALL_REGIONS=

top_parm=${this_dir}/../../parm

if [[ -s ${top_parm}/RadMon_config ]]; then
   . ${top_parm}/RadMon_config
else
   echo "Unable to source ${top_parm}/RadMon_config"
   exit
fi
if [[ -s ${top_parm}/RadMon_user_settings ]]; then
   . ${top_parm}/RadMon_user_settings
else
   echo "Unable to source ${top_parm}/RadMon_user_settings"
   exit
fi

. ${RADMON_IMAGE_GEN}/parm/plot_rad_conf
. ${RADMON_IMAGE_GEN}/parm/rgnl_conf


#--------------------------------------------------------------------
#  Check setting of RUN_ONLY_ON_DEV and possible abort if on prod and
#  not permitted to run there.
#--------------------------------------------------------------------

if [[ RUN_ONLY_ON_DEV -eq 1 ]]; then
   is_prod=`${SCRIPTS}/AmIOnProd.sh`
   if [[ $is_prod = 1 ]]; then
      exit 10
   fi
fi

#--------------------------------------------------------------------


tmpdir=${STMP_USER}/plot_rgnl_rad${SUFFIX}
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

export PLOT=0
export PLOT_HORIZ=0

mkdir -p $LOGDIR


#--------------------------------------------------------------------
# Check status of plot jobs.  If any are still running then exit
# this script.  If none are running then remove any old job records
# in the $LOADLQ directory.
#--------------------------------------------------------------------

running=0
if [[ $MY_MACHINE = "wcoss" ]]; then
   running=`bjobs -l | grep plot_${SUFFIX} | wc -l`
else
   running=`showq -n -u ${LOGNAME} | grep plot_${SUFFIX} | wc -l`
fi

if [[ $running -ne 0 ]]; then
   echo plot jobs still running for $SUFFIX, must exit
   cd $tmpdir
   cd ../
   rm -rf $tmpdir
   exit
fi



#--------------------------------------------------------------------
# Get date of cycle to process.  Exit if images are up to current
# $PRODATE.
#
# If plot_time has been specified via command line argument, then
# set PDATE to it.  Otherwise, use the IMGDATE from the DATA_MAP file
# and add 6 hrs to determine the next cycle.
#--------------------------------------------------------------------
export PRODATE=`${SCRIPTS}/find_cycle.pl 1 ${TANKDIR}`

if [[ $plot_time != "" ]]; then
   export PDATE=$plot_time
else
   export PDATE=$PRODATE
fi
export START_DATE=`$NDATE -720 $PDATE`
echo $PRODATE  $PDATE


#--------------------------------------------------------------------
#  exit if no new data is available

if [[ $PDATE -gt $PRODATE ]]; then
   cd $tmpdir
   cd ../
   rm -rf $tmpdir
   exit
fi


#--------------------------------------------------------------------
#  Plot all but horizontal data with each cycle.  Plot horizontal
#  data on 00z cycle. 

export CYA=`echo $PDATE|cut -c9-10`
export PLOT=1

if [[ "$CYA" = "00" ]];then
   export PLOT_HORIZ=1
fi


#--------------------------------------------------------------------
# If data is available, export variables, and submit driver for
# radiance monitoring jobs.

if [[ $PLOT -eq 1 ]]; then

   if [[ $USE_STATIC_SATYPE -eq 0 ]]; then
      PDY=`echo $PDATE|cut -c1-8`

      if [[ -d ${TANKDIR}/radmon.${PDY} ]]; then
         test_list=`ls ${TANKDIR}/radmon.${PDY}/angle.*${PDATE}.ieee_d*`
      else
         test_list=`ls ${TANKDIR}/angle/*.${PDATE}.ieee_d*`
      fi

      for test in ${test_list}; do
         this_file=`basename $test`
         tmp=`echo "$this_file" | cut -d. -f2`
         echo $tmp
         SATYPE_LIST="$SATYPE_LIST $tmp"
      done

      SATYPE=$SATYPE_LIST

   else
      TANKDIR_INFO=${TANKDIR}/info
      STATIC_SATYPE_FILE=${TANKDIR_INFO}/SATYPE.txt

      #-------------------------------------------------------------
      #  Load the SATYPE list from the STATIC_SATYPE_FILE or exit
      #  if unable to locate it.
      #-------------------------------------------------------------
      if [[ -s $STATIC_SATYPE_FILE ]]; then
         SATYPE=""
         SATYPE=`cat ${STATIC_SATYPE_FILE}`
         echo $SATYPE
      else
         echo Unable to locate $STATIC_SATYPE_FILE, must exit.
         cd $tmpdir
         cd ../
         rm -rf $tmpdir
         exit
      fi
 
   fi

 
  #------------------------------------------------------------------
  # Set environment variables.

  export PLOT_WORK_DIR=${STMP_USER}/plotjobs_${SUFFIX}
  mkdir -p $PLOT_WORK_DIR
  cd $PLOT_WORK_DIR


  #--------------------------------------------------------------------
  #   Set environment variables to export to subsequent scripts
  if [[ $MY_MACHINE = "wcoss" ]]; then
     `module load GrADS/2.0.1`
     echo GADDIR = $GADDIR
  fi
  export datdir=$RADSTAT_LOCATION


  #------------------------------------------------------------------
  #   Submit plot jobs.

  if [[ $PLOT_HORIZ -eq 1 ]]; then
     logfile=${LOGDIR}/mk_horiz_plots.log
     rm ${logfile}

     jobname=mk_plot_horiz_${SUFFIX}
     if [[ $MY_MACHINE = "wcoss" ]]; then
        $SUB -q $JOB_QUEUE -P $PROJECT -M 80 -R affinity[core]  -o ${logfile} -W 0:45 -J ${jobname} ${SCRIPTS}/mk_horiz_plots.sh
     else
        $SUB -A $ACCOUNT -l procs=1,walltime=0:20:00 -N ${jobname} -V -j oe -o $LOGDIR/mk_horiz_plots.log $SCRIPTS/mk_horiz_plots.sh
     fi
  fi

  ${SCRIPTS}/mk_angle_plots.sh

  ${SCRIPTS}/mk_bcoef_plots.sh

  ${SCRIPTS}/mk_bcor_plots.sh

  ${SCRIPTS}/mk_time_plots.sh

  #------------------------------------------------------------------
  #  Run the make_archive.sh script if $DO_ARCHIVE is switched on.
  #------------------------------------------------------------------
  if [[ $DO_ARCHIVE = 1 ]]; then
     ${SCRIPTS}/make_archive.sh
  fi

fi

#--------------------------------------------------------------------
# Clean up and exit
cd $tmpdir
cd ../
rm -rf $tmpdir

exit
