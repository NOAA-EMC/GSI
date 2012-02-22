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
  echo "Usage:  CkPlt_rgnl.sh suffix run_envir"
  echo "            File name for CkPlt_rgnl.sh may be full or relative path"
  echo "            Suffix is data source identifier that matches data in"
  echo "              TANKDIR/stats directory."
  echo "            The run_envir maybe dev, para, or prod."
}


set -ax
echo start CkPlt_rgnl.sh

nargs=$#
if [[ $nargs -ne 2 ]]; then
   usage
   exit 1
fi

this_file=`basename $0`
this_dir=`dirname $0`

SUFFIX=$1
RUN_ENVIR=$2

echo SUFFIX    = ${SUFFIX}
echo RUN_ENVIR = ${RUN_ENVIR}


#--------------------------------------------------------------------
# Set environment variables
#--------------------------------------------------------------------
export RAD_AREA=rgn

top_parm=${this_dir}/../../parm

if [[ -s ${top_parm}/RadMon_config ]]; then
   . ${top_parm}/RadMon_config
else
   echo "Unable to source ${top_parm}/RadMon_config"
   exit
fi

. ${RADMON_IMAGE_GEN}/parm/plot_rad_conf
. ${RADMON_IMAGE_GEN}/parm/rgnl_conf

tmpdir=${STMP_USER}/plot_rgnl_rad${SUFFIX}
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

export PLOT=0
export PLOT_HORIZ=0

mkdir -p $LOGDIR

rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

#--------------------------------------------------------------------
# Check status of plot jobs.  If any are still running then exit
# this script.  If none are running then remove any old job records
# in the $LOADLQ directory.
#--------------------------------------------------------------------

count=`ls ${LOADLQ}/plot*_${SUFFIX}* | wc -l`
complete=`grep "COMPLETED" ${LOADLQ}/plot*_$SUFFIX* | wc -l`

running=`expr $count - $complete`

if [[ $running -ne 0 ]]; then
   echo plot jobs still running for $SUFFIX, must exit
   cd $tmpdir
   cd ../
   rm -rf $tmpdir
   exit
else
   rm -f ${LOADLQ}/*plot*_${SUFFIX}*
fi

running=0
count=`ls ${LOADLQ}/verf*_${SUFFIX}* | wc -l`
complete=`grep "COMPLETED" ${LOADLQ}/verf*_$SUFFIX* | wc -l`

running=`expr $count - $complete`
if [[ $running -ne 0 ]]; then
   echo verf jobs still running for $SUFFIX, must exit
   cd $tmpdir
   cd ../
   rm -rf $tmpdir
   exit
fi

#--------------------------------------------------------------------
# Get date of cycle to process.  Exit if images are up to current
# $PRODATE.
#--------------------------------------------------------------------

export PRODATE=`${SCRIPTS}/get_prodate.sh ${SUFFIX} ${DATA_MAP}`
export IMGDATE=`${SCRIPTS}/get_imgdate.sh ${SUFFIX} ${DATA_MAP}`

export PDATE=`$NDATE +6 $IMGDATE`
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

   echo $USE_STATIC_SATYPE
   export USE_STATIC_SATYPE=`${SCRIPTS}/get_satype.sh ${SUFFIX} ${DATA_MAP}`
   if [[ $USE_STATIC_SATYPE -eq 0 ]]; then

      test_list=`ls $TANKDIR/angle/*.${PDATE}.ieee_d*`

      for test in ${test_list}; do
         this_file=`basename $test`
         tmp=`echo "$this_file" | cut -d. -f1`
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

   echo $SATYPE

  #------------------------------------------------------------------
  # Set environment variables.

  export PLOT_WORK_DIR=${STMP_USER}/plotjobs_${SUFFIX}
  mkdir -p $PLOT_WORK_DIR
  cd $PLOT_WORK_DIR


  #--------------------------------------------------------------------
  #   Set environment variables to export to subsequent scripts

  export WEBDIRL=${PTMP_USER}/stats/regional/${SUFFIX}
  export FIXDIR=/nwprod/fix

  export listvar=RAD_AREA,LOADLQ,PDATE,NDATE,TANKDIR,IMGNDIR,PLOT_WORK_DIR,WEB_SVR,WEB_USER,WEBDIR,WEBDIRL,EXEDIR,FIXDIR,LOGDIR,SCRIPTS,GSCRIPTS,STNMAP,GRADS,USER,U_USER,PTMP_USER,STMP_USER,USER_CLASS,SUB,SUFFIX,FIXANG,SATYPE,NCP,PLOT,ACOUNT,RADMON_DATA_EXTRACT,listvar


  #------------------------------------------------------------------
  #   Submit plot jobs.

  if [[ $PLOT_HORIZ -eq 1 ]]; then
     logfile=${LOGDIR}/mk_horiz_plots.log
     rm ${logfile}

     jobname=mk_plot_horiz_${SUFFIX}
     ${SUB} -a ${ACOUNT} -e ${listvar} -j ${jobname} -q dev -g ${USER_CLASS} -t 0:20:00 -o ${logfile} ${SCRIPTS}/mk_horiz_plots.sh ${SUFFIX} ${PDATE}
  fi

  ${SCRIPTS}/mk_angle_plots.sh

  ${SCRIPTS}/mk_bcoef_plots.sh

  ${SCRIPTS}/mk_bcor_plots.sh

  ${SCRIPTS}/mk_time_plots.sh

  ${SCRIPTS}/plot_update.sh

fi

#--------------------------------------------------------------------
# Clean up and exit
cd $tmpdir
cd ../
rm -rf $tmpdir

exit
