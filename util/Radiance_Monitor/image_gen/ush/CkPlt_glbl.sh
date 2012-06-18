#!/bin/sh
#--------------------------------------------------------------------
#
#  CkPlt_glbl.sh
#
#  Check for new global data and if found call scripts to plot image 
#  files and transfer new images to web server.  
#
#  Note:  this does not generate any data files.  Those must be 
#  already created for this script to function correctly.
#
#--------------------------------------------------------------------



function usage {
  echo "Usage:  CkPlt_glbl.sh suffix"
  echo "            File name for CkPlt_glbl.sh may be full or relative path"
  echo "            Suffix is data source identifier that matches data in "
  echo "              the $TANKDIR/stats directory."
}

set -ax
echo start CkPlt_glbl.sh

nargs=$#
if [[ $nargs -ne 1 ]]; then
   usage
   exit 1
fi

this_file=`basename $0`
this_dir=`dirname $0`

SUFFIX=$1
#RUN_ENVIR=$2

echo SUFFIX    = ${SUFFIX}
#echo RUN_ENVIR = ${RUN_ENVIR}


#--------------------------------------------------------------------
# Set environment variables
#--------------------------------------------------------------------
RAD_AREA=glb

top_parm=${this_dir}/../../parm

if [[ -s ${top_parm}/RadMon_config ]]; then
   . ${top_parm}/RadMon_config
else
   echo "Unable to source ${top_parm}/RadMon_config"
   exit
fi

. ${RADMON_IMAGE_GEN}/parm/plot_rad_conf
. ${RADMON_IMAGE_GEN}/parm/glbl_conf

tmpdir=${STMP_USER}/plot_rad${SUFFIX}
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

export PLOT=1
export PLOT_HORIZ=0

mkdir -p $LOGDIR


#--------------------------------------------------------------------
# Check status of plot jobs. If any are still running then exit
# this script. If none are running then remove any old job records 
# in the $LOADLQ directory.
#
# Also need to check verf jobs for suffix. Don't want to run until
# all verf jobs have been completed.
#--------------------------------------------------------------------

count=`ls ${LOADLQ}/plot*_$SUFFIX* | wc -l`
complete=`grep "COMPLETED" ${LOADLQ}/plot*_$SUFFIX* | wc -l`

running=`expr $count - $complete`

if [[ $running -ne 0 ]]; then
   echo plot jobs still running for $SUFFIX, must exit
   cd $tmpdir
   cd ../
   rm -rf $tmpdir
   exit
else
   rm -f ${LOADLQ}/plot*_${SUFFIX}*
fi

running=0
count=`ls ${LOADLQ}/verf*_$SUFFIX* | wc -l`
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
# Get date of cycle to process.  Exit if available data has already
# been plotted ($PDATE -gt $PRODATE).
#--------------------------------------------------------------------
export PRODATE=`${SCRIPTS}/query_data_map.pl ${DATA_MAP} ${SUFFIX} prodate`
export IMGDATE=`${SCRIPTS}/query_data_map.pl ${DATA_MAP} ${SUFFIX} imgdate`

export PDATE=`$NDATE +6 $IMGDATE`

echo $PRODATE  $PDATE

sdate=`echo $PDATE|cut -c1-8`
export CYA=`echo $PDATE|cut -c9-10`
export PDY=`echo $PDATE|cut -c1-8`


#--------------------------------------------------------------------
#  $PRODATE may be set to "auto", meaning automatically advance 
#  if data is available (data is being processed by ops or by 
#  a parallel.  We need to make sure the data for the next cycle
#  is complete.  Exit if proceed is set to "NO".
#--------------------------------------------------------------------
proceed="NO"
if [[ "$PRODATE" == "auto" ]]; then
   proceed=`${SCRIPTS}/confirm_data.sh ${SUFFIX} ${PDATE}`
elif [[ $PDATE -le $PRODATE ]]; then
   proceed="YES"
fi
echo $proceed

if [[ "$proceed" != "YES" ]]; then
   cd $tmpdir
   cd ../
   rm -rf $tmpdir
   exit
fi

#--------------------------------------------------------------------
# Make horizontal plots only on 00z cycle.  All other plotting
# is done with each cycle. 
#--------------------------------------------------------------------
if [[ "$CYA" = "00" ]];then
   export PLOT_HORIZ=1
fi

echo plot = $PLOT, plot_horiz = $PLOT_HORIZ


prev_cycle=`$NDATE -6 $PDATE`

if [[ -d $PLOT_WORK_DIR ]]; then
   rm -rf $PLOT_WORK_DIR
fi
mkdir $PLOT_WORK_DIR
cd $PLOT_WORK_DIR


export USE_STATIC_SATYPE=`${SCRIPTS}/query_data_map.pl ${DATA_MAP} ${SUFFIX} static_satype`
export ACOUNT=`${SCRIPTS}/query_data_map.pl ${DATA_MAP} ${SUFFIX} account`
export RUN_ENVIR=`${SCRIPTS}/query_data_map.pl ${DATA_MAP} ${SUFFIX} run_envir`
export USER_CLASS=`${SCRIPTS}/query_data_map.pl ${DATA_MAP} ${SUFFIX} user_class`


#-------------------------------------------------------------
#  If USE_STATIC_SATYPE == 0 then assemble the SATYPE list from
#  available data files in $TANKDIR/angle
#  If USE_STATIC_SATYPE == 1 then load SATYPE from the SATYPE.txt 
#  file.
#-------------------------------------------------------------
if [[ $USE_STATIC_SATYPE -eq 0 ]]; then

   if [[ -d ${TANKDIR}/radmon.${PDY} ]]; then
      test_list=`ls ${TANKDIR}/radmon.${PDY}/angle.*${PDATE}.ieee_d*`
      for test in ${test_list}; do
         this_file=`basename $test`
         tmp=`echo "$this_file" | cut -d. -f2`
         echo $tmp
         SATYPE_LIST="$SATYPE_LIST $tmp"
      done
   else
      test_list=`ls ${TANKDIR}/angle/*.${PDATE}.ieee_d*`
      for test in ${test_list}; do
         this_file=`basename $test`
         tmp=`echo "$this_file" | cut -d. -f1`
         echo $tmp
         SATYPE_LIST="$SATYPE_LIST $tmp"
      done
   fi
   
   SATYPE=$SATYPE_LIST
   echo $SATYPE

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
# Export variables
#------------------------------------------------------------------
export listvar=PARM,RAD_AREA,PDATE,NDATE,TANKDIR,IMGNDIR,LOADLQ,LLQ,WEB_SVR,WEB_USER,WEBDIR,EXEDIR,LOGDIR,SCRIPTS,GSCRIPTS,STNMAP,GRADS,USER,PTMP_USER,STMP_USER,USER_CLASS,SUB,SUFFIX,SATYPE,NCP,PLOT_WORK_DIR,ACOUNT,RADMON_PARM,DATA_MAP,listvar


#------------------------------------------------------------------
#   Start image plotting jobs.
#------------------------------------------------------------------
${SCRIPTS}/mk_angle_plots.sh

${SCRIPTS}/mk_bcoef_plots.sh

${SCRIPTS}/mk_bcor_plots.sh

if [[ ${PLOT_HORIZ} -eq 1 ]] ; then
  $SUB -a $ACOUNT -e $listvar -j plot_horiz_${SUFFIX} -q dev -g ${USER_CLASS} -t 0:20:00 -o $LOGDIR/horiz.log ${SCRIPTS}/mk_horiz_plots.sh ${SUFFIX} ${PDATE}
fi

${SCRIPTS}/mk_time_plots.sh

${SCRIPTS}/plot_update.sh

#--------------------------------------------------------------------
#  Check for log file and extract data for error report there
#--------------------------------------------------------------------
do_diag_rpt=`${SCRIPTS}/query_data_map.pl ${DATA_MAP} ${SUFFIX} do_diag_rpt`
do_data_rpt=`${SCRIPTS}/query_data_map.pl ${DATA_MAP} ${SUFFIX} do_data_rpt`

if [[ $do_data_rpt -eq 1 || $do_diag_rpt -eq 1 ]]; then
   export MAIL_TO=`${SCRIPTS}/query_data_map.pl ${DATA_MAP} ${SUFFIX} mail_to`
   export MAIL_CC=`${SCRIPTS}/query_data_map.pl ${DATA_MAP} ${SUFFIX} mail_cc`

   logfile_dir=`${SCRIPTS}/query_data_map.pl ${DATA_MAP} ${SUFFIX} logfile_dir`

   logfile=`ls ${logfile_dir}/${PDY}/gdas_verfrad_${CYA}.*`
   if [[ ! -s $logfile ]]; then
      logfile=${LOGDIR}/data_extract.${sdate}.${CYA}.log
   fi
  
   if [[ -s $logfile ]]; then
      ${SCRIPTS}/extract_err_rpts.sh $sdate $CYA $logfile
   fi
fi

#--------------------------------------------------------------------
# Clean up and exit
#--------------------------------------------------------------------
cd $tmpdir
cd ../
rm -rf $tmpdir

echo end CkPlt_glbl.sh
exit
