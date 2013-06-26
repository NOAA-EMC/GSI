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
  echo "Usage:  CkPlt_glbl.sh suffix [plot_date]"
  echo "            File name for CkPlt_glbl.sh may be full or relative path"
  echo "            Suffix is data source identifier that matches data in "
  echo "              the $TANKDIR/stats directory."
  echo "            Plot_date, format YYYYMMDDHH is optional.  If included the plot" 
  echo "              will be for the specified cycle, provided data files are available."
  echo "              If not included, the plot cycle will be for the latest cycle found"
  echo "              for this suffix."
}

set -ax
echo start CkPlt_glbl.sh
echo
echo $PATH
echo

nargs=$#
if [[ $nargs -lt 1 || $nargs -gt 2 ]]; then
   usage
   exit 1
fi

this_file=`basename $0`
this_dir=`dirname $0`

SUFFIX=$1

echo SUFFIX    = ${SUFFIX}

#
#  Temporary change, abort if running on prod.  This is an attempt to
#  eliminate prossible crons running on the prod machine for me only.
#
#   machine=`hostname | cut -c1`
#   prod=`cat /etc/prod | cut -c1`
#
#   if [[ $machine = $prod ]]; then
#      exit 10
#   fi
#
#  End temporary change
#


#--------------------------------------------------------------------
#  Set plot_time if it is included as an argument.
#--------------------------------------------------------------------
plot_time=
if [[ $nargs -eq 2 ]]; then
   plot_time=$2
fi

#--------------------------------------------------------------------
# Run config files to load environment variables, 
# set default plot conditions
#--------------------------------------------------------------------
RAD_AREA=glb

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
. ${RADMON_IMAGE_GEN}/parm/glbl_conf

export PLOT=1
export PLOT_HORIZ=0


#--------------------------------------------------------------------
# Check status of plot jobs. If any are still running then exit
# this script. If none are running then remove any old job records 
# in the $LOADLQ directory.
#
# Also need to check verf jobs for suffix. Don't want to run until
# all verf jobs have been completed.
#--------------------------------------------------------------------

if [[ $MY_MACHINE = "ccs" ]]; then
   running=`llq -u ${LOGNAME} -f %jn | grep plot_ | grep _${SUFFIX} | wc -l`
elif [[ $MY_MACHINE = "wcoss" ]]; then
   running=`bjobs -l | grep plot_${SUFFIX} | wc -l` 
else
   running=`showq -n -u ${LOGNAME} | grep plot_${SUFFIX} | wc -l`
fi

if [[ $running -ne 0 ]]; then
   echo "Plot jobs still running for $SUFFIX, must exit"
   exit
fi


#--------------------------------------------------------------------
#  Create tmpdir and LOGDIR
#--------------------------------------------------------------------

tmpdir=${STMP_USER}/plot_rad${SUFFIX}
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

mkdir -p $LOGDIR


#--------------------------------------------------------------------
# Get date of cycle to process.  Exit if available data has already
# been plotted ($PDATE -gt $PRODATE).
#
# If plot_time has been specified via command line argument, then 
# set PDATE to it.  Otherwise, use the IMGDATE from the DATA_MAP file
# and add 6 hrs to determine the next cycle.
#--------------------------------------------------------------------
export PRODATE=`${SCRIPTS}/find_last_cycle.pl ${TANKDIR}`

if [[ $plot_time != "" ]]; then
   export PDATE=$plot_time
else
   export PDATE=$PRODATE
fi
export START_DATE=`$NDATE -720 $PDATE`
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
   nfile_src=`ls -l ${TANKDIR}/radmon.${PDY}/*${PDATE}*ieee_d* | egrep -c '^-'` 
   if [[ $nfile_src -gt 0 ]]; then
      proceed="YES"
   fi
fi
echo proceed = $proceed

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

#SATYPE="iasi_metop-a"
#------------------------------------------------------------------
# Export variables
#------------------------------------------------------------------
export listvar=RAD_AREA,PDATE,NDATE,START_DATE,TANKDIR,IMGNDIR,LOADLQ,EXEDIR,LOGDIR,SCRIPTS,GSCRIPTS,STNMAP,GRADS,USER,PTMP_USER,STMP_USER,USER_CLASS,SUB,SUFFIX,SATYPE,NCP,PLOT_WORK_DIR,ACCOUNT,DATA_MAP,Z,COMPRESS,UNCOMPRESS,PTMP,STMP,TIMEX,LITTLE_ENDIAN,PLOT_ALL_REGIONS,SUB_AVG,listvar

#------------------------------------------------------------------
#   Start image plotting jobs.
#------------------------------------------------------------------
${SCRIPTS}/mk_angle_plots.sh

${SCRIPTS}/mk_bcoef_plots.sh

${SCRIPTS}/mk_bcor_plots.sh

if [[ ${PLOT_HORIZ} -eq 1 ]] ; then
   export datdir=${RADSTAT_LOCATION}

   export listvar=PARM,RAD_AREA,PDATE,NDATE,START_DATE,TANKDIR,IMGNDIR,LOADLQ,EXEDIR,LOGDIR,SCRIPTS,GSCRIPTS,STNMAP,GRADS,USER,PTMP_USER,STMP_USER,USER_CLASS,SUB,SUFFIX,SATYPE,NCP,PLOT_WORK_DIR,ACCOUNT,RADMON_PARM,DATA_MAP,Z,COMPRESS,UNCOMPRESS,PTMP,STMP,TIMEX,LITTLE_ENDIAN,PLOT_ALL_REGIONS,datdir,MY_MACHINE,listvar
   jobname="plot_horiz_${SUFFIX}"
   logfile="${LOGDIR}/horiz.log"
   if [[ $MY_MACHINE = "ccs" ]]; then
      $SUB -a $ACCOUNT -e $listvar -j ${jobname} -q dev -g ${USER_CLASS} -t 0:20:00 -o ${logfile} ${SCRIPTS}/mk_horiz_plots.sh
   elif [[ $MY_MACHINE = "wcoss" ]]; then
      $SUB -q dev -o ${logfile} -W 0:45 -J ${jobname} ${SCRIPTS}/mk_horiz_plots.sh
   else
      $SUB -A $ACCOUNT -l procs=1,walltime=0:20:00 -N ${jobname} -v $listvar -j oe -o ${logfile} $SCRIPTS/mk_horiz_plots.sh
   fi
fi

${SCRIPTS}/mk_time_plots.sh

#------------------------------------------------------------------
#  Run the plot_update.sh script if no $plot_time was specified on 
#  the command line
#------------------------------------------------------------------
if [[ $plot_time = "" ]]; then
   ${SCRIPTS}/plot_update.sh
fi

#--------------------------------------------------------------------
#  Check for log file and extract data for error report there
#--------------------------------------------------------------------
if [[ $DO_DATA_RPT -eq 1 || $DO_DIAG_RPT -eq 1 ]]; then

   logfile=${LOGSverf_rad}/rad${SUFFIX}/data_extract.${sdate}.${CYA}.log
  
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
