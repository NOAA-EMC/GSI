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

RADMON_SUFFIX=$1

echo RADMON_SUFFIX    = ${RADMON_SUFFIX}
RUN=${RUN:-gdas}

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

export RADMON_VERSION=${RADMON_VERSION:-${top_parm}/radmon.ver}
if [[ -s ${RADMON_VERSION} ]]; then
   . ${RADMON_VERSION}
else
   echo "Unable to source ${RADMON_VERSION} file"
   exit 2
fi

export RADMON_CONFIG=${RADMON_CONFIG:-${top_parm}/RadMon_config}

if [[ -s ${RADMON_CONFIG} ]]; then
   . ${RADMON_CONFIG}
else
   echo "Unable to source ${RADMON_CONFIG}"
   exit
fi

if [[ -s ${RADMON_USER_SETTINGS} ]]; then 
   . ${RADMON_USER_SETTINGS}
else
   echo "Unable to source ${RADMON_USER_SETTINGS}"
   exit
fi

. ${IG_PARM}/plot_rad_conf
. ${IG_PARM}/glbl_conf


#--------------------------------------------------------------------
#  Check for my monitoring use.  Abort if running on prod machine.
#--------------------------------------------------------------------

if [[ RUN_ONLY_ON_DEV -eq 1 ]]; then
   is_prod=`${IG_SCRIPTS}/onprod.sh`
   if [[ $is_prod = 1 ]]; then
      exit 10
   fi
fi

#--------------------------------------------------------------------


export PLOT=1


#--------------------------------------------------------------------
#  Create tmpdir and LOGdir
#--------------------------------------------------------------------

tmpdir=${STMP_USER}/plot_rad${RADMON_SUFFIX}
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

mkdir -p $LOGdir


#--------------------------------------------------------------------
# Get date of cycle to process.  Exit if available data has already
# been plotted ($PDATE -gt $PRODATE).
#
# If plot_time has been specified via command line argument, then 
# set PDATE to it.  Otherwise, determie the last cycle processed 
# (into *.ieee_d files) and use that as the PDATE.
#--------------------------------------------------------------------
if [[ $TANK_USE_RUN = 1 ]]; then
   export PRODATE=`${IG_SCRIPTS}/nu_find_cycle.pl --cyc 1 --dir ${TANKDIR} --run ${RUN}`
else
   export PRODATE=`${IG_SCRIPTS}/find_cycle.pl --cyc 1 --dir ${TANKDIR}`
fi

if [[ $plot_time != "" ]]; then
   export PDATE=$plot_time
else
   export PDATE=$PRODATE
fi
echo $PRODATE  $PDATE

export NUM_CYCLES=${NUM_CYCLES:-121}
hrs=`expr $NUM_CYCLES \\* -6`
echo "hrs = $hrs"

export START_DATE=`$NDATE ${hrs} $PDATE`
echo "start_date, prodate, pdate = $START_DATE $PRODATE  $PDATE"

sdate=`echo $PDATE|cut -c1-8`
export CYC=`echo $PDATE|cut -c9-10`
export PDY=`echo $PDATE|cut -c1-8`


#--------------------------------------------------------------------
#  Determine which directory structure is in use in $TANKDIR
#

if [[ $TANK_USE_RUN -eq 1 ]]; then
   ieee_src=${TANKverf}/${RUN}.${PDY}/${CYC}/${MONITOR}
   if [[ ! -d ${ieee_src} ]]; then
      ieee_src=${TANKverf}/${RUN}.${PDY}/${MONITOR}
   fi
else
   ieee_src=${TANKverf}/${MONITOR}.${PDY}
   if [[ ! -d ${ieee_src} ]]; then
      ieee_src=${TANKverf}/${RUN}.${PDY}
   fi 
fi

if [[ ! -d ${ieee_src} ]]; then
   echo "unable to set ieee_src, aborting plot"
   exit
fi


#--------------------------------------------------------------------
#  $PRODATE may be set to "auto", meaning automatically advance 
#  if data is available (data is being processed by ops or by 
#  a parallel.  We need to make sure the data for the next cycle
#  is complete.  Exit if proceed is set to "NO".
#--------------------------------------------------------------------
proceed="NO"
if [[ "$PRODATE" == "auto" ]]; then
   proceed=`${IG_SCRIPTS}/confirm_data.sh ${RADMON_SUFFIX} ${PDATE}`

elif [[ $PDATE -le $PRODATE ]]; then
   nfile_src=`ls -l ${ieee_src}/*${PDATE}*ieee_d* | egrep -c '^-'`

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



echo plot = $PLOT, plot_horiz = $PLOT_HORIZ

prev_cycle=`$NDATE -6 $PDATE`

pid=${pid:-$$}
export PLOT_WORK_DIR=${PLOT_WORK_DIR}.${pid}

if [[ -d $PLOT_WORK_DIR ]]; then
   rm -rf $PLOT_WORK_DIR
fi
mkdir $PLOT_WORK_DIR
cd $PLOT_WORK_DIR


#-------------------------------------------------------------
#  If USE_STATIC_SATYPE == 0 then assemble the SATYPE list from
#  available data files in $TANKDIR/angle
#  If USE_STATIC_SATYPE == 1 then load SATYPE from the SATYPE.txt 
#  file, or from fix/gdas_radmon_satype.txt.
#-------------------------------------------------------------
if [[ $USE_STATIC_SATYPE -eq 0 ]]; then

   test_list=`ls ${ieee_src}/angle.*${PDATE}.ieee_d*`

   for test in ${test_list}; do
      this_file=`basename $test`
      test_anl=`echo $this_file | grep "_anl"`
      if [[ $test_anl = "" ]]; then
         tmp=`echo "$this_file" | cut -d. -f2`
         echo $tmp
         SATYPE_LIST="$SATYPE_LIST $tmp"
      fi
   done

   SATYPE=$SATYPE_LIST
   echo $SATYPE

else 
   TANKDIR_INFO=${TANKDIR}/info
   export STATIC_SATYPE_FILE=${STATIC_SATYPE_FILE:-${TANKDIR_INFO}/SATYPE.txt}
   if [[ ! -e $STATIC_SATYPE_FILE ]]; then
      export STATIC_SATYPE_FILE=${HOMEgdas}/fix/gdas_radmon_satype.txt
   fi
   
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
#   Start image plotting jobs.
#------------------------------------------------------------------

${IG_SCRIPTS}/mk_angle_plots.sh

${IG_SCRIPTS}/mk_bcoef_plots.sh

if [[ ${PLOT_STATIC_IMGS} -eq 1 ]]; then
   ${IG_SCRIPTS}/mk_bcor_plots.sh
fi

if [[ ${PLOT_HORIZ} -eq 1 ]] ; then
   export datdir=${RADSTAT_LOCATION}

   jobname="plot_horiz_${RADMON_SUFFIX}"
   logfile="${LOGdir}/horiz.log"

   if [[ $MY_MACHINE = "wcoss" || $MY_MACHINE = "wcoss_d" ]]; then
      $SUB -P $PROJECT -q $JOB_QUEUE -o ${logfile} -M 80 -W 0:45 -cwd ${PWD} \
           -R affinity[core] -J ${jobname} ${IG_SCRIPTS}/mk_horiz_plots.sh
   elif [[ $MY_MACHINE = "cray" ]]; then
      $SUB -P $PROJECT -q $JOB_QUEUE -o ${logfile} -M 80 -W 0:45 -cwd ${PWD} \
           -J ${jobname} ${IG_SCRIPTS}/mk_horiz_plots.sh
   elif [[ $MY_MACHINE = "theia" ]]; then
      $SUB --account $ACCOUNT -o ${logfile} -D . -J ${jobname} --time 50 \
	   ${IG_SCRIPTS}/mk_horiz_plots.sh
   fi
fi

${IG_SCRIPTS}/mk_time_plots.sh



#------------------------------------------------------------------
#  Run the make_archive.sh script if $DO_ARCHIVE is switched on.
#------------------------------------------------------------------
if [[ $DO_ARCHIVE = 1 ]]; then
   ${IG_SCRIPTS}/nu_make_archive.sh
fi

#--------------------------------------------------------------------
#  Check for log file and extract data for error report there
#--------------------------------------------------------------------
if [[ $DO_DATA_RPT -eq 1 || $DO_DIAG_RPT -eq 1 ]]; then

   logfile=${LOGdir}/data_extract.${sdate}.${CYC}.log
  
   if [[ -s $logfile ]]; then
      ${IG_SCRIPTS}/ck_missing_diags.sh ${PDATE} ${TANKDIR}
      ${IG_SCRIPTS}/extract_err_rpts.sh ${sdate} ${CYC} ${logfile}
   fi
fi

#----------------------------------------------------------------------
#  Conditionally queue transfer to run
# 
#	None:  The $run_time is a one-hour delay to the Transfer job
#  	       to ensure the plots are all finished prior to transfer.
#----------------------------------------------------------------------
if [[ $RUN_TRANSFER -eq 1 ]]; then

   if [[ $MY_MACHINE = "wcoss" || $MY_MACHINE = "wcoss_d" ]]; then
      cmin=`date +%M`		# minute (MM)
      ctime=`date +%G%m%d%H`	# YYYYMMDDHH
      rtime=`$NDATE +1 $ctime`	# ctime + 1 hour

      rhr=`echo $rtime|cut -c9-10`
      run_time="$rhr:$cmin"	# HH:MM format for lsf (bsub command) 		

      transfer_log=${LOGdir}/Transfer_${RADMON_SUFFIX}.log

      transfer_queue=transfer
      if [[ $MY_MACHINE = "wcoss_d" ]]; then
         transfer_queue=dev_transfer
      fi

      jobname=transfer_${RADMON_SUFFIX}
      job="${IG_SCRIPTS}/Transfer.sh --nosrc --area $RAD_AREA ${RADMON_SUFFIX}"

      if [[ $TANK_USE_RUN -eq 1 ]]; then
         job="${job} --run $RUN"
      fi
      echo "job = $job"

      $SUB -P $PROJECT -q $transfer_queue -o ${transfer_log} -M 80 -W 0:45 -R affinity[core] -J ${jobname} -cwd ${PWD} -b $run_time ${job}

   else
      ${IG_SCRIPTS}/Transfer.sh ${RADMON_SUFFIX} --nosrc \
          1>/ptmpp1/Edward.Safford/logs/Transfer_${RADMON_SUFFIX}.log \
          2>/ptmpp1/Edward.Safford/logs/Transfer_${RADMON_SUFFIX}.err
   fi
fi

#--------------------------------------------------------------------
# Clean up and exit
#--------------------------------------------------------------------
cd $tmpdir
cd ../
rm -rf $tmpdir

echo "exiting CkPlt_glbl.sh"
exit
