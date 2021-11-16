#!/bin/sh
#--------------------------------------------------------------------
#
#  RadMon_IG_glb.sh
#
#  RadMon image generation script for global data sources.
#  This script queues the plot and transfer scripts.
#
#--------------------------------------------------------------------


#--------------------------------------------------------------------
#  usage
#--------------------------------------------------------------------
function usage {
  echo "Usage:  RadMon_IG_glb.sh suffix [-p|--pdate -r|--run ]"
  echo ""
  echo "            suffix is the indentifier for this data source."
  echo "              This is usually the same as the NET value."
  echo ""
  echo "            -p|--pdate is the full YYYYMMDDHH cycle to run.  If not specified"
  echo "              the TANKverf directory will be used to determine the next cycle time"
  echo ""
  echo "            -r|--run is gdas|gfs.  gdas is the default if not specified."
  echo ""
}

echo start RadMon_IG_glb.sh
echo

#--------------------------------------------------------------------
#  RadMon_DE_glb begins here.
#--------------------------------------------------------------------
exit_value=0

nargs=$#
if [[ $nargs -lt 1 || $nargs -gt 5 ]]; then
   usage
   exit 1
fi

#-----------------------------------------------------------
#  Set default values and process command line arguments.
#
run=gdas

while [[ $# -ge 1 ]]
do
   key="$1"

   case $key in
      -p|--pdate)
         pdate="$2"
         shift # past argument
      ;;
      -r|--run)
         run="$2"
         shift # past argument
      ;;
      *)
         #any unspecified key is RADMON_SUFFIX
         export RADMON_SUFFIX=$key
      ;;
   esac

   shift
done

export RUN=${run}

echo "RADMON_SUFFIX = ${RADMON_SUFFIX}"
echo "RUN           = ${RUN}"
echo "pdate         = ${pdate}"

set -ax

#--------------------------------------------------------------------
# Run config files to load environment variables, 
# set default plot conditions
#--------------------------------------------------------------------
rad_area=glb

this_dir=`dirname $0`
top_parm=${this_dir}/../../parm

export RADMON_CONFIG=${RADMON_CONFIG:-${top_parm}/RadMon_config}

if [[ -s ${RADMON_CONFIG} ]]; then
   . ${RADMON_CONFIG}
else
   echo "Unable to source ${RADMON_CONFIG}"
   exit 2
fi

if [[ -s ${RADMON_USER_SETTINGS} ]]; then 
   . ${RADMON_USER_SETTINGS}
else
   echo "Unable to source ${RADMON_USER_SETTINGS}"
   exit 3
fi

#. ${IG_PARM}/plot_rad_conf
. ${IG_PARM}/glbl_conf

#--------------------------------------------------------------------
# Determine cycle to plot.  Exit if cycle is > last available
# data.
#
# PDATE can be set one of 3 ways.  This is the order of priority:
#
#   1.  Specified via command line argument
#   2.  Read from ${TANKimg}/last_plot_time file and advanced
#        one cycle.
#   3.  Using the last available cycle for which there is
#        data in ${TANKDIR}.
#
# If option 2 has been used the ${IMGNDIR}/last_plot_time file
# will be updated with ${PDATE} if the plot is able to run.
#--------------------------------------------------------------------

echo "TANKimg = ${TANKimg}"
last_plot_time=${TANKimg}/${RUN}/radmon/last_plot_time
echo "last_plot_time file = ${last_plot_time}"

latest_data=`${IG_SCRIPTS}/nu_find_cycle.pl --cyc 1 \
                           --dir ${TANKDIR} --run ${RUN}`
if [[ ${latest_data} = "" ]]; then
   latest_data=`${IG_SCRIPTS}/find_cycle.pl --cyc 1 \
                           --dir ${TANKDIR} --run ${RUN}`
fi

if [[ ${pdate} = "" ]]; then
   if [[ -e ${last_plot_time} ]]; then
      echo " USING last_plot_time file"
      last_plot=`cat ${last_plot_time}`
      pdate=`$NDATE +6 ${last_plot}`
   else
      echo " USING nu_find_cycle file"
      pdate=${latest_data}
   fi
fi


if [[ ${pdate} -gt ${latest_data} ]]; then
  echo " Unable to plot, pdate is > latest_data, ${pdate}, ${latest_data}"
  exit 4 
else
  echo " OK to plot"
fi

export PDATE=${pdate}
echo "PDATE = ${PDATE}"

#--------------------------------------------------------------------
#  Make sure $LOGdir exists
#--------------------------------------------------------------------
echo "LOGdir = ${LOGdir}"
if [[ ! -e ${LOGdir} ]]; then
   mkdir -p $LOGdir
fi

#--------------------------------------------------------------------
#  Calculate start date (first cycle to be included in the plots --
#  PDATE is the latest/last cycle in the plots. 
#--------------------------------------------------------------------
export NUM_CYCLES=${NUM_CYCLES:-121}
hrs=`expr ${NUM_CYCLES} \\* -6`
echo "hrs = $hrs"

export START_DATE=`${NDATE} ${hrs} ${PDATE}`
echo "span is start_date to pdate = ${START_DATE}, ${PDATE}"

export CYC=`echo $PDATE|cut -c9-10`
export PDY=`echo $PDATE|cut -c1-8`


#--------------------------------------------------------------------
#  Locate ieee_src in $TANKDIR and verify data files are present
#

ieee_src=${TANKverf}/${RUN}.${PDY}/${CYC}/${MONITOR}

if [[ ! -d ${ieee_src} ]]; then
   ieee_src=${TANKverf}/${RUN}.${PDY}/${MONITOR}

   if [[ ! -d ${ieee_src} ]]; then
      ieee_src=${TANKverf}/${RUN}.${PDY}
   fi
fi

if [[ ! -d ${ieee_src} ]]; then
   echo "Unable to set ieee_src, aborting plot"
   exit 5
fi

nfile_src=`ls -l ${ieee_src}/*${PDATE}*ieee_d* | egrep -c '^-'`

if [[ $nfile_src -le 0 ]]; then
   echo " Missing ieee_src files, nfile_src = ${nfile_src}, aborting plot"
   exit 6
fi

export PLOT_WORK_DIR=${PLOT_WORK_DIR}.${PDATE}

if [[ -d $PLOT_WORK_DIR ]]; then
   rm -rf $PLOT_WORK_DIR
fi
mkdir -p $PLOT_WORK_DIR
cd $PLOT_WORK_DIR


#-------------------------------------------------------------
#  Locate the satype file or set SATYPE by assembling a list 
#  from available data files in $TANKDIR/angle. 
#-------------------------------------------------------------
tankdir_info=${TANKDIR}/info
satype_file=${satype_file:-${tankdir_info}/gdas_radmon_satype.txt}
if [[ ! -e $satype_file ]]; then
   satype_file=${HOMEgdas}/fix/gdas_radmon_satype.txt
fi
echo "using satype_file: ${satype_file}"

if [[ -s ${satype_file} ]]; then
   satype=`cat ${satype_file}`
else
   echo "Unable to locate satype_file: ${satype_file}"
fi

#-------------------------------------------------------------
#  Add any satypes not in the $satype_file for which we have
#  data.  This will get us a list of satypes to plot even if
#  the $satype_file can't be found.
#
test_list=`ls ${ieee_src}/angle.*${PDATE}.ieee_d*`

for test in ${test_list}; do
   this_file=`basename $test`
   test_anl=`echo $this_file | grep "_anl"`

   if [[ $test_anl = "" ]]; then
      tmp=`echo "$this_file" | cut -d. -f2`
      test_satype=`echo ${satype} | grep ${tmp}`

      if [[ ${test_satype} = "" ]]; then
         satype="${satype} ${tmp}" 
         echo "added ${tmp} to satype"
      fi
   fi
done

export SATYPE=${satype}
echo $SATYPE


#------------------------------------------------------------------
#   Start plot scripts.
#------------------------------------------------------------------
${IG_SCRIPTS}/mk_time_plots.sh

${IG_SCRIPTS}/mk_bcoef_plots.sh

${IG_SCRIPTS}/mk_angle_plots.sh

if [[ ${PLOT_STATIC_IMGS} -eq 1 ]]; then
   ${IG_SCRIPTS}/mk_bcor_plots.sh
fi

#--------------------------------------------------------------------
#  Check for log file and extract data for error report there
#--------------------------------------------------------------------
if [[ $DO_DATA_RPT -eq 1 || $DO_DIAG_RPT -eq 1 ]]; then

   warn_file=${TANKDIR}/${RUN}.${PDY}/${CYC}/${MONITOR}/warning.${PDATE}
   if [[ -e ${warn_file} ]]; then
      echo "mailing warning report"
      /bin/mail -s "RadMon warning" -c "${MAIL_CC}" ${MAIL_TO} < ${warn_file}
   fi

fi

#--------------------------------------------------------------------
#  Update the last_plot_time file if found
#--------------------------------------------------------------------
if [[ -e ${last_plot_time} ]]; then
   echo "update last_plot_time file"
   echo ${PDATE} > ${last_plot_time}
fi


#--------------------------------------------------------------------
#  Remove all but the last 30 cycles worth of data image files.
#--------------------------------------------------------------------
${IG_SCRIPTS}/rm_img_files.pl --dir ${TANKimg}/radmon/pngs --nfl 30



#----------------------------------------------------------------------
#  Conditionally queue transfer to run
# 
#	None:  The $run_time is a one-hour delay to the Transfer job
#  	       to ensure the plots are all finished prior to transfer.
#----------------------------------------------------------------------
if [[ $RUN_TRANSFER -eq 1 ]]; then

   if [[ $MY_MACHINE = "wcoss_c" || $MY_MACHINE = "wcoss_d" ]]; then
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
      job="${IG_SCRIPTS}/Transfer.sh --nosrc --area ${rad_area} ${RADMON_SUFFIX}"

      if [[ $TANK_USE_RUN -eq 1 ]]; then
         job="${job} --run $RUN"
      fi
      echo "job = $job"

      $SUB -P $PROJECT -q $transfer_queue -o ${transfer_log} -M 80 -W 0:45 -R affinity[core] -J ${jobname} -cwd ${PWD} -b $run_time ${job}

   fi
fi


echo "exiting RadMon_IG_glb.sh"
exit
