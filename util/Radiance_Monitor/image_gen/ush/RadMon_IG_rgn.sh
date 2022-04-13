#!/bin/bash
#--------------------------------------------------------------------
#
#  RadMon_IG_rgn.sh
#
#  Plot image files and queue transfer job.
#
#--------------------------------------------------------------------

#--------------------------------------------------------------------
#  usage
#--------------------------------------------------------------------
function usage {
  echo "Usage:  RadMon_IG_rgn.sh suffix [-p|--pdate -r|--run ]"
  echo ""
  echo "            suffix is the indentifier for this data source."
  echo "              This is usually the same as the NET value."
  echo ""
  echo "            -p|--pdate is the full YYYYMMDDHH cycle to run.  If not specified"
  echo "              the TANKverf directory will be used to determine the next cycle time"
  echo ""
  echo "            -r|--run  nam is the default if not specified."
  echo ""
}


echo start RadMon_IG_rgn.sh


nargs=$#
if [[ $nargs -lt 1 || $nargs -gt 5 ]]; then
   usage
   exit 1
fi

#-----------------------------------------------------------
#  Process command line arguments.
#
run=nam

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
# Set environment variables
#--------------------------------------------------------------------
export RAD_AREA=rgn
export REGIONAL_RR=1

this_dir=`dirname $0`
top_parm=${this_dir}/../../parm

export RADMON_CONFIG=${RADMON_CONFIG:-${top_parm}/RadMon_config}
if [[ -s ${RADMON_CONFIG} ]]; then
   . ${RADMON_CONFIG}
else
   echo "Unable to source ${RADMON_CONFIG}"
   exit 1
fi

if [[ -s ${RADMON_USER_SETTINGS} ]]; then
   . ${RADMON_USER_SETTINGS}
else
   echo "Unable to source ${RADMON_USER_SETTINGS}"
   exit 2
fi

export PLOT_ALL_REGIONS=""


#--------------------------------------------------------------------
#  Make sure $LOGdir exists
#--------------------------------------------------------------------
echo "LOGdir = ${LOGdir}"
if [[ -e ${LOGdir} ]]; then
   mkdir -p $LOGdir
fi

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
#        data in ${TANKverf}.
#
# If option 2 has been used the last_plot_time file will be 
# updated with ${PDATE} if the plot is able to run.
#--------------------------------------------------------------------

echo "TANKimg = ${TANKimg}"
last_plot_time=${TANKimg}/last_plot_time
echo "last_plot_time file = ${last_plot_time}"

latest_data=`${IG_SCRIPTS}/find_cycle.pl --cyc 1 --dir ${TANKverf}`

if [[ ${pdate} = "" ]]; then
   if [[ -e ${last_plot_time} ]]; then
      echo " USING last_plot_time file"
      last_plot=`cat ${last_plot_time}`
      pdate=`$NDATE +${CYCLE_INTERVAL} ${last_plot}`
   else
      echo " USING find_cycle file"
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

#--------------------------------------------------------------------
#  exit if no new data is available
#
if [[ ${PDATE} -gt ${latest_data} ]]; then
   echo "requested plot date is later than available data, ${PDATE}, ${latest_data}"
   exit 5
fi


export NUM_CYCLES=${NUM_CYCLES:-120}
export START_DATE=`$NDATE -${NUM_CYCLES} $PDATE`


#--------------------------------------------------------------------
#  Note:  for REGIONAL_RR cases the 19z-00z data files are stored in
#         the next day's radmon.yyyymmdd file.  So for those cases
#         add 6 hrs to pdate and then set the $PDY value.
#
sdate=`echo $PDATE|cut -c1-8`
export CYA=`echo $PDATE|cut -c9-10`
echo "sdate, CYA = $sdate, $CYA"

if [[ $REGIONAL_RR -eq 1 ]]; then
   echo "getting date for REGIONAL_RR model"
   tdate=`$NDATE +6 $PDATE`
   export PDY=`echo $tdate|cut -c1-8`
else 
   export PDY=`echo $PDATE|cut -c1-8`
fi

satype_file=${TANKverf}/info/nam_radmon_satype.txt

if [[ ! -e $satype_file ]]; then
   satype_file=${HOMEnam}/fix/nam_radmon_satype.txt
fi
if [[ -s $satype_file ]]; then
   satype=`cat ${satype_file}`
fi

echo "satype : ${satype}"

#-------------------------------------------------------------
#  Add any satypes not in the $satype_file for which we have
#  data.  This will get us a list of satypes to plot even if
#  the $satype_file can't be found.
#
if [[ -d ${TANKverf}/radmon.${PDY} ]]; then
   test_list=`ls ${TANKverf}/radmon.${PDY}/*angle.*${PDATE}.ieee_d.*`
fi

for test in ${test_list}; do
   this_file=`basename ${test}`
   test_anl=`echo ${this_file} | grep "_anl"`

   if [[ ${test_anl} = "" ]]; then
      tmp=`echo "${this_file}" | cut -d. -f2`
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
# Set environment variables.
#
pid=${pid:-$$}
export PLOT_WORK_DIR=${STMP_USER}/${RADMON_SUFFIX}/radmon/plotjobs.${pid}
mkdir -p $PLOT_WORK_DIR
if [[ ! -d ${PLOT_WORK_DIR} ]]; then
   echo "Unable to create PLOT_WORK_DIR:  ${PLOT_WORK_DIR}"
   exit 6
fi

#------------------------------------------------------------------
#   Submit plot jobs.
#
${IG_SCRIPTS}/mk_angle_plots.sh

${IG_SCRIPTS}/mk_bcoef_plots.sh

if [[ ${PLOT_STATIC_IMGS} -eq 1 ]]; then
   ${IG_SCRIPTS}/mk_bcor_plots.sh
fi

${IG_SCRIPTS}/mk_time_plots.sh


#--------------------------------------------------------------------
#  update last_plot_time if used
#
if [[ -e ${last_plot_time} ]]; then
   echo ${PDATE} > ${last_plot_time}
fi


#----------------------------------------------------------------------
#  Conditionally queue transfer to run
#
#       None:  The $run_time is a one-hour delay to the Transfer job
#              to ensure the plots are all finished prior to transfer.
#----------------------------------------------------------------------
if [[ $RUN_TRANSFER -eq 1 ]]; then

   cyc=`echo $PDATE|cut -c9-10`
   if [[ ${cyc} = "00" || ${cyc} = "06" || ${cyc} = "12" || ${cyc} = "18" ]]; then

      if [[ $MY_MACHINE = "wcoss_c" || $MY_MACHINE = "wcoss_d" || $MY_MACHINE = "wcoss2" ]]; then
         cmin=`date +%M`           # minute (MM)
         ctime=`date +%G%m%d%H`    # YYYYMMDDHH
         rtime=`$NDATE +1 $ctime`  # ctime + 1 hour

         rhr=`echo $rtime|cut -c9-10`
         run_time="$rhr:$cmin"     # HH:MM format for lsf (bsub command)

         transfer_log=${LOGdir}/Transfer_${RADMON_SUFFIX}.log
         if [[ -e ${transfer_log} ]]; then
            rm ${transfer_log}
         fi

         transfer_queue=transfer
         if [[ $MY_MACHINE = "wcoss_d" || $MY_MACHINE = "wcoss2" ]]; then
            transfer_queue=dev_transfer
         fi

         jobname=transfer_${RADMON_SUFFIX}
         job="${IG_SCRIPTS}/Transfer.sh --nosrc ${RADMON_SUFFIX}"
         echo "job = $job"

         export WEBDIR=${WEBDIR}/regional/${RADMON_SUFFIX}/pngs

	 if [[ $MY_MACHINE = "wcoss2" ]]; then
	    cmdfile=transfer_cmd
	    echo "${IG_SCRIPTS}/Transfer.sh --nosrc ${RADMON_SUFFIX}" >$cmdfile

	    $SUB -q $transfer_queue -A $ACCOUNT -o ${transfer_log} -e ${LOGdir}/Transfer_${RADMON_SUFFIX}.err 
	         -V -l select=1:mem=500M -l walltime=45:00 -N ${jobname} ${cmdfile}

	 else
            $SUB -P $PROJECT -q $transfer_queue -o ${transfer_log} -M 80 -W 0:45 \
                 -R affinity[core] -J ${jobname} -cwd ${PWD} -b $run_time ${job}
         fi

      fi
   fi
fi


#--------------------------------------------------------------------
#  remove all but the last 30 cycles of image files.
#--------------------------------------------------------------------
${IG_SCRIPTS}/rm_img_files.pl --dir ${TANKimg}/pngs --nfl 30


echo end RadMon_IG_rgn.sh
exit
