#!/bin/bash

#------------------------------------------------------------------
#
# mk_bcoef_plots.sh
#
# submit the plot jobs to make the bcoef images.
#
#------------------------------------------------------------------

set -ax
date
echo "begin mk_bcoef_plots.sh"

#export NUM_CYCLES=${NUM_CYCLES:-121}
#export CYCLE_INTERVAL=${CYCLE_INTERVAL:-6}
echo "NUM_CYCLES, CYCLE_INTERVAL = ${NUM_CYCLES}, ${CYCLE_INTERVAL}"

imgndir="${IMGNDIR}/bcoef"
tankdir="${TANKDIR}/bcoef"

if [[ ! -d ${imgndir} ]]; then
   mkdir -p ${imgndir}
fi

#-------------------------------------------------------------------
#  Locate/update the control files in $TANKDIR/radmon.$PDY.  $PDY
#  starts at END_DATE and walks back to START_DATE until ctl files
#  are found or we run out of dates to check.  Report an error to
#  the log file and exit if no ctl files are found.
#
allmissing=1
PDY=`echo $PDATE|cut -c1-8`
cycdy=$((24/$CYCLE_INTERVAL))
ndays=$(($NUM_CYCLES/$cycdy))

test_day=$PDATE

for type in ${SATYPE}; do
   found=0
   finished=0
   test_day=$PDATE
   ctr=$ndays

   while [[ $found -eq 0 && $finished -ne 1 ]]; do
      if [[ $REGIONAL_RR -eq 1 ]]; then         # REGIONAL_RR stores hrs 18-23 in next
         tdate=`$NDATE +6 ${test_day}`          # day's radmon.yyymmdd directory
         pdy=`echo $test_day|cut -c1-8`
      else
         pdy=`echo $test_day|cut -c1-8`
      fi

      ieee_src=${TANKverf}/${RUN}.${PDY}/${CYC}/${MONITOR}
      if [[ ! -d ${ieee_src} ]]; then
         ieee_src=${TANKverf}/${RUN}.${PDY}/${MONITOR}
      fi
      if [[ ! -d ${ieee_src} ]]; then
         ieee_src=${TANKverf}/${MONITOR}.${PDY}
      fi


      if [[ -s ${ieee_src}/bcoef.${type}.ctl.${Z} ]]; then
         $NCP ${ieee_src}/bcoef.${type}.ctl.${Z} ${imgndir}/${type}.ctl.${Z}
         if [[ -s ${ieee_src}/bcoef.${type}_anl.ctl.${Z} ]]; then
            $NCP ${ieee_src}/bcoef.${type}_anl.ctl.${Z} ${imgndir}/${type}_anl.ctl.${Z}
         fi
         found=1
      elif [[ -s ${ieee_src}/bcoef.${type}.ctl ]]; then
         $NCP ${ieee_src}/bcoef.${type}.ctl ${imgndir}/${type}.ctl
         if [[ -s ${ieee_src}/bcoef.${type}_anl.ctl ]]; then
            $NCP ${ieee_src}/bcoef.${type}_anl.ctl ${imgndir}/${type}_anl.ctl
         fi
         found=1
      fi

     if [[ $found -eq 0 ]]; then
         if [[ $ctr -gt 0 ]]; then
            test_day=`$NDATE -24 ${pdy}00`
            ctr=$(($ctr-1))
         else
            finished=1
         fi
      fi
   done

   
   if [[ -s ${imgndir}/${type}.ctl.${Z} || -s ${imgndir}/${type}.ctl ]]; then
      allmissing=0
      found=1
   fi
done

if [[ $allmissing = 1 ]]; then
   echo "ERROR:  Unable to plot.  All bcoef control files are missing from ${TANKDIR} for requested date range."
   exit 2
fi


for type in ${SATYPE}; do
   if [[ -s ${imgndir}/${type}.ctl.${Z} ]]; then
     ${UNCOMPRESS} ${imgndir}/${type}.ctl.${Z}
   fi

   ${IG_SCRIPTS}/update_ctl_tdef.sh ${imgndir}/${type}.ctl ${START_DATE} ${NUM_CYCLES}
   ${COMPRESS} ${imgndir}/${type}.ctl
done



#-------------------------------------------------------------------
# submit plot job
#

jobname="plot_${RADMON_SUFFIX}_bcoef"
logfile="$LOGdir/plot_bcoef.log"
rm ${logfile}

if [[ $MY_MACHINE = "wcoss_d" ]]; then
   $SUB -q $JOB_QUEUE -P $PROJECT -o ${logfile} -M 80 -W 1:15 \
        -R "affinity[core]" -J ${jobname} -cwd ${PWD} $IG_SCRIPTS/plot_bcoef.sh

elif [[ $MY_MACHINE = "wcoss_c" ]]; then
   $SUB -q $JOB_QUEUE -P $PROJECT -o ${logfile} -M 80 -W 1:15 \
        -J ${jobname} -cwd ${PWD} $IG_SCRIPTS/plot_bcoef.sh

elif [[ $MY_MACHINE = "hera" ]]; then
   $SUB --account $ACCOUNT --ntasks=1 --mem=5g --time=1:00:00 -J ${jobname} \
        -o ${logfile} -D . $IG_SCRIPTS/plot_bcoef.sh 
fi

echo "end mk_bcoef_plots.sh"
exit
