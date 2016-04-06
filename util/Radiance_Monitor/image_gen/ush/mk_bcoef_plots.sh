#!/bin/ksh

#------------------------------------------------------------------
#
# mk_bcoef_plots.sh
#
# submit the plot jobs to make the bcoef images.
#
#------------------------------------------------------------------

set -ax
date

export NUM_CYCLES=${NUM_CYCLES:-121}

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
ndays=$(($NUM_CYCLES/4))
test_day=$PDATE

for type in ${SATYPE}; do
   found=0
   done=0
   test_day=$PDATE
   ctr=$ndays

   while [[ $found -eq 0 && $done -ne 1 ]]; do

      pdy=`echo $test_day|cut -c1-8`   

      if [[ -s ${TANKDIR}/radmon.${pdy}/bcoef.${type}.ctl.${Z} ]]; then
         $NCP ${TANKDIR}/radmon.${pdy}/bcoef.${type}.ctl.${Z} ${imgndir}/${type}.ctl.${Z}
         found=1
      elif [[ -s ${TANKDIR}/radmon.${pdy}/bcoef.${type}.ctl ]]; then
         $NCP ${TANKDIR}/radmon.${pdy}/bcoef.${type}.ctl ${imgndir}/${type}.ctl
         found=1
      fi

      if [[ $found -eq 0 ]]; then
         if [[ $ctr -gt 0 ]]; then
            test_day=`$NDATE -24 ${pdy}00`
            ctr=$(($ctr-1))
         else
            done=1
         fi
      fi
   done

   
   if [[ -s ${imgndir}/${type}.ctl.${Z} || -s ${imgndir}/${type}.ctl ]]; then
      allmissing=0
      found=1
   fi
done

if [[ $allmissing = 1 ]]; then
   echo ERROR:  Unable to plot.  All bcoef control files are missing from ${TANKDIR} for requested
 date range.
   exit
fi


#-------------------------------------------------------------------
#   Update the time definition (tdef) line in the bcoef control
#   files.  Conditionally remove any cray_32bit_ieee flags.

for type in ${SATYPE}; do
   if [[ -s ${imgndir}/${type}.ctl.${Z} ]]; then
     ${UNCOMPRESS} ${imgndir}/${type}.ctl.${Z}
   fi
   ${IG_SCRIPTS}/update_ctl_tdef.sh ${imgndir}/${type}.ctl ${START_DATE} ${NUM_CYCLES}

   if [[ $MY_MACHINE = "wcoss" || $MY_MACHINE = "zeus" || $MY_MACHINE = "theia" ]]; then
      sed -e 's/cray_32bit_ieee/ /' ${imgndir}/${type}.ctl > tmp_${type}.ctl
      mv -f tmp_${type}.ctl ${imgndir}/${type}.ctl
   fi

   ${COMPRESS} ${imgndir}/${type}.ctl
done



#-------------------------------------------------------------------
# submit plot job
#

jobname="plot_${SUFFIX}_bcoef"
logfile="$LOGdir/plot_bcoef.log"
rm ${logfile}

if [[ $MY_MACHINE = "wcoss" ]]; then
   $SUB -q $JOB_QUEUE -P $PROJECT -o ${logfile} -M 80 -W 1:15 -R affinity[core] -J ${jobname} $IG_SCRIPTS/plot_bcoef.sh
elif [[ $MY_MACHINE = "cray" ]]; then
   $SUB -q $JOB_QUEUE -P $PROJECT -o ${logfile} -M 80 -W 1:15 -J ${jobname} $IG_SCRIPTS/plot_bcoef.sh
elif [[ $MY_MACHINE = "zeus" || $MY_MACHINE = "theia" ]]; then
   $SUB -A $ACCOUNT -l procs=1,walltime=2:00:00 -N ${jobname} -V -j oe -o ${logfile} $IG_SCRIPTS/plot_bcoef.sh 
fi

exit
