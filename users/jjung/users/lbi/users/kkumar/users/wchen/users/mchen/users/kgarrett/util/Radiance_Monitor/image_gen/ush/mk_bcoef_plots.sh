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
export list=$listvar

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
#   echo "before while loop, found, done = $found, $done"

   while [[ $found -eq 0 && $done -ne 1 ]]; do
#      echo "top of while loop"

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

#   elif [[ -s ${TANKDIR}/radmon.${PDY}/bcoef.${type}.ctl || -s ${TANKDIR}/radmon.${PDY}/bcoef.${type}.ctl.${Z} ]]; then
#      $NCP ${TANKDIR}/radmon.${PDY}/bcoef.${type}.ctl.${Z} ${imgndir}/${type}.ctl.${Z}
#      if [[ ! -s ${imgndir}/${type}.ctl.${Z} ]]; then
#         $NCP ${TANKDIR}/radmon.${PDY}/bcoef.${type}.ctl ${imgndir}/${type}.ctl
#      fi
#      allmissing=0
#      found=1
#
#   elif [[ -s ${tankdir}/${type}.ctl.${Z} || -s ${tankdir}/${type}.ctl  ]]; then
#      $NCP ${tankdir}/${type}.ctl* ${imgndir}/.
#      allmissing=0
#      found=1
#
#   else
#      echo WARNING:  unable to locate ${type}.ctl
   fi
done

if [[ $allmissing = 1 ]]; then
   echo ERROR:  Unable to plot.  All bcoef control files are missing from ${TANKDIR} for requested
 date range.
   exit
fi


# TESTING
#export SATYPE="sndrd1_g15"

#-------------------------------------------------------------------
#   Update the time definition (tdef) line in the bcoef control
#   files.  Conditionally rm "cray_32bit_ieee" from the options line.
#
#   Note that the logic for the tdef in time series is backwards
#   from bcoef series.  Time tdefs start at -720 from PDATE.  For
#   bcoef series the tdef = $PDATE and the script works backwards.
#   Some consistency on this point would be great.

#start_date=`$NDATE -720 $PDATE`

for type in ${SATYPE}; do
   if [[ -s ${imgndir}/${type}.ctl.${Z} ]]; then
     ${UNCOMPRESS} ${imgndir}/${type}.ctl.${Z}
   fi
   ${SCRIPTS}/update_ctl_tdef.sh ${imgndir}/${type}.ctl ${START_DATE} ${NUM_CYCLES}

   if [[ $MY_MACHINE = "wcoss" ]]; then
      sed -e 's/cray_32bit_ieee/ /' ${imgndir}/${type}.ctl > tmp_${type}.ctl
      mv -f tmp_${type}.ctl ${imgndir}/${type}.ctl
   fi

   ${COMPRESS} ${imgndir}/${type}.ctl
done



#-------------------------------------------------------------------
# submit plot job
#

jobname="plot_${SUFFIX}_bcoef"
logfile="$LOGDIR/plot_bcoef.log"
rm ${logfile}

if [[ $MY_MACHINE = "ccs" ]]; then
   $SUB -a $ACCOUNT -e $listvar -j ${jobname} -u $USER -q dev  -g ${USER_CLASS} -t 1:00:00 -o ${logfile} $SCRIPTS/plot_bcoef.sh
elif [[ $MY_MACHINE = "wcoss" ]]; then
   $SUB -q dev -o ${logfile} -W 0:45 -R affinity[core] -J ${jobname} $SCRIPTS/plot_bcoef.sh
elif [[ $MY_MACHINE = "zeus" ]]; then
   $SUB -A $ACCOUNT -l procs=1,walltime=2:00:00 -N ${jobname} -v $listvar -j oe -o ${logfile} $SCRIPTS/plot_bcoef.sh 
fi

exit
