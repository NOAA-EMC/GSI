#!/bin/ksh

#------------------------------------------------------------------
#
#  mk_bcor_plots.sh
#
#  Submit plot jobs to make the bcor images.
#
#  Log:
#   08/2010  safford  initial coding (adapted from bcor.sh).
#------------------------------------------------------------------

set -ax
date

echo "begin mk_bcor_plots.sh"

imgndir=${IMGNDIR}/bcor
tankdir=${TANKDIR}/bcor

if [[ ! -d ${imgndir} ]]; then
   mkdir -p ${imgndir}
fi


#-------------------------------------------------------------------
#  Locate/update the control files in $TANKDIR/radmon.$pdy.  $pdy
#  starts at END_DATE and walks back to START_DATE until ctl files
#  are found or we run out of dates to check.  Report an error to
#  the log file and exit if no ctl files are found.
#
allmissing=1
pdy=`echo $PDATE|cut -c1-8`
cyc=`echo $PDATE|cut -c9-10`

cycdy=$((24/$CYCLE_INTERVAL))           # number cycles per day
ndays=$(($NUM_CYCLES/$cycdy))           # number days in plot period

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

      ieee_src=${TANKverf}/${RUN}.${pdy}/${cyc}/${MONITOR}
      if [[ ! -d ${ieee_src} ]]; then
         ieee_src=${TANKverf}/${RUN}.${pdy}/${MONITOR}
      fi
      if [[ ! -d ${ieee_src} ]]; then
         ieee_src=${TANKverf}/${MONITOR}.${pdy}
      fi
      if [[ ! -d ${ieee_src} ]]; then
         ieee_src=${TANKverf}/${RUN}.${pdy}
      fi
      if [[ ! -d ${ieee_src} ]]; then
         echo "Unable to locate ieee_src directory, exiting mk_time_plots.sh."
         exit 12
      fi


      if [[ -s ${ieee_src}/bcor.${type}.ctl.${Z} ]]; then
         $NCP ${ieee_src}/bcor.${type}.ctl.${Z} ${imgndir}/${type}.ctl.${Z}
         if [[ -s ${ieee_src}/bcor.${type}_anl.ctl.${Z} ]]; then
            $NCP ${ieee_src}/bcor.${type}_anl.ctl.${Z} ${imgndir}/${type}_anl.ctl.${Z}
         fi
         found=1
      elif [[ -s ${ieee_src}/bcor.${type}.ctl ]]; then
         $NCP ${ieee_src}/bcor.${type}.ctl ${imgndir}/${type}.ctl
         if [[ -s ${ieee_src}/bcor.${type}_anl.ctl ]]; then
            $NCP ${ieee_src}/bcor.${type}_anl.ctl ${imgndir}/${type}_anl.ctl
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
   echo ERROR:  Unable to plot.  All bcor control files are missing.
   exit 14
fi


#-------------------------------------------------------------------
#   Update the time definition (tdef) line in the bcor control
#   files.

for type in ${SATYPE}; do
   if [[ -s ${imgndir}/${type}.ctl.${Z} ]]; then
     ${UNCOMPRESS} ${imgndir}/${type}.ctl.${Z}
   fi
   ${IG_SCRIPTS}/update_ctl_tdef.sh ${imgndir}/${type}.ctl ${START_DATE} ${NUM_CYCLES}

done

for sat in ${SATYPE}; do
   nchanl=`cat ${imgndir}/${sat}.ctl | gawk '/title/{print $NF}'`
   if [[ $nchanl -ge 100 ]]; then
      bigSATLIST=" $sat $bigSATLIST "      
   else         
      SATLIST=" $sat $SATLIST "
   fi
done

${COMPRESS} ${imgndir}/*.ctl


#------------------------------------------------------------------
#   Submit plot jobs
#
plot_list="count total fixang lapse lapse2 const scangl clw cos sin emiss ordang4 ordang3 ordang2 ordang1"

export PLOT_WORK_DIR=${PLOT_WORK_DIR}/plotbcor_${RADMON_SUFFIX}
if [[ -d ${PLOT_WORK_DIR} ]]; then 
   rm -f ${PLOT_WORK_DIR}
fi
mkdir -p ${PLOT_WORK_DIR}
cd ${PLOT_WORK_DIR}


#-------------------------------------------------------------------------
# Loop over satellite/instruments.  Submit poe job to make plots.  Each task handles
# a single satellite/insrument.

suffix=a
cmdfile=cmdfile_pbcor_${suffix}
jobname=plot_${RADMON_SUFFIX}_bcor_${suffix}
logfile=${LOGdir}/plot_bcor_${suffix}.log

rm -f ${cmdfile}
rm -f ${logfile}
>$cmdfile

ctr=0
for sat in ${SATLIST}; do
   if [[ $MY_MACHINE = "hera" || $MY_MACHINE = "jet" || $MY_MACHINE = "s4" ]]; then
      echo "${ctr} $IG_SCRIPTS/plot_bcor.sh $sat $suffix '$plot_list'" >> $cmdfile
   else   
      echo "$IG_SCRIPTS/plot_bcor.sh $sat $suffix '$plot_list'" >> $cmdfile
   fi
   ((ctr=ctr+1))
done

chmod 755 $cmdfile


if [[ $PLOT_ALL_REGIONS -eq 1 || $ndays -gt 30 ]]; then
   wall_tm="1:30"
else
   wall_tm="0:45"
fi

if [[ $MY_MACHINE = "wcoss_d" ]]; then
   $SUB -q $JOB_QUEUE -P $PROJECT -M 80 -R affinity[core] -o ${logfile} \
        -W ${wall_tm} -J ${jobname} -cwd ${PWD} ./$cmdfile

elif [[ $MY_MACHINE = "wcoss_c" ]]; then
   $SUB -q $JOB_QUEUE -P $PROJECT -M 80 -o ${logfile} -W ${wall_tm} \
        -J ${jobname} -cwd ${PWD} ./$cmdfile

elif [[ $MY_MACHINE = "hera" || $MY_MACHINE = "s4" ]]; then
   $SUB --account ${ACCOUNT} -n $ctr  -o ${logfile} -D . -J ${jobname} \
        --time=2:00:00 --wrap "srun -l --multi-prog ${cmdfile}"

elif [[ $MY_MACHINE = "jet" ]]; then
   $SUB --account ${ACCOUNT} -n $ctr  -o ${logfile} -D . -J ${jobname} \
        -p ${RADMON_PARTITION} --time=2:00:00 --wrap "srun -l --multi-prog ${cmdfile}"

fi


#--------------------------------------------------------------------------
#  bigSATLIST
#  
#    Some satellite/instrument sources have so many channels that a separate
#    job to handle each plot type is the fastest solution.
#
#--------------------------------------------------------------------------
for sat in ${bigSATLIST}; do
   echo "processing $sat"
   suffix=$sat

   cmdfile=cmdfile_pbcor_${suffix}
   jobname=plot_${RADMON_SUFFIX}_bcor_${suffix}
   logfile=${LOGdir}/plot_bcor_${suffix}.log

   rm -f $cmdfile
   rm ${logfile}
>$cmdfile

   ctr=0
   for var in $plot_list; do
      if [[ $MY_MACHINE = "hera" || $MY_MACHINE = "jet" || $MY_MACHINE = "s4" ]]; then
         echo "$ctr $IG_SCRIPTS/plot_bcor.sh $sat $var $var" >> $cmdfile
      else
         echo "$IG_SCRIPTS/plot_bcor.sh $sat $var $var" >> $cmdfile
      fi
      ((ctr=ctr+1))
   done

   chmod 755 $cmdfile

   if [[ $PLOT_ALL_REGIONS -eq 1 || $ndays -gt 30 ]]; then
      wall_tm="2:30"
   else
      wall_tm="1:00"
   fi

   if [[ $MY_MACHINE = "wcoss_d" ]]; then
      $SUB -q $JOB_QUEUE -P $PROJECT -M 80 -R affinity[core] -o ${logfile} \
           -W ${wall_tm} -J ${jobname} -cwd ${PWD} ./$cmdfile

   elif [[ $MY_MACHINE = "wcoss_c" ]]; then      
      $SUB -q $JOB_QUEUE -P $PROJECT -M 80 -o ${logfile} -W ${wall_tm} \
           -J ${jobname} -cwd ${PWD} ./$cmdfile

   elif [[ $MY_MACHINE = "hera" || $MY_MACHINE = "s4" ]]; then
      $SUB --account ${ACCOUNT} -n $ctr  -o ${logfile} -D . -J ${jobname} \
           --time=1:00:00 --wrap "srun -l --multi-prog ${cmdfile}"

   elif [[ $MY_MACHINE = "jet" ]]; then
      $SUB --account ${ACCOUNT} -n $ctr  -o ${logfile} -D . -J ${jobname} \
           -p ${RADMON_PARTITION} --time=1:00:00 --wrap "srun -l --multi-prog ${cmdfile}"

   fi

   echo "submitted $sat"
done


echo "end mk_bcor_plots.sh"
exit 0
