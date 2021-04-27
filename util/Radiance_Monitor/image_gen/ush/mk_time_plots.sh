#!/bin/bash

#-------------------------------------------------------------------
#
#  script:  mk_time_plots.sh
#
#  submit plot jobs to make the time images.
#  
#-------------------------------------------------------------------

date

echo Start mk_time_plots.sh

set -ax

imgndir=${IMGNDIR}/time
tankdir=${TANKDIR}/time

if [[ ! -d ${imgndir} ]]; then
   mkdir -p ${imgndir}
fi

#-------------------------------------------------------------------
#  Locate/update the control files.  If no ctl file is available
#  report a warning to the log file.  Search order is $imgndir,
#  the $TANKDIR/radmon.$pdy, then $tankdir.
#
allmissing=1
pdy=`echo $PDATE|cut -c1-8`
cyc=`echo $PDATE|cut -c9-10`

cycdy=$((24/$CYCLE_INTERVAL))		# number cycles per day
ndays=$(($NUM_CYCLES/$cycdy))		# number days in plot period

test_day=$PDATE

for type in ${SATYPE}; do
   found=0
   finished=0
   test_day=$PDATE
   ctr=$ndays

   while [[ ${found} -eq 0 && $finished -ne 1 ]]; do

      if [[ $REGIONAL_RR -eq 1 ]]; then		# REGIONAL_RR stores hrs 18-23 in next 
         tdate=`$NDATE +6 ${test_day}`		# day's radmon.yyymmdd directory
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

      if [[ -s ${ieee_src}/time.${type}.ctl.${Z} ]]; then
         $NCP ${ieee_src}/time.${type}.ctl.${Z} ${imgndir}/${type}.ctl.${Z}
         if [[ -s ${ieee_src}/time.${type}_anl.ctl.${Z} ]]; then
            $NCP ${ieee_src}/time.${type}_anl.ctl.${Z} ${imgndir}/${type}_anl.ctl.${Z}
         fi
         found=1
      elif [[ -s ${ieee_src}/time.${type}.ctl ]]; then
         $NCP ${ieee_src}/time.${type}.ctl ${imgndir}/${type}.ctl
         if [[ -s ${ieee_src}/time.${type}_anl.ctl ]]; then
            $NCP ${ieee_src}/time.${type}_anl.ctl ${imgndir}/${type}_anl.ctl
         fi
         found=1
      fi

      if [[ ${found} -eq 0 ]]; then
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
   echo ERROR:  Unable to plot.  All time control files are missing.
   exit
fi


#-------------------------------------------------------------------
#   Update the time definition (tdef) line in the time control
#   files. 
#
for type in ${SATYPE}; do
   if [[ -s ${imgndir}/${type}.ctl.${Z} ]]; then
      ${UNCOMPRESS} ${imgndir}/${type}.ctl.${Z}
   fi
   ${IG_SCRIPTS}/update_ctl_tdef.sh ${imgndir}/${type}.ctl ${START_DATE} ${NUM_CYCLES}

   if [[ -s ${imgndir}/${type}_anl.ctl ]]; then
      ${IG_SCRIPTS}/update_ctl_tdef.sh ${imgndir}/${type}_anl.ctl ${START_DATE} ${NUM_CYCLES}
   fi
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


#-------------------------------------------------------------------
#  Summary plots
#
#    Submit the summary plot job.
#
#-------------------------------------------------------------------

jobname=plot_${RADMON_SUFFIX}_sum
logfile=${LOGdir}/plot_summary.log
rm ${logfile}

if [[ ${MY_MACHINE} = "wcoss_d" ]]; then
   ${SUB} -q ${JOB_QUEUE} -P ${PROJECT} -M 100 -R affinity[core] -o ${logfile} \
          -W 1:00 -J ${jobname} -cwd ${PWD} ${IG_SCRIPTS}/plot_summary.sh

elif [[ ${MY_MACHINE} = "wcoss_c" ]]; then
   ${SUB} -q ${JOB_QUEUE} -P ${PROJECT} -M 100 -o ${logfile} -W 1:00 \
          -J ${jobname} -cwd ${PWD} ${IG_SCRIPTS}/plot_summary.sh

elif [[ ${MY_MACHINE} = "hera" ]]; then
   ${SUB} --account ${ACCOUNT}  --ntasks=1 --mem=5g --time=1:00:00 -J ${jobname} \
          -o ${logfile} ${IG_SCRIPTS}/plot_summary.sh
fi


#-------------------------------------------------------------------
#-------------------------------------------------------------------
#  Time plots
#
#    Submit the time plot jobs.
#-------------------------------------------------------------------
#-------------------------------------------------------------------


#-------------------------------------------------------------------
#   Rename PLOT_WORK_DIR to time subdir.
#
export PLOT_WORK_DIR="${PLOT_WORK_DIR}/plottime_${RADMON_SUFFIX}"
if [ -d $PLOT_WORK_DIR ] ; then
   rm -f $PLOT_WORK_DIR
fi
mkdir -p $PLOT_WORK_DIR
cd $PLOT_WORK_DIR

list="count penalty omgnbc total omgbc"

#-------------------------------------------------------------------
#  Build command file and submit plot job for intruments not on 
#    the bigSAT list.
#

   suffix=a
   cmdfile=${PLOT_WORK_DIR}/cmdfile_ptime_${suffix}
   jobname=plot_${RADMON_SUFFIX}_tm_${suffix}
   logfile=${LOGdir}/plot_time_${suffix}.log

   rm -f $cmdfile
   rm ${logfile}

>$cmdfile

   ctr=0

   for sat in ${SATLIST}; do
      if [[ ${MY_MACHINE} = "hera" ]]; then
         echo "${ctr} $IG_SCRIPTS/plot_time.sh $sat $suffix '$list'" >> $cmdfile
      else
         echo "$IG_SCRIPTS/plot_time.sh $sat $suffix '$list'" >> $cmdfile
      fi
      ((ctr=ctr+1))
   done

   chmod 755 $cmdfile

   if [[ $PLOT_ALL_REGIONS -eq 1 || $ndays -gt 30 ]]; then
      wall_tm="2:30"
   else
      wall_tm="0:45"
   fi

   if [[ $MY_MACHINE = "wcoss_d" ]]; then
      $SUB -q $JOB_QUEUE -P $PROJECT -M 500 -R affinity[core] -o ${logfile} \
           -W ${wall_tm} -J ${jobname} -cwd ${PWD} ${cmdfile}

   elif [[ $MY_MACHINE = "hera" ]]; then
      echo "using ctr = ${ctr}"
      $SUB --account ${ACCOUNT} -n ${ctr}  -o ${logfile} -D . -J ${jobname} --time=1:00:00 \
           --wrap "srun -l --multi-prog ${cmdfile}"

   elif [[ ${MY_MACHINE} = "wcoss_c" ]]; then
      $SUB -q $JOB_QUEUE -P $PROJECT -M 500 -o ${logfile} -W ${wall_tm} \
           -J ${jobname} -cwd ${PWD} ${cmdfile}
   fi
      


#---------------------------------------------------------------------------
#  bigSATLIST
#
#    For some sat/instrument sources (airs_aqua, iasi, etc) there is so much 
#    data that a separate job for each provides a faster solution.
#   
#---------------------------------------------------------------------------
   for sat in ${bigSATLIST}; do 

      cmdfile=${PLOT_WORK_DIR}/cmdfile_ptime_${sat}
      jobname=plot_${RADMON_SUFFIX}_tm_${sat}
      logfile=${LOGdir}/plot_time_${sat}.log

      rm -f ${logfile}
      rm -f ${cmdfile}

      ctr=0 
      for var in $list; do
         if [[ ${MY_MACHINE} = "hera" ]]; then
            echo "${ctr} $IG_SCRIPTS/plot_time.sh $sat $var $var" >> $cmdfile
         else
            echo "$IG_SCRIPTS/plot_time.sh $sat $var $var" >> $cmdfile
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
         $SUB -q $JOB_QUEUE -P $PROJECT -M 500  -R affinity[core] -o ${logfile} \
              -W ${wall_tm} -J ${jobname} -cwd ${PWD} ${cmdfile}
      elif [[ ${MY_MACHINE} = "wcoss_c" ]]; then
         $SUB -q $JOB_QUEUE -P $PROJECT -M 500  -o ${logfile} -W ${wall_tm} \
              -J ${jobname} -cwd ${PWD} ${cmdfile}
      elif [[ $MY_MACHINE = "hera" ]]; then	
         $SUB --account ${ACCOUNT} -n ${ctr}  -o ${logfile} -D . -J ${jobname} --time=4:00:00 \
              --wrap "srun -l --multi-prog ${cmdfile}"
      fi

   done


echo End mk_time_plots.sh
exit
