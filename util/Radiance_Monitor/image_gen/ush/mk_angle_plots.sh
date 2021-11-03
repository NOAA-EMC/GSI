#!/bin/bash

#------------------------------------------------------------------
#
#  mk_angle_plots.sh
#
#  submit the plot jobs to create the angle images.
#
#  Log:
#   08/2010  safford  initial coding (adapted from angle.sh).
#------------------------------------------------------------------

date

echo ""
echo "Begin mk_angle_plots.sh"
echo ""

set -ax
export CYCLE_INTERVAL=${CYCLE_INTERVAL:-6}

echo "START_DATE, CYCLE_INTERVAL = ${START_DATE}, ${CYCLE_INTERVAL}"

imgndir=${IMGNDIR}/angle
tankdir=${TANKDIR}/angle

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

cycdy=$((24/$CYCLE_INTERVAL))           # number cycles per day
ndays=$(($NUM_CYCLES/$cycdy))		# number of days in plot period

echo SATYPE=$SATYPE

for type in ${SATYPE}; do
   found=0
   finished=0
   ctr=$ndays
   test_day=$PDATE

   while [[ $found -eq 0 && $finished -ne 1 ]]; do

      if [[ $REGIONAL_RR -eq 1 ]]; then         # REGIONAL_RR stores hrs 18-23 in next 
         tdate=`$NDATE +6 ${test_day}`          # day's radmon.yyymmdd directory
         pdy=`echo $test_day|cut -c1-8`
         ieee_src=${TANKverf}/radmon.${pdy}
      else
         pdy=`echo $test_day|cut -c1-8`    
         ieee_src=${TANKverf}/${RUN}.${pdy}/${CYC}/${MONITOR}
         if [[ ! -d ${ieee_src} ]]; then
            ieee_src=${TANKverf}/${RUN}.${pdy}/${MONITOR}
         fi
         if [[ ! -d ${ieee_src} ]]; then
            ieee_src=${TANKverf}/${RUN}.${pdy}
         fi
      fi

      echo "ieee_src =  with pdy = $pdy"

      if [[ -s ${ieee_src}/angle.${type}.ctl.${Z} ]]; then
         $NCP ${ieee_src}/angle.${type}.ctl.${Z} ${imgndir}/${type}.ctl.${Z}
         if [[ -s ${ieee_src}/angle.${type}_anl.ctl.${Z} ]]; then
            $NCP ${ieee_src}/angle.${type}_anl.ctl.${Z} ${imgndir}/${type}_anl.ctl.${Z}
         fi 
         found=1

      elif [[ -s ${ieee_src}/angle.${type}.ctl ]]; then
         $NCP ${ieee_src}/angle.${type}.ctl ${imgndir}/${type}.ctl
         if [[ -s ${ieee_src}/angle.${type}_anl.ctl ]]; then
            $NCP ${ieee_src}/angle.${type}_anl.ctl ${imgndir}/${type}_anl.ctl
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
   echo ERROR:  Unable to plot.  All angle control files are missing from ${TANKDIR} for requested date range.
   exit 2
fi


#-------------------------------------------------------------------
#   Update the time definition (tdef) line in the angle control 
#   files. 
# 
for type in ${SATYPE}; do
   if [[ -s ${imgndir}/${type}.ctl.${Z} ]]; then
     ${UNCOMPRESS} ${imgndir}/${type}.ctl.${Z}
   fi
   ${IG_SCRIPTS}/update_ctl_tdef.sh ${imgndir}/${type}.ctl ${START_DATE} ${NUM_CYCLES}
done


#-------------------------------------------------------------------
#  Separate the sources with a large number of channels.  These will
#  be submitted in dedicated jobs, while the sources with a smaller
#  number of channels will be submitted together.
#
for sat in ${SATYPE}; do
   nchanl=`cat ${imgndir}/${sat}.ctl | gawk '/title/{print $NF}'` 
   if [[ $nchanl -lt 100 ]]; then
      satlist=" $sat $satlist "
   else
      big_satlist=" $sat $big_satlist "
   fi
done

echo ""
echo " satlist: ${satlist}"
echo ""
echo " big_satlist: ${big_satlist}"
echo ""


#-------------------------------------------------------------------
#   Rename PLOT_WORK_DIR to angle subdir.
#
export PLOT_WORK_DIR="${PLOT_WORK_DIR}/plotangle_${RADMON_SUFFIX}"

if [[ -d $PLOT_WORK_DIR ]]; then
   rm -f $PLOT_WORK_DIR
fi
mkdir -p $PLOT_WORK_DIR
cd $PLOT_WORK_DIR


#-----------------------------------------------------------------
# Loop over satellite types.  Submit job to make plots.
#
list="count penalty omgnbc total omgbc fixang lapse lapse2 const scangl clw cos sin emiss ordang4 ordang3 ordang2 ordang1"

suffix=a
cmdfile=${PLOT_WORK_DIR}/cmdfile_pangle_${suffix}
jobname=plot_${RADMON_SUFFIX}_ang_${suffix}
logfile=${LOGdir}/plot_angle_${suffix}.log

rm -f ${cmdfile}
rm -f ${logfile}

rm ${LOGdir}/plot_angle_${suffix}.log

#--------------------------------------------------
# sbatch (slurm) requires a line number added
# to the cmdfile
ctr=0
for type in ${satlist}; do

   if [[ ${MY_MACHINE} = "hera" || ${MY_MACHINE} = "jet" || ${MY_MACHINE} = "s4" ]]; then
      echo "${ctr} ${IG_SCRIPTS}/plot_angle.sh ${type} ${suffix} '${list}'" >> ${cmdfile}
   else
      echo "${IG_SCRIPTS}/plot_angle.sh ${type} ${suffix} '${list}'" >> ${cmdfile}
   fi
   ((ctr=ctr+1))
done

chmod 755 ${cmdfile}
echo "CMDFILE:  ${cmdfile}"

ntasks=`cat $cmdfile|wc -l `
wall_tm="0:20"

if [[ ${MY_MACHINE} = "wcoss_d" ]]; then
   $SUB -q $JOB_QUEUE -P $PROJECT -o ${logfile} -M 500 -W ${wall_tm} \
        -R "affinity[core]" -J ${jobname} -cwd ${PWD} $cmdfile

elif [[ ${MY_MACHINE} = "hera" || ${MY_MACHINE} = "s4" ]]; then
   $SUB --account ${ACCOUNT} -n $ctr  -o ${logfile} -D . -J ${jobname} --time=30:00 \
        --wrap "srun -l --multi-prog ${cmdfile}"

elif [[ ${MY_MACHINE} = "jet" ]]; then
   $SUB --account ${ACCOUNT} -n $ctr  -o ${logfile} -D . -J ${jobname} --time=30:00 \
        -p ${RADMON_PARTITION} --wrap "srun -l --multi-prog ${cmdfile}"

elif [[ ${MY_MACHINE} = "wcoss_c" ]]; then
   $SUB -q $JOB_QUEUE -P $PROJECT -o ${logfile} -M 600 -W ${wall_tm} \
        -J ${jobname} -cwd ${PWD} $cmdfile
fi



#----------------------------------------------------------------------------
#  big_satlist
#   
#    There is so much data for some sat/instrument sources that a separate 
#    job for each is necessary.
#   
echo "starting big_satlist"


for sat in ${big_satlist}; do
   echo processing $sat in $big_satlist

   if [[ ${MY_MACHINE} = "wcoss_d" || $MY_MACHINE = "wcoss_c" ]]; then 	

      cmdfile=${PLOT_WORK_DIR}/cmdfile_pangle_${sat}
      if [[ -e ${cmdfile} ]]; then
         rm -f $cmdfile
      fi
      echo "$IG_SCRIPTS/plot_angle.sh $sat $sat ${list}" >> $cmdfile
      chmod 755 $cmdfile

      jobname=plot_${RADMON_SUFFIX}_ang_${sat}
      logfile=${LOGdir}/plot_angle_${sat}.log
      ntasks=`cat $cmdfile|wc -l `
      echo "ntasks = $ntasks"

      wall_tm="0:30"

      if [[ $MY_MACHINE = "wcoss_d" ]]; then
         mem="12000"
         $SUB -q $JOB_QUEUE -P $PROJECT -o ${logfile} -M ${mem} -W ${wall_tm} \
              -R "affinity[core]" -J ${jobname} -cwd ${PWD} $cmdfile

      elif [[ $MY_MACHINE = "wcoss_c" ]]; then
         $SUB -q $JOB_QUEUE -P $PROJECT -o ${logfile} -M 600 -W ${wall_tm} \
              -J ${jobname} -cwd ${PWD} $cmdfile
      fi


   elif [[ $MY_MACHINE = "hera" || $MY_MACHINE = "jet" || $MY_MACHINE = "s4" ]]; then		# hera|jet|s4, submit 1 job for each sat/list item

      ii=0
      logfile=${LOGdir}/plot_angle_${sat}.log
      cmdfile=${PLOT_WORK_DIR}/cmdfile_pangle_${sat}
      rm -f $cmdfile

      logfile=${LOGdir}/plot_angle_${sat}.log
      jobname=plot_${RADMON_SUFFIX}_ang_${sat}

      while [[ $ii -le ${#list[@]}-1 ]]; do
         echo "${ii} ${IG_SCRIPTS}/plot_angle.sh $sat $sat ${list[$ii]}" >> $cmdfile
         (( ii=ii+1 ))
      done

      if [[ ! $MY_MACHINE = "jet" ]]; then
         $SUB --account ${ACCOUNT} -n $ii  -o ${logfile} -D . -J ${jobname} --time=4:00:00 \
              --wrap "srun -l --multi-prog ${cmdfile}"
      else
         $SUB --account ${ACCOUNT} -n $ii  -o ${logfile} -D . -J ${jobname} --time=4:00:00 \
              -p ${RADMON_PARTITION} --wrap "srun -l --multi-prog ${cmdfile}"
      fi

   fi

done


echo ""
echo "End mk_angle_plots.sh"
echo ""
echo ""

exit
