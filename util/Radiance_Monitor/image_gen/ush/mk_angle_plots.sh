#!/bin/ksh

#------------------------------------------------------------------
#
#  mk_angle_plots.sh
#
#  submit the plot jobs to create the angle images.
#
#  Log:
#   08/2010  safford  initial coding (adapted from angle.sh).
#------------------------------------------------------------------

set -ax
date

export NUM_CYCLES=${NUM_CYCLES:-121}

imgndir=${IMGNDIR}/angle
tankdir=${TANKDIR}/angle

if [[ ! -d ${imgndir} ]]; then
   mkdir -p ${imgndir}
fi

echo Z = $Z

#-------------------------------------------------------------------
#  Locate/update the control files in $TANKDIR/radmon.$PDY.  $PDY 
#  starts at END_DATE and walks back to START_DATE until ctl files
#  are found or we run out of dates to check.  Report an error to 
#  the log file and exit if no ctl files are found. 
#
allmissing=1
PDY=`echo $PDATE|cut -c1-8`
ndays=$(($NUM_CYCLES/4))

for type in ${SATYPE}; do
   found=0
   done=0
   test_day=$PDATE
   ctr=$ndays

   while [[ $found -eq 0 && $done -ne 1 ]]; do

      pdy=`echo $test_day|cut -c1-8`    
      if [[ -s ${TANKDIR}/radmon.${pdy}/angle.${type}.ctl.${Z} ]]; then
         $NCP ${TANKDIR}/radmon.${pdy}/angle.${type}.ctl.${Z} ${imgndir}/${type}.ctl.${Z}
         found=1
      elif [[ -s ${TANKDIR}/radmon.${pdy}/angle.${type}.ctl ]]; then
         $NCP ${TANKDIR}/radmon.${pdy}/angle.${type}.ctl ${imgndir}/${type}.ctl
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
   echo ERROR:  Unable to plot.  All angle control files are missing from ${TANKDIR} for requested date range.
   exit
fi

#-------------------------------------------------------------------
#   Update the time definition (tdef) line in the angle control 
#   files. Conditionally rm "cray_32bit_ieee" from the options line.
 
for type in ${SATYPE}; do
   if [[ -s ${imgndir}/${type}.ctl.${Z} ]]; then
     ${UNCOMPRESS} ${imgndir}/${type}.ctl.${Z}
   fi
   ${IG_SCRIPTS}/update_ctl_tdef.sh ${imgndir}/${type}.ctl ${START_DATE} ${NUM_CYCLES}

   if [[ $MY_MACHINE = "wcoss" || $MY_MACHINE = "zeus" || $MY_MACHINE = "theia" ]]; then
      sed -e 's/cray_32bit_ieee/ /' ${imgndir}/${type}.ctl > tmp_${type}.ctl
      mv -f tmp_${type}.ctl ${imgndir}/${type}.ctl
   fi

done


for sat in ${SATYPE}; do
   nchanl=`cat ${imgndir}/${sat}.ctl | gawk '/title/{print $NF}'` 
   if [[ $nchanl -lt 100 ]]; then
      SATLIST=" $sat $SATLIST "
   else
      bigSATLIST=" $sat $bigSATLIST "
   fi
done

${COMPRESS} -f ${imgndir}/*.ctl


#-------------------------------------------------------------------
#   Rename PLOT_WORK_DIR to angle subdir.
#
export PLOT_WORK_DIR="${PLOT_WORK_DIR}/plotangle_${SUFFIX}"

if [[ -d $PLOT_WORK_DIR ]]; then
   rm -f $PLOT_WORK_DIR
fi
mkdir -p $PLOT_WORK_DIR
cd $PLOT_WORK_DIR


  #-----------------------------------------------------------------
  # Loop over satellite types.  Submit job to make plots.
  #

list="count penalty omgnbc total omgbc fixang lapse lapse2 const scangl clw cos sin emiss ordang4 ordang3 ordang2 ordang1"

  if [[ ${MY_MACHINE} = "wcoss" ]]; then
     suffix=a
     cmdfile=${PLOT_WORK_DIR}/cmdfile_pangle_${suffix}
     jobname=plot_${SUFFIX}_ang_${suffix}
     logfile=$LOGdir/plot_angle_${suffix}.log

     rm -f $cmdfile
     rm -f $logfile

     rm $LOGdir/plot_angle_${suffix}.log
#>$cmdfile
     for type in ${SATLIST}; do
       echo "$IG_SCRIPTS/plot_angle.sh $type $suffix '$list'" >> $cmdfile
     done
     chmod 755 $cmdfile

     ntasks=`cat $cmdfile|wc -l `

     if [[ $PLOT_ALL_REGIONS -eq 1 || $ndays -gt 30 ]]; then
        wall_tm="2:30"
     else
        wall_tm="1:45"
     fi

     $SUB -q $JOB_QUEUE -P $PROJECT -o ${logfile} -M 10000 -W ${wall_tm} -R affinity[core] -J ${jobname} $cmdfile

  else				# Zeus/linux platform
     for sat in ${SATLIST}; do
        suffix=${sat} 
        cmdfile=${PLOT_WORK_DIR}/cmdfile_pangle_${suffix}
        jobname=plot_${SUFFIX}_ang_${suffix}
        logfile=${LOGdir}/plot_angle_${suffix}.log

        rm -f $cmdfile
        rm -f $logfile

        echo "$IG_SCRIPTS/plot_angle.sh $sat $suffix '$list'" >> $cmdfile

        if [[ $PLOT_ALL_REGIONS -eq 1 || $ndays -gt 30 ]]; then
           wall_tm="5:00:00"
        else
           wall_tm="2:30:00"
        fi

        $SUB -A $ACCOUNT -l procs=1,walltime=${wall_tm} -N ${jobname} -V -j oe -o ${logfile} ${cmdfile}
     done
  fi



  #----------------------------------------------------------------------------
  #  bigSATLIST
  #   
  #    There is so much data for some sat/instrument sources that a separate 
  #    job for each is necessary.
  #   

echo "starting $bigSATLIST"

set -A list count penalty omgnbc total omgbc fixang lapse lapse2 const scangl clw cos sin emiss ordang4 ordang3 ordang2 ordang1

for sat in ${bigSATLIST}; do
   echo processing $sat in $bigSATLIST

   #
   #  CCS submit 4 jobs for each $sat
   #
   if [[ $MY_MACHINE = "wcoss" ]]; then 	
      batch=1
      ii=0

      suffix="${sat}_${batch}"
      cmdfile=${PLOT_WORK_DIR}/cmdfile_pangle_${suffix}
      rm -f $cmdfile
      jobname=plot_${SUFFIX}_ang_${suffix}
      logfile=${LOGdir}/plot_angle_${suffix}.log

      while [[ $ii -le ${#list[@]}-1 ]]; do

         echo "$IG_SCRIPTS/plot_angle.sh $sat $suffix ${list[$ii]}" >> $cmdfile
         ntasks=`cat $cmdfile|wc -l `
         chmod 755 $cmdfile

         if [[ $PLOT_ALL_REGIONS -eq 1 || $ndays -gt 30 ]]; then
            wall_tm="3:00"
         else
            wall_tm="1:00"
         fi

        
         mem="6000"
         if [[ $batch -eq 1 ]]; then
            mem="100000"
         fi

         $SUB -q $JOB_QUEUE -P $PROJECT -o ${logfile} -M ${mem} -W ${wall_tm} -R affinity[core] -J ${jobname} $cmdfile

         (( batch=batch+1 ))

         suffix="${sat}_${batch}"
         cmdfile=${PLOT_WORK_DIR}/cmdfile_pangle_${suffix}
         rm -f $cmdfile
         jobname=plot_${SUFFIX}_ang_${suffix}
         logfile=${LOGdir}/plot_angle_${suffix}.log

         (( ii=ii+1 ))
      done

   else					# Zeus, submit 1 job for each sat/list item

      ii=0
      suffix="${sat}"

      while [[ $ii -le ${#list[@]}-1 ]]; do
         cmdfile=${PLOT_WORK_DIR}/cmdfile_pangle_${suffix}_${list[$ii]}
         rm -f $cmdfile
         logfile=${LOGdir}/plot_angle_${suffix}_${list[$ii]}.log
         jobname=plot_${SUFFIX}_ang_${suffix}_${list[$ii]}

         echo "${IG_SCRIPTS}/plot_angle.sh $sat $suffix ${list[$ii]}" >> $cmdfile

         if [[ $PLOT_ALL_REGIONS -eq 1 || $ndays -gt 30 ]]; then
            wall_tm="5:00:00"
         else
            wall_tm="2:30:00"
         fi

         $SUB -A $ACCOUNT -l procs=1,walltime=${wall_tm} -N ${jobname} -V -j oe -o ${logfile} ${cmdfile}

         (( ii=ii+1 ))
      done
  fi

done


exit
