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
tankdir=${TANKverf}/angle

if [[ ! -d ${imgndir} ]]; then
   mkdir -p ${imgndir}
fi

#-------------------------------------------------------------------
#  Locate/update the control files in $TANKverf/radmon.$PDY.  $PDY 
#  starts at END_DATE and walks back to START_DATE until ctl files
#  are found or we run out of dates to check.  Report an error to 
#  the log file and exit if no ctl files are found. 
#
allmissing=1

cycdy=$((24/$CYCLE_INTERVAL))           # number cycles per day
ndays=$(($NUM_CYCLES/$cycdy))		# number of days in plot period

for type in ${SATYPE}; do
   found=0
   test_day=$PDATE
   ctr=$ndays
 
   while [[ ${found} -eq 0 && $ctr -gt 0 ]]; do
 
      if [[ $REGIONAL_RR -eq 1 ]]; then         # REGIONAL_RR stores hrs 18-23 in next
         tdate=`$NDATE +6 ${test_day}`          # day's radmon.yyymmdd directory
         pdy=`echo $tdate|cut -c1-8`
         cyc=`echo $tdate|cut -c9-10`
      else
         pdy=`echo $test_day|cut -c1-8`
         cyc=`echo $test_day|cut -c9-10`
      fi
 

      #---------------------------------------------------
      #  Check to see if the *ctl* files are in $imgndir
      #
      nctl=`ls ${imgndir}/${type}*ctl* -1 | wc -l`
      if [[ ( $USE_ANL -eq 1 && $nctl -ge 2 ) ||
            ( $USE_ANL -eq 0 && $nctl -ge 1 ) ]]; then
         found=1
 
      else
         #-------------------------
         #  Locate $ieee_src
         #
         ieee_src=${TANKverf}/${RUN}.${pdy}/${cyc}/${MONITOR}
         if [[ ! -d ${ieee_src} ]]; then
            ieee_src=${TANKverf}/${RUN}.${pdy}/${MONITOR}
         fi
         if [[ ! -d ${ieee_src} ]]; then
            ieee_src=${TANKverf}/${RUN}.${pdy}
         fi
         if [[ ! -d ${ieee_src} ]]; then
            ieee_src=${TANKverf}/${MONITOR}.${pdy}
         fi
 
         using_tar=0
         #--------------------------------------------------------------
         #  Determine if the angle files are in a tar file.  If so
         #  extract the ctl files for this $type.  If both a compressed
         #  and uncompressed version of the radmon_bcoef.tar file exist,
         #  report that as an error condition.
         #
         if [[ -e ${ieee_src}/radmon_angle.tar && -e ${ieee_src}/radmon_angle.tar.${Z} ]]; then
            echo "Located both radmon_angle.tar and radmon_angle.tar.${Z} in ${ieee_src}.  Unable to plot."
            exit 2

         elif [[ -e ${ieee_src}/radmon_angle.tar || -e ${ieee_src}/radmon_angle.tar.${Z} ]]; then
            using_tar=1
            ctl_list=`tar -tf ${ieee_src}/radmon_angle.tar* | grep ${type} | grep ctl`
            if [[ ${ctl_list} != "" ]]; then
               cwd=`pwd`
               cd ${ieee_src}
               tar -xf ./radmon_angle.tar* ${ctl_list}
               cd ${cwd} 
            fi
         fi
 
         #-------------------------------------------------
         #  Copy the *ctl* files to $imgndir, dropping
         #  'angle' from the file name.
         #
         ctl_files=`ls $ieee_src/angle.$type*.ctl*`
         prefix='angle.'
         for file in $ctl_files; do
            newfile=`basename $file | sed -e "s/^$prefix//"`
            $NCP ${file} ${imgndir}/${newfile}
            found=1
         done

         #----------------------------------------------------------------
         #  If there's a radmon_angle.tar archive in ${ieee_src} then
         #  delete the extracted *ctl* files to leave just the tar files.
         #
         if [[ $using_tar -eq 1 ]]; then
            rm -f ${ieee_src}/angle.${type}.ctl*
            rm -f ${ieee_src}/angle.${type}_anl.ctl*
         fi

      fi

      if [[ ${found} -eq 0 ]]; then
	 #------------------------------------------
	 #  Step to the previous day and try again.
	 #
         if [[ $ctr -gt 0 ]]; then
            test_day=`$NDATE -24 ${pdy}00`
            ctr=$(($ctr-1))
         fi
      fi
   done

   if [[ -s ${imgndir}/${type}.ctl.${Z} || -s ${imgndir}/${type}.ctl ]]; then
      allmissing=0
      found=1
   fi

done


if [[ $allmissing = 1 ]]; then
   echo ERROR:  Unable to plot.  All angle control files are missing from ${TANKverf} for requested date range.
   exit 3
fi


#-------------------------------------------------------------------
#   Update the time definition (tdef) line in the angle control 
#   files. 
# 
for sat in ${SATYPE}; do
   if [[ -s ${imgndir}/${sat}.ctl.${Z} ]]; then
     ${UNCOMPRESS} ${imgndir}/${sat}.ctl.${Z}
   fi
   ${IG_SCRIPTS}/update_ctl_tdef.sh ${imgndir}/${sat}.ctl ${START_DATE} ${NUM_CYCLES}

   #-------------------------------------------------------------------
   #  Separate the sources with a large number of channels.  These will
   #  be submitted in dedicated jobs, while the sources with a smaller
   #  number of channels will be submitted together.
   #
   nchanl=`cat ${imgndir}/${sat}.ctl | gawk '/title/{print $NF}'` 

   if [[ $nchanl -lt 100 ]]; then
      satlist=" $sat $satlist "
   else
      big_satlist=" $sat $big_satlist "
   fi

   ${COMPRESS} ${imgndir}/${sat}.ctl
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

elif [[ $MY_MACHINE = "wcoss2" ]]; then
   $SUB -q $JOB_QUEUE -A $ACCOUNT -o ${logfile} -e ${LOGdir}/plot_angle_${suffix}.err \
	-V -l select=1:mem=1g -l walltime=30:00 -N ${jobname} ${cmdfile}
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

   if [[ ${MY_MACHINE} = "wcoss_d" || $MY_MACHINE = "wcoss_c" || $MY_MACHINE = "wcoss2" ]]; then 	

      cmdfile=${PLOT_WORK_DIR}/cmdfile_pangle_${sat}
      if [[ -e ${cmdfile} ]]; then
         rm -f $cmdfile
      fi
      echo "$IG_SCRIPTS/plot_angle.sh $sat $sat ${list}" >> $cmdfile
      chmod 755 $cmdfile

      jobname=plot_${RADMON_SUFFIX}_ang_${sat}
      logfile=${LOGdir}/plot_angle_${sat}.log
      if [[ -e ${logfile} ]]; then 
         rm ${logfile}
      fi

      wall_tm="0:30"

      if [[ $MY_MACHINE = "wcoss_d" ]]; then
         $SUB -q $JOB_QUEUE -P $PROJECT -o ${logfile} -W ${wall_tm} \
              -R "affinity[core]" -R "rusage[mem=10000]" -J ${jobname} -cwd ${PWD} $cmdfile

      elif [[ $MY_MACHINE = "wcoss_c" ]]; then
         $SUB -q $JOB_QUEUE -P $PROJECT -o ${logfile} -M 600 -W ${wall_tm} \
              -J ${jobname} -cwd ${PWD} $cmdfile

      elif [[ $MY_MACHINE = "wcoss2" ]]; then
         errfile=${LOGdir}/plot_angle_${sat}.err
         if [[ -e ${errfile} ]]; then
            rm ${errfile}
         fi
         $SUB -q $JOB_QUEUE -A $ACCOUNT -o ${logfile} -e ${LOGdir}/plot_angle_${sat}.err \
              -V -l select=1:mem=1g -l walltime=30:00 -N ${jobname} ${cmdfile}
      fi

   #---------------------------------------------------
   #  hera|jet|s4, submit 1 job for each sat/list item
   elif [[ $MY_MACHINE = "hera" || $MY_MACHINE = "jet" || $MY_MACHINE = "s4" ]]; then		

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

      if [[ $MY_MACHINE = "hera" ]]; then
         $SUB --account ${ACCOUNT} -n $ii  -o ${logfile} -D . -J ${jobname} --time=4:00:00 \
              --mem=0 --wrap "srun -l --multi-prog ${cmdfile}"
      elif [[ $MY_MACHINE = "s4" ]]; then
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
