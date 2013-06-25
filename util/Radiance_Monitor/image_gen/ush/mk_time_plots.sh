#!/bin/ksh

#-------------------------------------------------------------------
#
#  script:  mk_time_plots.sh
#
#  submit plot jobs to make the time images.
#  
#-------------------------------------------------------------------

set -ax
date

export list=$listvar
export NUM_CYCLES=${NUM_CYCLES:-121}

imgndir=${IMGNDIR}/time
tankdir=${TANKDIR}/time

if [[ ! -d ${imgndir} ]]; then
   mkdir -p ${imgndir}
fi

#
# testing
#export SATYPE="sndrd1_g15"
#export SATYPE="iasi_metop-a"

#-------------------------------------------------------------------
#  Locate/update the control files.  If no ctl file is available
#  report a warning to the log file.  Search order is $imgndir,
#  the $TANKDIR/radmon.$PDY, then $tankdir.
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
      if [[ -s ${TANKDIR}/radmon.${pdy}/time.${type}.ctl.${Z} ]]; then
         $NCP ${TANKDIR}/radmon.${pdy}/time.${type}.ctl.${Z} ${imgndir}/${type}.ctl.${Z}
         found=1
      elif [[ -s ${TANKDIR}/radmon.${pdy}/time.${type}.ctl ]]; then
         $NCP ${TANKDIR}/radmon.${pdy}/time.${type}.ctl ${imgndir}/${type}.ctl
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

#   elif [[ -s ${TANKDIR}/radmon.${PDY}/time.${type}.ctl || -s ${TANKDIR}/radmon.${PDY}/time.${type}.ctl.${Z} ]]; then
#      $NCP ${TANKDIR}/radmon.${PDY}/time.${type}.ctl.${Z} ${imgndir}/${type}.ctl.${Z}
#      if [[ ! -s ${imgndir}/${type}.ctl.${Z} ]]; then
#         $NCP ${TANKDIR}/radmon.${PDY}/time.${type}.ctl ${imgndir}/${type}.ctl
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
   echo ERROR:  Unable to plot.  All time control files are missing.
   exit
fi


#-------------------------------------------------------------------
#   Update the time definition (tdef) line in the time control
#   files.  Conditionally remove cray_32bit_ieee from the options line.
#
#   Note that the logic for the tdef in time series is backwards 
#   from angle series.  Time tdefs start at -720 from PDATE.  For
#   angle series the tdef = $PDATE and the script works backwards.
#   Some consistency on this point would be great.

   start_date=`$NDATE -720 $PDATE`

   for type in ${SATYPE}; do
      if [[ -s ${imgndir}/${type}.ctl.${Z} ]]; then
        ${UNCOMPRESS} ${imgndir}/${type}.ctl.${Z}
      fi
#      ${SCRIPTS}/update_ctl_tdef.sh ${imgndir}/${type}.ctl ${start_date}
      ${SCRIPTS}/update_ctl_tdef.sh ${imgndir}/${type}.ctl ${START_DATE} ${NUM_CYCLES}
 
      if [[ $MY_MACHINE = "wcoss" ]]; then
         sed -e 's/cray_32bit_ieee/ /' ${imgndir}/${type}.ctl > tmp_${type}.ctl
         mv -f tmp_${type}.ctl ${imgndir}/${type}.ctl
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

   export listvars=RAD_AREA,LOADLQ,PDATE,NDATE,TANKDIR,IMGNDIR,PLOT_WORK_DIR,EXEDIR,LOGDIR,SCRIPTS,GSCRIPTS,STNMAP,GRADS,GADDIR,USER,STMP_USER,PTMP_USER,USER_CLASS,SUB,SUFFIX,SATYPE,NCP,Z,COMPRESS,UNCOMPRESS,PLOT_ALL_REGIONS,SUB_AVG,listvars


#-------------------------------------------------------------------
#  Summary plots
#
#    Submit the summary plot job.
#
#-------------------------------------------------------------------

   cmdfile=${PLOT_WORK_DIR}/cmdfile_psummary
   jobname=plot_${SUFFIX}_sum
   logfile=${LOGDIR}/plot_summary.log

   rm -f $cmdfile
   rm ${logfile}

>$cmdfile
   for type in ${SATYPE}; do
      echo "$SCRIPTS/plot_summary.sh $type" >> $cmdfile
   done

   ntasks=`cat $cmdfile|wc -l `
   ((nprocs=(ntasks+1)/2))

   if [[ $MY_MACHINE = "ccs" ]]; then
      $SUB -a $ACCOUNT -e $listvar -j ${jobname} -u $USER -q dev  -g ${USER_CLASS} -t 0:30:00 -o ${logfile} $SCRIPTS/plot_summary.sh
   elif [[ $MY_MACHINE = "wcoss" ]]; then
      $SUB -q dev -R affinity[core] -o ${logfile} -W 0:45 -J ${jobname} $SCRIPTS/plot_summary.sh
   elif [[ $MY_MACHINE = "zeus" ]]; then
      $SUB -A $ACCOUNT -l procs=1,walltime=0:30:00 -N ${jobname} -v $listvar -j oe -o ${logfile} $SCRIPTS/plot_summary.sh
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
  export PLOT_WORK_DIR="${PLOT_WORK_DIR}/plot_time_${SUFFIX}"
  if [ -d $PLOT_WORK_DIR ] ; then
     rm -f $PLOT_WORK_DIR
  fi
  mkdir -p $PLOT_WORK_DIR
  cd $PLOT_WORK_DIR


#-------------------------------------------------------------------
#  Look over satellite types.  Submit plot job for each type.
#
   export listvars=RAD_AREA,LOADLQ,PDATE,NDATE,TANKDIR,IMGNDIR,PLOT_WORK_DIR,EXEDIR,LOGDIR,SCRIPTS,GSCRIPTS,STNMAP,GRADS,GADDIR,USER,STMP_USER,PTMP_USER,USER_CLASS,SUB,SUFFIX,NPREDR,NCP,Z,COMPRESS,UNCOMPRESS,PLOT_ALL_REGIONS,SUB_AVG,listvars

   list="count penalty omgnbc total omgbc"

   if [[ $MY_MACHINE = "ccs" || $MY_MACHINE = "wcoss" ]]; then		# ccs and wcoss
      suffix=a
      cmdfile=${PLOT_WORK_DIR}/cmdfile_ptime_${suffix}
      jobname=plot_${SUFFIX}_tm_${suffix}
      logfile=${LOGDIR}/plot_time_${suffix}.log

      rm -f $cmdfile
      rm ${logfile}

>$cmdfile

      for sat in ${SATLIST}; do
         echo "$SCRIPTS/plot_time.sh $sat $suffix '$list'" >> $cmdfile
      done
      chmod 755 $cmdfile

#      ((nprocs=(ntasks+1)/2))

      if [[ $MY_MACHINE = "wcoss" ]]; then   
         $SUB -q dev -R affinity[core] -o ${logfile} -W 0:45 -J ${jobname} ${cmdfile}
      else
        ntasks=`cat $cmdfile|wc -l `
        $SUB -a $ACCOUNT -e $listvars -j ${jobname} -u $USER -t 1:00:00 -o ${logfile} -p $ntasks/1/N -q dev -g {USER_CLASS} /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes -stdoutmode ordered
      fi

   else							# zeus/linux
      for sat in ${SATLIST}; do
         cmdfile=${PLOT_WORK_DIR}/cmdfile_ptime_${sat}
         jobname=plot_${SUFFIX}_tm_${sat}
         logfile=${LOGDIR}/plot_time_${sat}

         rm -f ${cmdfile}
         rm -f ${logfile}

         echo "$SCRIPTS/plot_time.sh $sat $sat '$list'" >> $cmdfile

         if [[ $PLOT_ALL_REGIONS -eq 0 ]]; then
            wall_tm="0:20:00"
         else
            wall_tm="0:40:00"
         fi

         $SUB -A $ACCOUNT -l procs=1,walltime=${wall_tm} -N ${jobname} -v $listvars -j oe -o ${logfile} $cmdfile
      done
   fi


#---------------------------------------------------------------------------
#  bigSATLIST
#
#    For some sat/instrument sources (airs_aqua, iasi, etc) there is so much 
#    data that a separate job for each provides a faster solution.
#   
#---------------------------------------------------------------------------
   for sat in ${bigSATLIST}; do 

      if [[ $MY_MACHINE = "ccs" || $MY_MACHINE = "wcoss" ]]; then	# ccs and wcoss
         cmdfile=${PLOT_WORK_DIR}/cmdfile_ptime_${sat}
         jobname=plot_${SUFFIX}_tm_${sat}
         logfile=${LOGDIR}/plot_time_${sat}.log

         rm -f ${logfile}
         rm -f ${cmdfile}
 
         list="count penalty omgnbc total omgbc"
         for var in $list; do
            echo "$SCRIPTS/plot_time.sh $sat $var $var" >> $cmdfile
         done
         chmod 755 $cmdfile

         ntasks=`cat $cmdfile|wc -l `

         if [[ $MY_MACHINE = "wcoss" ]]; then
            $SUB -q dev  -R affinity[core] -o ${logfile} -W 1:00 -J ${jobname} ${cmdfile}
         else
            $SUB -a $ACCOUNT -e $listvars -j ${jobname} -u $USER -t 1:00:00 -o ${logfile} -p $ntasks/1/N -q dev -g {USER_CLASS} /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes -stdoutmode ordered
         fi
      else						# zeus/linux
         for var in $list; do
            cmdfile=${PLOT_WORK_DIR}/cmdfile_ptime_${sat}_${var}
            jobname=plot_${SUFFIX}_tm_${sat}_${var}
            logfile=${LOGDIR}/plot_time_${sat}_${var}.log
            rm -f ${logfile}
            rm -f ${cmdfile}

            if [[ $PLOT_ALL_REGIONS -eq 0 ]]; then
               wall_tm="0:60:00"
            else
               wall_tm="2:00:00"
            fi

            echo "$SCRIPTS/plot_time.sh $sat $var $var" >> $cmdfile

            $SUB -A $ACCOUNT -l procs=1,walltime=${wall_tm} -N ${jobname} -v $listvars -j oe -o ${logfile} $cmdfile
         done
      fi
   done

exit
