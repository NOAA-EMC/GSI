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
for type in ${SATYPE}; do
   found=0

   if [[ -s ${imgndir}/${type}.ctl.${Z} || -s ${imgndir}/${type}.ctl ]]; then
      allmissing=0
      found=1

   elif [[ -s ${TANKDIR}/radmon.${PDY}/time.${type}.ctl || -s ${TANKDIR}/radmon.${PDY}/time.${type}.ctl.${Z} ]]; then
      $NCP ${TANKDIR}/radmon.${PDY}/time.${type}.ctl.${Z} ${imgndir}/${type}.ctl.${Z}
      if [[ ! -s ${imgndir}/${type}.ctl.${Z} ]]; then
         $NCP ${TANKDIR}/radmon.${PDY}/time.${type}.ctl ${imgndir}/${type}.ctl
      fi
      allmissing=0
      found=1

   elif [[ -s ${tankdir}/${type}.ctl.${Z} || -s ${tankdir}/${type}.ctl  ]]; then
      $NCP ${tankdir}/${type}.ctl* ${imgndir}/.
      allmissing=0
      found=1

   else
      echo WARNING:  unable to locate ${type}.ctl
   fi
done

if [[ $allmissing = 1 ]]; then
   echo ERROR:  Unable to plot.  All control files are missing.
   exit
fi


#-------------------------------------------------------------------
#   Update the time definition (tdef) line in the time control
#   files.
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
      ${SCRIPTS}/update_ctl_tdef.sh ${imgndir}/${type}.ctl ${start_date}
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

   export listvars=RAD_AREA,LOADLQ,PDATE,NDATE,TANKDIR,IMGNDIR,PLOT_WORK_DIR,WEB_SVR,WEB_USER,WEBDIR,EXEDIR,LOGDIR,SCRIPTS,GSCRIPTS,STNMAP,GRADS,USER,STMP_USER,PTMP_USER,USER_CLASS,SUB,SUFFIX,SATYPE,NCP,Z,COMPRESS,UNCOMPRESS,PLOT_ALL_REGIONS,listvars


#-------------------------------------------------------------------
#  Summary plots
#
#    Submit the summary plot job.
#
#-------------------------------------------------------------------

   cmdfile=${PLOT_WORK_DIR}/cmdfile_psummary
   jobname=plot_${SUFFIX}_sum

   rm -f $cmdfile
   rm $LOGDIR/plot_summary.log

>$cmdfile
   for type in ${SATYPE}; do
      echo "$SCRIPTS/plot_summary.sh $type" >> $cmdfile
   done

   ntasks=`cat $cmdfile|wc -l `
   ((nprocs=(ntasks+1)/2))

   if [[ $MY_OS = "aix" ]]; then
      $SUB -a $ACCOUNT -e $listvar -j ${jobname} -u $USER -q dev  -g ${USER_CLASS} -t 0:30:00 -o $LOGDIR/plot_summary.log $SCRIPTS/plot_summary.sh
   else
      $SUB -A $ACCOUNT -l procs=1,walltime=0:10:00 -N ${jobname} -v $listvar -j oe -o $LOGDIR/plot_summary.log $SCRIPTS/plot_summary.sh
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
   export listvars=RAD_AREA,LOADLQ,PDATE,NDATE,TANKDIR,IMGNDIR,PLOT_WORK_DIR,WEB_SVR,WEB_USER,WEBDIR,EXEDIR,LOGDIR,SCRIPTS,GSCRIPTS,STNMAP,GRADS,USER,STMP_USER,PTMP_USER,USER_CLASS,SUB,SUFFIX,NPREDR,NCP,Z,COMPRESS,UNCOMPRESS,PLOT_ALL_REGIONS,listvars

   list="count penalty omgnbc total omgbc"

   if [[ $MY_OS = "aix" ]]; then			# CCS/aix
      suffix=a
      cmdfile=${PLOT_WORK_DIR}/cmdfile_ptime_${suffix}
      jobname=plot_${SUFFIX}_tm_${suffix}

      rm -f $cmdfile
      rm $LOGDIR/plot_time_${suffix}.log

>$cmdfile

      for sat in ${SATLIST}; do
         echo "$SCRIPTS/plot_time.sh $sat $suffix '$list'" >> $cmdfile
      done

      ntasks=`cat $cmdfile|wc -l `
#      ((nprocs=(ntasks+1)/2))

      $SUB -a $ACCOUNT -e $listvars -j ${jobname} -u $USER -t 1:00:00 -o $LOGDIR/plot_time_${suffix}.log -p $ntasks/1/N -q dev -g {USER_CLASS} /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes -stdoutmode ordered

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

      if [[ $MY_OS = "aix" ]]; then			# CCS/aix
         cmdfile=${PLOT_WORK_DIR}/cmdfile_ptime_${sat}
         jobname=plot_${SUFFIX}_tm_${sat}
         logfile=${LOGDIR}/plot_time_${sat}.log
         rm -f ${logfile}
         rm -f ${cmdfile}
 
         list="count penalty omgnbc total omgbc"
         for var in $list; do
            echo "$SCRIPTS/plot_time.sh $sat $var $var" >> $cmdfile
         done

         ntasks=`cat $cmdfile|wc -l `
#         ((nprocs=(ntasks+1)/2))

         $SUB -a $ACCOUNT -e $listvars -j ${jobname} -u $USER -t 1:00:00 -o ${logfile} -p $ntasks/1/N -q dev -g {USER_CLASS} /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes -stdoutmode ordered

      else						# zeus/linux
         for var in $list; do
            cmdfile=${PLOT_WORK_DIR}/cmdfile_ptime_${sat}_${var}
            jobname=plot_${SUFFIX}_tm_${sat}_${var}
            logfile=${LOGDIR}/plot_time_${sat}_${var}.log
            rm -f ${logfile}
            rm -f ${cmdfile}

            if [[ $PLOT_ALL_REGIONS -eq 0 ]]; then
               wall_tm="0:40:00"
            else
               wall_tm="2:00:00"
            fi

            echo "$SCRIPTS/plot_time.sh $sat $var $var" >> $cmdfile

            $SUB -A $ACCOUNT -l procs=1,walltime=${wall_tm} -N ${jobname} -v $listvars -j oe -o ${logfile} $cmdfile
         done
      fi
   done

exit
