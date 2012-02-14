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

#-------------------------------------------------------------------
#  Locate/update the control files.  If no ctl file is available
#  report a warning to the log file.  If there is a ctl file in 
#  $tankdir then copy it to $imgndir.
#
allmissing=1
for type in ${SATYPE}; do

   # warn if no ctl file(s) available at all
   if [[ ! -s ${imgndir}/${type}.ctl.Z && ! -s ${imgndir}/${type}.ctl &&
         ! -s ${tankdir}/${type}.ctl.Z && ! -s ${tankdir}/${type}.ctl ]]; then
      echo WARNING:  unable to locate ${type}.ctl
   fi

   if [[ -s ${tankdir}/${type}.ctl.Z || -s ${tankdir}/${type}.ctl  ]]; then
      $NCP ${tankdir}/${type}.ctl* ${imgndir}/.
      allmissing=0
   fi

   if [[ -s ${imgndir}/${type}.ctl.Z || -s ${imgndir}/${type}.ctl ]]; then
      allmissing=0
   fi 
done

if [[ $allmissing = 1 ]]; then
   echo ERROR:  Unable to plot.  All control files are missing.
   exit
fi


#-------------------------------------------------------------------
#   Update the time definition (tdef) line in the angle control
#   files.
#
#   Note that the logic for the tdef in time series is backwards 
#   from angle series.  Time tdefs start at -720 from PDATE.  For
#   angle series the tdef = $PDATE and the script works backwards.
#   Some consistency on this point would be great.

   start_date=`$NDATE -720 $PDATE`

   for type in ${SATYPE}; do
      if [[ -s ${imgndir}/${type}.ctl.Z ]]; then
        uncompress ${imgndir}/${type}.ctl.Z
      fi
      ${SCRIPTS}/update_ctl_tdef.sh ${imgndir}/${type}.ctl ${start_date}
      compress ${imgndir}/${type}.ctl
   done


   export listvars=RAD_AREA,LOADLQ,PDATE,NDATE,TANKDIR,IMGNDIR,PLOT_WORK_DIR,WEB_SVR,WEB_USER,WEBDIR,EXEDIR,LOGDIR,SCRIPTS,GSCRIPTS,STNMAP,GRADS,USER,STMP_USER,PTMP_USER,U_USER,USER_CLASS,SUB,SUFFIX,SATYPE,NCP,listvars


#-------------------------------------------------------------------
#  Summary plots
#
#    Submit the summary plot job.
#
#-------------------------------------------------------------------

   cmdfile=${PLOT_WORK_DIR}/cmdfile_psummary
   jobname=plot_${SUFFIX}_summary

   rm -f $cmdfile
   rm $LOGDIR/plot_summary.log

>$cmdfile
   for type in ${SATYPE}; do
      echo "$SCRIPTS/plot_summary.sh $type" >> $cmdfile
   done

   ntasks=`cat $cmdfile|wc -l `
   ((nprocs=(ntasks+1)/2))

   $SUB -a $ACOUNT -e $listvars -j ${jobname} -u $USER -t 0:20:00 -o $LOGDIR/plot_summary.log -p $ntasks/1/N -q dev -g ${USER_CLASS} /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes -stdoutmode ordered


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

   export listvars=RAD_AREA,LOADLQ,PDATE,NDATE,TANKDIR,IMGNDIR,PLOT_WORK_DIR,WEB_SVR,WEB_USER,WEBDIR,EXEDIR,LOGDIR,SCRIPTS,GSCRIPTS,STNMAP,GRADS,USER,STMP_USER,PTMP_USER,U_USER,USER_CLASS,SUB,SUFFIX,NPREDR,SATYPE,NCP,listvars


#-------------------------------------------------------------------
# extract airs_aqua from SATYPE for separate submission 
#
   use_airs_aqua=0

   for sat in ${SATYPE}; do
      if [[ $sat = "airs_aqua" ]]; then
         use_airs_aqua=1
      else
         SATYPE3="$sat $SATYPE3"
      fi
   done


#-------------------------------------------------------------------
#  Look over satellite types.  Submit plot job for each type.
#
   export listvars=RAD_AREA,LOADLQ,PDATE,NDATE,TANKDIR,IMGNDIR,PLOT_WORK_DIR,WEB_SVR,WEB_USER,WEBDIR,EXEDIR,LOGDIR,SCRIPTS,GSCRIPTS,STNMAP,GRADS,USER,STMP_USER,PTMP_USER,U_USER,USER_CLASS,SUB,SUFFIX,NPREDR,SATYPE,NCP,listvars

   list="count penalty omgnbc total omgbc"
   suffix=a
   cmdfile=${PLOT_WORK_DIR}/cmdfile_ptime_${suffix}
   jobname=plot_${SUFFIX}_time_${suffix}

   rm -f $cmdfile
   rm $LOGDIR/plot_time_${suffix}.log

>$cmdfile
      for type in ${SATYPE3}; do
         echo "$SCRIPTS/plot_time.sh $type $suffix '$list'" >> $cmdfile
      done

      ntasks=`cat $cmdfile|wc -l `
      ((nprocs=(ntasks+1)/2))

      $SUB -a $ACOUNT -e $listvars -j ${jobname} -u $USER -t 1:00:00 -o $LOGDIR/plot_time_${suffix}.log -p $ntasks/1/N -q dev -g {USER_CLASS} /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes -stdoutmode ordered


#---------------------------------------------------------------------------
#  airs_aqua 
#
#    There is so much airs data that we submit a job for each 
#    type of plot to make.
#   
#    Submit these jobs if airs_aqua was included in $SATYPE list.
#    The $use_airs_aqua var is the flag for this condition.
#
#---------------------------------------------------------------------------
   if [[ $use_airs_aqua -eq 1 ]]; then

      suffix=airs
      cmdfile=${PLOT_WORK_DIR}/cmdfile_ptime_${suffix}
      jobname=plot_${SUFFIX}_time_${suffix}

      rm -f $cmdfile

      rm $LOGDIR/plot_time_${suffix}.log
      SATYPE3="airs_aqua"
>$cmdfile
      for type in ${SATYPE3}; do 
         list="count penalty omgnbc total omgbc"
         for var in $list; do
            echo "$SCRIPTS/plot_time.sh $type $var $var" >> $cmdfile
         done
      done
      ntasks=`cat $cmdfile|wc -l `
      ((nprocs=(ntasks+1)/2))
      $SUB -a $ACOUNT -e $listvars -j plot_${SUFFIX}_time_${suffix} -u $USER -t 1:00:00 -o $LOGDIR/plot_time_${suffix}.log -p $ntasks/1/N -q dev -g {USER_CLASS} /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes -stdoutmode ordered

   fi

exit
