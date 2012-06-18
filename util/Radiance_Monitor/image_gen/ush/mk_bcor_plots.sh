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
export list=$listvar

imgndir=${IMGNDIR}/bcor
tankdir=${TANKDIR}/bcor

if [[ ! -d ${imgndir} ]]; then
   mkdir -p ${imgndir}
fi


#-------------------------------------------------------------------
#  Locate/update the control files.  If no ctl file is available
#  report a warning to the log file.  Search order is $imgndir,
#  the $TANKDIR/radmon.$PDY, then $tankdir.
#
allmissing=1
PDY=`echo $PDATE|cut -c1-8`
for type in ${SATYPE}; do
   found=0

   if [[ -s ${imgndir}/${type}.ctl.Z || -s ${imgndir}/${type}.ctl ]]; then
      allmissing=0
      found=1

   elif [[ -s ${TANKDIR}/radmon.${PDY}/bcor.${type}.ctl || -s ${TANKDIR}/radmon.${PDY}/bcor.${type}.ctl.Z ]]; then
      $NCP ${TANKDIR}/radmon.${PDY}/bcor.${type}.ctl.Z ${imgndir}/${type}.ctl.Z
      if [[ ! -s ${imgndir}/${type}.ctl.Z ]]; then
         $NCP ${TANKDIR}/radmon.${PDY}/bcor.${type}.ctl ${imgndir}/${type}.ctl
      fi
      allmissing=0
      found=1

   elif [[ -s ${tankdir}/${type}.ctl.Z || -s ${tankdir}/${type}.ctl  ]]; then
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
#   Update the time definition (tdef) line in the bcor control
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


#------------------------------------------------------------------
#   Submit plot jobs
#

  plot_list="count total fixang lapse lapse2 const scangl clw"

  export PLOT_WORK_DIR=${PLOT_WORK_DIR}/plotbcor_${SUFFIX}
  if [ -d ${PLOT_WORK_DIR} ] ; then 
     rm -f ${PLOT_WORK_DIR}
  fi
  mkdir -p ${PLOT_WORK_DIR}
  cd ${PLOT_WORK_DIR}


  #-------------------------------------------------------------------------
  #  extract airs_aqua from SATYPE for separate submission
  #
  use_airs_aqua=0

  for sat in ${SATYPE}; do
    if [[ $sat = "airs_aqua" ]]; then
      use_airs_aqua=1
    else
      SATYPE3="$sat $SATYPE3"
    fi
  done


  #-------------------------------------------------------------------------
  # Loop over satellite types.  Submit poe job to make plots.  Each task handles
  # a single satellite type

  export listvars=RAD_AREA,LOADLQ,PDATE,NDATE,TANKDIR,IMGNDIR,PLOT_WORK_DIR,WEB_SVR,WEB_USER,WEBDIR,EXEDIR,LOGDIR,SCRIPTS,GSCRIPTS,STNMAP,GRADS,USER,STMP_USER,PTMP_USER,SUB,SUFFIX,SATYPE,NCP,listvars
  suffix=a
  cmdfile=cmdfile_pbcor_${suffix}
  jobname=plot_${SUFFIX}_bcor_${suffix}

  rm -f $cmdfile
  rm $LOGDIR/plot_bcor_${suffix}.log

>$cmdfile
  for type in ${SATYPE3}; do
     echo "$SCRIPTS/plot_bcor.sh $type $suffix '$plot_list'" >> $cmdfile
  done

  ntasks=`cat $cmdfile|wc -l `
  ((nprocs=(ntasks+1)/2))
  echo $nprocs

  $SUB -a $ACOUNT -e $listvars -j ${jobname} -u $USER -t 1:00:00 -o $LOGDIR/plot_bcor_${suffix}.log -p $ntasks/1/N -q dev -g ${USER_CLASS} /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes -stdoutmode ordered
#  $SUB -a $ACOUNT -e $listvars -j ${jobname} -u $USER -t 1:00:00 -o $LOGDIR/plot_bcor_${suffix}.log -p $nprocs/1/N -q dev -g ${USER_CLASS} /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes -stdoutmode ordered


  #--------------------------------------------------------------------------
  #  airs_aqua
  #  
  #    There is so much airs data that we submit a separate poe 
  #    job to handle each plot type for the airs data.
  #
  #    Submit these jobs if airs_aqua was included in $SATYPE list.
  #    The $use_airs_aqua var is the flag for this condition.
  #
  #--------------------------------------------------------------------------
  if [[ $use_airs_aqua -eq 1 ]]; then

     SATYPE3="airs_aqua"
     suffix=airs
     cmdfile=cmdfile_pbcor_${suffix}
     jobname=plot_${SUFFIX}_bcor_${suffix}

     rm -f $cmdfile
     rm $LOGDIR/plot_bcor_${suffix}.log

>$cmdfile
     for type in ${SATYPE3}; do 
        for var in $plot_list; do
           echo "$SCRIPTS/plot_bcor.sh $type $var $var" >> $cmdfile
        done
     done
     ntasks=`cat $cmdfile|wc -l `
     ((nprocs=(ntasks+1)/2))

     $SUB -a $ACOUNT -e $listvars -j ${jobname} -u $USER -t 1:00:00 -o $LOGDIR/plot_bcor_${suffix}.log -p $ntasks/1/N -q dev -g ${USER_CLASS} /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes -stdoutmode ordered
#     $SUB -a $ACOUNT -e $listvars -j ${jobname} -u $USER -t 1:00:00 -o $LOGDIR/plot_bcor_${suffix}.log -p $nprocs/1/N -q dev -g ${USER_CLASS} /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes -stdoutmode ordered
  fi
  fi


exit
