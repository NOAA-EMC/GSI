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
export list=$listvar

echo ${LOADLQ}
imgndir=${IMGNDIR}/angle
tankdir=${TANKDIR}/angle

if [[ ! -d ${imgndir} ]]; then
   mkdir -p ${imgndir}
fi

#-------------------------------------------------------------------
#  Locate/update the control files.  If no ctl file is available
#  report a warning to the log file.  If there is a ctl file in
#  $tankdir then copy it to $imgndir.

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

thirtydays=`$NDATE -720 $PDATE`
for type in ${SATYPE}; do
   if [[ -s ${imgndir}/${type}.ctl.Z ]]; then
     uncompress ${imgndir}/${type}.ctl.Z
   fi
   ${SCRIPTS}/update_ctl_tdef.sh ${imgndir}/${type}.ctl ${thirtydays}
   compress ${imgndir}/${type}.ctl
done


#-------------------------------------------------------------------
#   Rename PLOT_WORK_DIR to angle subdir.
#

  export PLOT_WORK_DIR="${PLOT_WORK_DIR}/plotangle_${SUFFIX}"
  if [ -d $PLOT_WORK_DIR ] ; then
     rm -f $PLOT_WORK_DIR
  fi
  mkdir -p $PLOT_WORK_DIR
  cd $PLOT_WORK_DIR


  #-----------------------------------------------------------------
  #  extract airs_aqua and iasi_metop-a from SATYPE for separate submissions.
  #
  use_airs_aqua=0
  use_iasi_metop=0

  for sat in ${SATYPE}; do
    if [[ $sat = "airs_aqua" ]]; then
      use_airs_aqua=1
    elif [[ $sat = "iasi_metop-a" ]]; then
      use_iasi_metop=1
    else
      SATYPE3="$sat $SATYPE3"
    fi
  done 


  #-----------------------------------------------------------------
  # Loop over satellite types.  Submit poe job to make plots.
  #
  export listvars=RAD_AREA,LOADLQ,PDATE,NDATE,TANKDIR,IMGNDIR,WEB_SVR,WEB_USER,WEBDIR,PLOT_WORK_DIR,EXEDIR,LOGDIR,SCRIPTS,GSCRIPTS,STNMAP,GRADS,USER,U_USER,STMP_USER,PTMP_USER,USER_CLASS,SUB,SUFFIX,SATYPE,NCP,listvars
  list="count penalty omgnbc total omgbc fixang lapse lapse2 const scangl clw"
  suffix=a
  cmdfile=${PLOT_WORK_DIR}/cmdfile_pangle_${suffix}
  jobname=plot_${SUFFIX}_angle_${suffix}

  rm -f $cmdfile

  rm $LOGDIR/plot_angle_${suffix}.log
>$cmdfile
  for type in ${SATYPE3}; do
    echo "$SCRIPTS/plot_angle.sh $type $suffix '$list'" >> $cmdfile
  done

  ntasks=`cat $cmdfile|wc -l `
  ((nprocs=(ntasks+1)/2))

  $SUB -a $ACOUNT -e $listvars -j ${jobname} -u $USER -t 0:45:00 -o $LOGDIR/plot_angle_${suffix}.log -p $ntasks/1/N -q dev -g ${USER_CLASS}  /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes -stdoutmode ordered


  #----------------------------------------------------------------------------
  #  airs_aqua
  #   
  #    There is so much airs data that we submit a job for each type of 
  #    plot to make.
  #   
  #    Submit these jobs if airs_aqua was included in the $SATYPE list.
  #    The $use_airs_aqua var is the flag for this condition.
  #

  if [[ $use_airs_aqua -eq 1 ]]; then

    SATYPE3="airs_aqua"
    set -A list count penalty omgnbc total omgbc fixang lapse lapse2 const scangl clw

    ii=0
    batch=1
    suffix="airs_${batch}"
    cmdfile=${PLOT_WORK_DIR}/cmdfile_pangle_${suffix}
    jobname=plot_${SUFFIX}_angle_${suffix}
    rm -f $cmdfile

    while [[ $ii -le ${#list[@]}-1 ]]; do

       echo "$SCRIPTS/plot_angle.sh $SATYPE3 $suffix ${list[$ii]}" >> $cmdfile
       (( test=ii+1 ))
       (( test=test%2 ))

       if [[ $test -eq 0 || $ii -eq ${#list[@]}-1 ]]; then
          ntasks=`cat $cmdfile|wc -l `
          ((nprocs=(ntasks+1)/2))

          $SUB -a $ACOUNT -e $listvars -j ${jobname} -u $USER -t 1:00:00 -o $LOGDIR/plot_angle_${suffix}.log -p $ntasks/1/N -q dev -g ${USER_CLASS} /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes -stdoutmode ordered
          (( batch=batch+1 ))
          suffix="airs_${batch}"
          cmdfile=${PLOT_WORK_DIR}/cmdfile_pangle_${suffix}
          jobname=plot_${SUFFIX}_angle_${suffix}
          rm -f $cmdfile
       fi
       (( ii=ii+1 ))

    done

  fi


  #----------------------------------------------------------------------------
  #  iasi_metop-a
  #
  #    There is so much iasi data that we submit a job for each 
  #    type of plot to make.
  #
  #    Submit these jobs if iasi_metop-a was included in $SATYPE list.
  #    The $use_iasi_metop var is the flag for this condition.
  #

  if [[ $use_iasi_metop -eq 1 ]]; then
    SATYPE3="iasi_metop-a"
    set -A list count penalty omgnbc total omgbc fixang lapse lapse2 const scangl clw

    ii=0
    batch=1
    suffix="iasi_metop_${batch}"
    cmdfile=${PLOT_WORK_DIR}/cmdfile_pangle_${suffix}
    jobname=plot_${SUFFIX}_angle_${suffix}
    rm -f $cmdfile

    while [[ $ii -le ${#list[@]}-1 ]]; do

       echo "$SCRIPTS/plot_angle.sh $SATYPE3 $suffix ${list[$ii]}" >> $cmdfile
       (( test=ii+1 ))
       (( test=test%2 ))

       if [[ $test -eq 0 || $ii -eq ${#list[@]}-1 ]]; then
          ntasks=`cat $cmdfile|wc -l `
          ((nprocs=(ntasks+1)/2))

          $SUB -a $ACOUNT -e $listvars -j ${jobname} -u $USER -t 1:00:00 -o $LOGDIR/plot_angle_${suffix}.log -p $ntasks/1/N -q dev -g ${USER_CLASS} /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes -stdoutmode ordered
          (( batch=batch+1 ))
          suffix="iasi_metop_${batch}"
          cmdfile=${PLOT_WORK_DIR}/cmdfile_pangle_${suffix}
          jobname=plot_${SUFFIX}_angle_${suffix}
          rm -f $cmdfile
       fi
       (( ii=ii+1 ))

    done
  fi

exit
