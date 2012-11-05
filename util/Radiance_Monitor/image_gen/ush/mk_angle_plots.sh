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
#export list=$listvar

imgndir=${IMGNDIR}/angle
tankdir=${TANKDIR}/angle

if [[ ! -d ${imgndir} ]]; then
   mkdir -p ${imgndir}
fi

echo Z = $Z

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

   elif [[ -s ${TANKDIR}/radmon.${PDY}/angle.${type}.ctl || -s ${TANKDIR}/radmon.${PDY}/angle.${type}.ctl.${Z} ]]; then
      $NCP ${TANKDIR}/radmon.${PDY}/angle.${type}.ctl.${Z} ${imgndir}/${type}.ctl.${Z}
      if [[ ! -s ${imgndir}/${type}.ctl.${Z} ]]; then
         $NCP ${TANKDIR}/radmon.${PDY}/angle.${type}.ctl ${imgndir}/${type}.ctl
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

# TESTING
#export SATYPE="iasi_metop-a sndrd1_g15 sndrd2_g15"
#export SATYPE="iasi_metop-a"

#-------------------------------------------------------------------
#   Update the time definition (tdef) line in the angle control 
#   files.

thirtydays=`$NDATE -720 $PDATE`
for type in ${SATYPE}; do
   if [[ -s ${imgndir}/${type}.ctl.${Z} ]]; then
     ${UNCOMPRESS} ${imgndir}/${type}.ctl.${Z}
   fi
   ${SCRIPTS}/update_ctl_tdef.sh ${imgndir}/${type}.ctl ${thirtydays}

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
#   Rename PLOT_WORK_DIR to angle subdir.
#
  export PLOT_WORK_DIR="${PLOT_WORK_DIR}/plotangle_${SUFFIX}"

  if [ -d $PLOT_WORK_DIR ] ; then
     rm -f $PLOT_WORK_DIR
  fi
  mkdir -p $PLOT_WORK_DIR
  cd $PLOT_WORK_DIR


  #-----------------------------------------------------------------
  # Loop over satellite types.  Submit job to make plots.
  #
  export listvar=RAD_AREA,LOADLQ,PDATE,NDATE,TANKDIR,IMGNDIR,WEB_SVR,WEB_USER,WEBDIR,PLOT_WORK_DIR,EXEDIR,LOGDIR,SCRIPTS,GSCRIPTS,STNMAP,GRADS,USER,STMP_USER,PTMP_USER,USER_CLASS,SUB,SUFFIX,SATYPE,NCP,Z,COMPRESS,UNCOMPRESS,PLOT_ALL_REGIONS,listvar

  list="count penalty omgnbc total omgbc fixang lapse lapse2 const scangl clw"

  if [[ $MY_MACHINE = "ccs" ]]; then	    # CCS/aix platform
     suffix=a
     cmdfile=${PLOT_WORK_DIR}/cmdfile_pangle_${suffix}
     jobname=plot_${SUFFIX}_ang_${suffix}
     logfile=$LOGDIR/plot_angle_${suffix}.log

     rm -f $cmdfile
     rm -f $logfile

     rm $LOGDIR/plot_angle_${suffix}.log
#>$cmdfile
     for type in ${SATLIST}; do
       echo "$SCRIPTS/plot_angle.sh $type $suffix '$list'" >> $cmdfile
     done

     ntasks=`cat $cmdfile|wc -l `

     $SUB -a $ACCOUNT -e $listvar -j ${jobname} -u $USER -t 0:45:00 -o ${logfile} -p $ntasks/1/N -q dev -g ${USER_CLASS}  /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes -stdoutmode ordered

  else				# Zeus/linux platform
     for sat in ${SATLIST}; do
        suffix=${sat} 
        cmdfile=${PLOT_WORK_DIR}/cmdfile_pangle_${suffix}
        jobname=plot_${SUFFIX}_ang_${suffix}
        logfile=${LOGDIR}/plot_angle_${suffix}.log

        rm -f $cmdfile
        rm -f $logfile

        echo "$SCRIPTS/plot_angle.sh $sat $suffix '$list'" >> $cmdfile

        if [[ $PLOT_ALL_REGIONS -eq 0 ]]; then
           wall_tm="0:30:00"
        else
           wall_tm="0:50:00"
        fi

        $SUB -A $ACCOUNT -l procs=1,walltime=${wall_tm} -N ${jobname} -v $listvar -j oe -o ${logfile} ${cmdfile}
     done
  fi



  #----------------------------------------------------------------------------
  #  bigSATLIST
  #   
  #    There is so much data for some sat/instrument sources that a separate 
  #    job for each is necessary.
  #   

echo starting $bigSATLIST
set -A list count penalty omgnbc total omgbc fixang lapse lapse2 const scangl clw

for sat in ${bigSATLIST}; do
   echo processing $sat in $bigSATLIST

   if [[ $MY_MACHINE = "ccs" ]]; then 	# CCS/aix, submit 4 job for each $sat
      batch=1
      ii=0

      suffix="${sat}_${batch}"
      cmdfile=${PLOT_WORK_DIR}/cmdfile_pangle_${suffix}
      rm -f $cmdfile
      jobname=plot_${SUFFIX}_ang_${suffix}
      logfile=${LOGDIR}/plot_angle_${suffix}.log

      while [[ $ii -le ${#list[@]}-1 ]]; do

         echo "$SCRIPTS/plot_angle.sh $sat $suffix ${list[$ii]}" >> $cmdfile
         (( test=ii+1 ))
         (( test=test%3 ))

         if [[ $test -eq 0 || $ii -eq ${#list[@]}-1 ]]; then
            ntasks=`cat $cmdfile|wc -l `
#           ((nprocs=(ntasks+1)/2))

            $SUB -a $ACCOUNT -e $listvar -j ${jobname} -u $USER -t 1:00:00 -o ${logfile} -p $ntasks/1/N -q dev -g ${USER_CLASS} /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes -stdoutmode ordered

            (( batch=batch+1 ))

            suffix="${sat}_${batch}"
            cmdfile=${PLOT_WORK_DIR}/cmdfile_pangle_${suffix}
            rm -f $cmdfile
            jobname=plot_${SUFFIX}_ang_${suffix}
            logfile=${LOGDIR}/plot_angle_${suffix}.log
         fi
         (( ii=ii+1 ))
      done

   else					# Zeus/Linux, submit 1 job for each sat/list item
      ii=0
      suffix="${sat}"

      while [[ $ii -le ${#list[@]}-1 ]]; do
         cmdfile=${PLOT_WORK_DIR}/cmdfile_pangle_${suffix}_${list[$ii]}
         rm -f $cmdfile
         logfile=${LOGDIR}/plot_angle_${suffix}_${list[$ii]}.log
         jobname=plot_${SUFFIX}_ang_${suffix}_${list[$ii]}

         echo "${SCRIPTS}/plot_angle.sh $sat $suffix ${list[$ii]}" >> $cmdfile

         if [[ $PLOT_ALL_REGIONS -eq 0 ]]; then
#            wall_tm="2:00:00"
            wall_tm="0:40:00"
         else
            wall_tm="3:30:00"
         fi

         $SUB -A $ACCOUNT -l procs=1,walltime=${wall_tm} -N ${jobname} -v $listvar -j oe -o ${logfile} ${cmdfile}

         (( ii=ii+1 ))
      done
   fi

done


exit
