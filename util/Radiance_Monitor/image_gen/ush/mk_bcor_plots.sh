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

#
# testing
#export SATYPE="iasi_metop-a"

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

   if [[ -s ${imgndir}/${type}.ctl.${Z} || -s ${imgndir}/${type}.ctl ]]; then
      allmissing=0
      found=1

   elif [[ -s ${TANKDIR}/radmon.${PDY}/bcor.${type}.ctl || -s ${TANKDIR}/radmon.${PDY}/bcor.${type}.ctl.${Z} ]]; then
      $NCP ${TANKDIR}/radmon.${PDY}/bcor.${type}.ctl.${Z} ${imgndir}/${type}.ctl.${Z}
      if [[ ! -s ${imgndir}/${type}.ctl.${Z} ]]; then
         $NCP ${TANKDIR}/radmon.${PDY}/bcor.${type}.ctl ${imgndir}/${type}.ctl
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
#   Update the time definition (tdef) line in the bcor control
#   files. Conditionally rm cray_32bit_ieee from options line.
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


#------------------------------------------------------------------
#   Submit plot jobs
#

  plot_list="count total fixang lapse lapse2 const scangl clw"

  export PLOT_WORK_DIR=${PLOT_WORK_DIR}/plotbcor_${SUFFIX}
  if [[ -d ${PLOT_WORK_DIR} ]]; then 
     rm -f ${PLOT_WORK_DIR}
  fi
  mkdir -p ${PLOT_WORK_DIR}
  cd ${PLOT_WORK_DIR}


  #-------------------------------------------------------------------------
  # Loop over satellite/instruments.  Submit poe job to make plots.  Each task handles
  # a single satellite/insrument.

  export listvars=RAD_AREA,LOADLQ,PDATE,NDATE,TANKDIR,IMGNDIR,PLOT_WORK_DIR,WEB_SVR,WEB_USER,WEBDIR,EXEDIR,LOGDIR,SCRIPTS,GSCRIPTS,STNMAP,GRADS,GADDIR,USER,STMP_USER,PTMP_USER,SUB,SUFFIX,SATYPE,NCP,Z,COMPRESS,UNCOMPRESS,PLOT_ALL_REGIONS,listvars

  if [[ $MY_MACHINE = "ccs" || $MY_MACHINE = "wcoss" ]]; then		#CCS and wcoss
     suffix=a
     cmdfile=cmdfile_pbcor_${suffix}
     jobname=plot_${SUFFIX}_bcor_${suffix}
     logfile=${LOGDIR}/plot_bcor_${suffix}.log

     rm -f ${cmdfile}
     rm -f ${logfile}

>$cmdfile
     for sat in ${SATLIST}; do
        echo "$SCRIPTS/plot_bcor.sh $sat $suffix '$plot_list'" >> $cmdfile
     done
     chmod 755 $cmdfile

     ntasks=`cat $cmdfile|wc -l `
#     ((nprocs=(ntasks+1)/2))

     if [[ $MY_MACHINE = "wcoss" ]]; then
        $SUB -q transfer -n $ntasks -o ${logfile} -W 0:45 -J ${jobname} ./$cmdfile
     else
        $SUB -a $ACCOUNT -e $listvars -j ${jobname} -u $USER -t 1:00:00 -o ${logfile} -p $ntasks/1/N -q dev -g ${USER_CLASS} /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes -stdoutmode ordered
     fi
  else					#Zeus/linux
     for sat in ${SATLIST}; do
        suffix=${sat}
        cmdfile=cmdfile_pbcor_${sat}
        jobname=plot_${SUFFIX}_bcor_${sat}
        logfile=${LOGDIR}/plot_bcor_${sat}.log

        rm -f $cmdfile
        rm -f $logfile

        echo "$SCRIPTS/plot_bcor.sh $sat $suffix '$plot_list'" >> $cmdfile

        if [[ $PLOT_ALL_REGIONS -eq 0 ]]; then
           wall_tm="0:20:00"
        else
           wall_tm="0:40:00"
        fi

        $SUB -A $ACCOUNT -l procs=1,walltime=${wall_tm} -N ${jobname} -v $listvars -j oe -o ${logfile} $cmdfile
     done
  fi

  #--------------------------------------------------------------------------
  #  bigSATLIST
  #  
  #    Some satellite/instrument sources have so many channels that a separate
  #    job to handle each plot type is the fastest solution.
  #
  #--------------------------------------------------------------------------
  for sat in ${bigSATLIST}; do
     suffix=$sat

     if [[ $MY_MACHINE = "ccs" || $MY_MACHINE = "wcoss" ]]; then	# CCS/aix

        cmdfile=cmdfile_pbcor_${suffix}
        jobname=plot_${SUFFIX}_bcor_${suffix}
        logfile=${LOGDIR}/plot_bcor_${suffix}.log

        rm -f $cmdfile
        rm ${logfile}

>$cmdfile
        for var in $plot_list; do
           echo "$SCRIPTS/plot_bcor.sh $sat $var $var" >> $cmdfile
        done
        chmod 755 $cmdfile
        ntasks=`cat $cmdfile|wc -l `

        if [[ $MY_MACHINE = "wcoss" ]]; then
           $SUB -q transfer -n $ntasks -o ${logfile} -W 0:45 -J ${jobname} ./$cmdfile
        else
           $SUB -a $ACCOUNT -e $listvars -j ${jobname} -u $USER -t 1:00:00 -o ${logfile} -p $ntasks/1/N -q dev -g ${USER_CLASS} /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes -stdoutmode ordered
        fi
     else					# zeus/linux
        for var in $plot_list; do
           cmdfile=cmdfile_pbcor_${suffix}_${var}
           jobname=plot_${SUFFIX}_bcor_${suffix}_${var}
           logfile=${LOGDIR}/plot_bcor_${suffix}_${var}.log

           rm -f ${cmdfile}
           rm -f ${logfile}

           echo "$SCRIPTS/plot_bcor.sh $sat $var $var" >> $cmdfile
           if [[ $PLOT_ALL_REGIONS -eq 0 ]]; then            
              wall_tm="0:20:00"
           else
              wall_tm="0:50:00"
           fi

           $SUB -A $ACCOUNT -l procs=1,walltime=${wall_tm} -N ${jobname} -v $listvars -j oe -o ${logfile} $cmdfile

        done
     fi
  done

exit
