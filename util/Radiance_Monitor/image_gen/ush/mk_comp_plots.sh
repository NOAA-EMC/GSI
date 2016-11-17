#!/bin/ksh

#-------------------------------------------------------------------
#
#  script:  mk_comp_plots.sh
#
#  submit plot jobs to make the comparison images.
#  
#-------------------------------------------------------------------

set -ax

echo start mk_comp_plots.sh
date

cd $PLOT_WORK_DIR
echo sdate = $SDATE, edate = $EDATE

CYCLES=`${IG_SCRIPTS}/cycle_delta.pl ${SDATE} ${EDATE}`
export NUM_CYCLES=`expr $CYCLES + 1`

echo NUM_CYCLES = $NUM_CYCLES

imgndir1=${IMGNDIR}/${SUFFIX1}/pngs/comp
echo imgndir1 = $imgndir1
imgndir2=${IMGNDIR}/${SUFFIX2}/pngs/comp
echo imgndir2 = $imgndir2

tankdir1=${TANKDIR}/${SUFFIX1}
echo tankdir1 = $tankdir1
tankdir2=${TANKDIR}/${SUFFIX2}
echo tankdir2 = $tankdir2

if [[ ! -d ${imgndir1} ]]; then
   mkdir -p ${imgndir1}
fi
if [[ ! -d ${imgndir2} ]]; then
   mkdir -p ${imgndir2}
fi

#-------------------------------------------------------------------
#  Locate/update the control files.  If no ctl file is available
#  report a warning to the log file.  Search order is $imgndir,
#  the $TANKDIR/radmon.$PDY, then $tankdir.
#  Note that the comparision plots use the time data and control 
#  files.

allmissing=1
PDY=`echo $EDATE|cut -c1-8`
ndays=$(($NUM_CYCLES/4))
test_day=$EDATE

for type in ${SATYPE}; do
   found=0
   alldone=0
   test_day=$EDATE
   ctr=$ndays

   while [[ $found -eq 0 && $alldone -ne 1 ]]; do
      pdy=`echo $test_day|cut -c1-8`
      if [[ -s ${TANKDIR1}/radmon.${pdy}/time.${type}.ctl.${Z} ]]; then
         $NCP ${TANKDIR1}/radmon.${pdy}/time.${type}.ctl.${Z} ${imgndir1}/${type}.ctl.${Z}
         found=1
      elif [[ -s ${TANKDIR1}/radmon.${pdy}/time.${type}.ctl ]]; then
         $NCP ${TANKDIR1}/radmon.${pdy}/time.${type}.ctl ${imgndir1}/${type}.ctl
         found=1
      fi

      if [[ $found -eq 0 ]]; then
         if [[ $ctr -gt 0 ]]; then
            test_day=`$NDATE -24 ${pdy}00`
            ctr=$(($ctr-1))
         else
            alldone=1
         fi
      fi
   done

   if [[ -s ${imgndir1}/${type}.ctl.${Z} || -s ${imgndir1}/${type}.ctl ]]; then
      allmissing=0
      found=1
   fi
done

if [[ $allmissing = 1 ]]; then
   echo ERROR:  Unable to plot.  All time control files are missing.
   exit
fi


#-------------------------------------------------------------------
#   Copy all control files to $WORK_DIR.
#
#   Update the time definition (tdef) line in the local control
#   files and conditionally remove cray_32bit_ieee from the options line.

   for type in ${SATYPE}; do
      $NCP ${imgndir1}/${type}.ctl* ${PLOT_WORK_DIR}/.
      ${UNCOMPRESS} ${PLOT_WORK_DIR}/${type}.ctl.${Z}

      ${IG_SCRIPTS}/update_ctl_tdef.sh ${PLOT_WORK_DIR}/${type}.ctl ${SDATE} ${NUM_CYCLES}
 
      if [[ $MY_MACHINE = "wcoss" || $MY_MACHINE = "zeus" || $MY_MACHINE = "theia" ]]; then
         sed -e 's/cray_32bit_ieee/ /' ${PLOT_WORK_DIR}/${type}.ctl > ${PLOT_WORK_DIR}/tmp_${type}.ctl
         sed -s 's/\^/\'"^${SUFFIX1}."'/1' ${PLOT_WORK_DIR}/tmp_${type}.ctl > ${PLOT_WORK_DIR}/${SUFFIX1}.${type}.ctl
         sed -s 's/\^/\'"^${SUFFIX2}."'/1' ${PLOT_WORK_DIR}/tmp_${type}.ctl > ${PLOT_WORK_DIR}/${SUFFIX2}.${type}.ctl
         
         rm ${type}.ctl 
         rm tmp_${type}.ctl
      fi
   done

#   for sat in ${SATYPE}; do
#      nchanl=`cat ${imgndir}/${sat}.ctl | gawk '/title/{print $NF}'`
#      if [[ $nchanl -ge 100 ]]; then
#         bigSATLIST=" $sat $bigSATLIST "
#      else
#         SATLIST=" $sat $SATLIST "
#      fi
#   done


#-------------------------------------------------------------------
#
#    Submit the comparison plot jobs.
#
#-------------------------------------------------------------------

#   cmdfile=${PLOT_WORK_DIR}/cmdfile_pcomp
   jobname=plot_${SUFFIX1}_comp
   logfile=${LOGdir}/plot_${SUFFIX1}_comp.log

#   rm -f $cmdfile
   rm ${logfile}

#>$cmdfile
#   for type in ${SATYPE}; do
#      echo "$SCRIPTS/plot_comp.sh $type" >> $cmdfile
#   done
#
#   ntasks=`cat $cmdfile|wc -l `
#   ((nprocs=(ntasks+1)/2))

   if [[ $MY_MACHINE = "wcoss" ]]; then
      $SUB -q $JOB_QUEUE -P $PROJECT -M 40 -R affinity[core] -o ${logfile} -W 0:20 -J ${jobname} $IG_SCRIPTS/plot_comp.sh
   elif [[ $MY_MACHINE = "cray" ]]; then
      $SUB -q $JOB_QUEUE -P $PROJECT -M 40 -o ${logfile} -W 0:20 -J ${jobname} $IG_SCRIPTS/plot_comp.sh
   elif [[ $MY_MACHINE = "zeus" || $MY_MACHINE = "theia" ]]; then
      $SUB -A $ACCOUNT -l procs=1,walltime=0:30:00 -N ${jobname} -V -j oe -o ${logfile} $IG_SCRIPTS/plot_comp.sh
   fi

echo end mk_comp_plots.sh

exit
