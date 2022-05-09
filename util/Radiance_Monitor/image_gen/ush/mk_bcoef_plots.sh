#!/bin/bash

#------------------------------------------------------------------
#
# mk_bcoef_plots.sh
#
# submit the plot jobs to make the bcoef images.
#
#------------------------------------------------------------------

set -ax
date
echo "begin mk_bcoef_plots.sh"

imgndir="${IMGNDIR}/bcoef"
tankdir="${TANKverf}/bcoef"

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
PDY=`echo $PDATE|cut -c1-8`
cycdy=$((24/$CYCLE_INTERVAL))
ndays=$(($NUM_CYCLES/$cycdy))

test_day=$PDATE

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
         #  Determine if the bcoef files are in a tar file.  If so
         #  extract the ctl files for this $type.  If both a compressed
         #  and uncompressed version of the radmon_bcoef.tar file exist, 
         #  flag that as an error condition.
         #
         if [[ -e ${ieee_src}/radmon_bcoef.tar && -e ${ieee_src}/radmon_bcoef.tar.${Z} ]]; then
            echo "Located both radmon_bcoef.tar and radmon_bcoef.tar.${Z} in ${ieee_src}.  Unable to plot."
            exit 1

         elif [[ -e ${ieee_src}/radmon_bcoef.tar || -e ${ieee_src}/radmon_bcoef.tar.${Z} ]]; then
            using_tar=1
            ctl_list=`tar -tf ${ieee_src}/radmon_bcoef.tar* | grep ${type} | grep ctl`
            if [[ ${ctl_list} != "" ]]; then
               cwd=`pwd`
               cd ${ieee_src}
               tar -xf ./radmon_bcoef.tar* ${ctl_list}
               cd ${cwd}
            fi
         fi

         #--------------------------------------------------
         #  Copy the *ctl* files to $imgndir, dropping
         #  'bcoef.' from the file name.
         #
         ctl_files=`ls $ieee_src/bcoef.$type*.ctl*`
         prefix='bcoef.'
         for file in $ctl_files; do
            newfile=`basename $file | sed -e "s/^$prefix//"`
            $NCP ${file} ${imgndir}/${newfile}
            found=1
         done

         #-------------------------------------------------------
         #  If there's a radmon_bcoef.tar archive in ${ieee_src}
         #  then delete the extracted *ctl* files.
         if [[ $using_tar -eq 1 ]]; then
            rm -f ${ieee_src}/bcoef.${type}.ctl*
            rm -f ${ieee_src}/bcoef.${type}_anl.ctl*
         fi

      fi

      if [[ $found -eq 0 ]]; then
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
   echo "ERROR:  Unable to plot.  All bcoef control files are missing from ${TANKverf} for requested date range."
   exit 2
fi


for type in ${SATYPE}; do
   if [[ -s ${imgndir}/${type}.ctl.${Z} ]]; then
     ${UNCOMPRESS} ${imgndir}/${type}.ctl.${Z}
   fi

   ${IG_SCRIPTS}/update_ctl_tdef.sh ${imgndir}/${type}.ctl ${START_DATE} ${NUM_CYCLES}
   ${COMPRESS} ${imgndir}/${type}.ctl
done



#-------------------------------------------------------------------
# submit plot job
#

jobname="plot_${RADMON_SUFFIX}_bcoef"
logfile="$LOGdir/plot_bcoef.log"
rm ${logfile}

if [[ $MY_MACHINE = "wcoss_d" ]]; then
   $SUB -q $JOB_QUEUE -P $PROJECT -o ${logfile} -M 80 -W 1:15 \
        -R "affinity[core]" -J ${jobname} -cwd ${PWD} $IG_SCRIPTS/plot_bcoef.sh

elif [[ $MY_MACHINE = "wcoss_c" ]]; then
   $SUB -q $JOB_QUEUE -P $PROJECT -o ${logfile} -M 80 -W 1:15 \
        -J ${jobname} -cwd ${PWD} $IG_SCRIPTS/plot_bcoef.sh

elif [[ $MY_MACHINE = "hera" || $MY_MACHINE = "s4" ]]; then
   $SUB --account $ACCOUNT --ntasks=1 --mem=5g --time=1:00:00 -J ${jobname} \
        -o ${logfile} -D . $IG_SCRIPTS/plot_bcoef.sh 

elif [[ $MY_MACHINE = "jet" ]]; then
   $SUB --account $ACCOUNT --ntasks=1 --mem=5g --time=1:00:00 -J ${jobname} \
        -p ${RADMON_PARTITION} -o ${logfile} -D . $IG_SCRIPTS/plot_bcoef.sh

elif [[ $MY_MACHINE = "wcoss2" ]]; then
   $SUB -q $JOB_QUEUE -A $ACCOUNT -o ${logfile} -e $LOGdir/plot_bcoef.err -V \
        -l select=1:mem=1g -l walltime=1:00:00 -N ${jobname} $IG_SCRIPTS/plot_bcoef.sh
fi

echo "end mk_bcoef_plots.sh"
exit
