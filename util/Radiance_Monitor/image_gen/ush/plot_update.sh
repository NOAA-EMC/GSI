#! /bin/ksh

#------------------------------------------------------------------
#
#  plot_update.sh
#
#  Update the imgdate post image generation.
#
#------------------------------------------------------------------
set -ax
export list=$listvar


#------------------------------------------------------------------
# Set environment variables.
#------------------------------------------------------------------
tmpdir=${STMP_USER}/done_${SUFFIX}.$PDATE
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir


#------------------------------------------------------------------
# Store the processing date
#------------------------------------------------------------------
#next_cycle=`$NDATE +6 $PDATE`
#echo $next_cycle > ./dum
#$NCP ./dum $IMGNDIR/cycle/imgdate
#${SCRIPTS}/set_imgdate.sh ${SUFFIX} ${DATA_MAP} ${next_cycle} 
${SCRIPTS}/set_imgdate.sh ${SUFFIX} ${DATA_MAP} ${PDATE} 

#------------------------------------------------------------------
#  Archive yesterday's files to hpss and remove those archived 
#  files older than 120 days.
#
#  Do this after the 06 cycle; don't want to lengthen the 00 
#  processing any further.
#------------------------------------------------------------------

if [[ ${MAKE_TAPE_ARCHIVE} = "1" ]]; then
   YSTRD=`$NDATE -24 $PDATE`
   TARDATE=`echo $YSTRD|cut -c1-8`

   ONEYEAR=`$NDATE -8760 $YSTRD`

   CYCLE=`echo $PDATE|cut -c9-10`
   DAY=`echo $PDATE|cut -c7-8`
   SUB_DIRS="angle bcoef bcor time"


#------------------------------------------------------------------
#  Create daily tape backup of data files and
#  remove old backup files older than a year.
#------------------------------------------------------------------
   if [[ "$CYCLE" = "06" ]]; then
      for dir in ${SUB_DIRS}; do
         cd ${TANKDIR}/${dir}
         if [[ "$dir" = "time" ]]; then
           htar -cvf ${HPSSDIR}/${dir}/${TARDATE}.tar *.${TARDATE}*.ieee_d* bad_*.${TARDATE}* 
           if [[ "DAY" = "01" ]]; then
              htar -cvf ${HPSSDIR}/${dir}/${TARDATE}.base.tar *.base
              hsi "rm ${HPSSDIR}/${dir}/${ONEYEAR}.base.tar"
           fi
         else
           htar -cvf ${HPSSDIR}/${dir}/${TARDATE}.tar *.${TARDATE}*.ieee_d* 
         fi
         hsi "rm ${HPSSDIR}/${dir}/${ONEYEAR}.tar*"
      done 
   fi
fi

#------------------------------------------------------------------
# Clean up and exit
#------------------------------------------------------------------
cd $tmpdir
cd ../
rm -rf $tmpdir

exit
 
