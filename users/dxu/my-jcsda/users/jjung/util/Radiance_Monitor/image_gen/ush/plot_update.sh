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
#  Archive yesterday's files to hpss 
#
#  Do this after the 06 cycle; don't want to lengthen the 00 
#  processing any further.
#------------------------------------------------------------------

CYCLE=`echo $PDATE|cut -c9-10`

if [[ ${DO_ARCHIVE} = "1" && ${CYCLE} = "06" ]]; then
   HPSSDIR=${HPSS_DIR}/${SUFFIX}

   YSTRD=`$NDATE -24 $PDATE`
   TARDATE=`echo $YSTRD|cut -c1-8`

#   ONEYEAR=`$NDATE -8760 $YSTRD`
   DAY=`echo $PDATE|cut -c7-8`

   htar -cvf ${HPSSDIR}/radmon.${TARDATE}.tar ${TANKDIR}/radmon.${TARDATE}

fi

#------------------------------------------------------------------
# Clean up and exit
#------------------------------------------------------------------
cd $tmpdir
cd ../
rm -rf $tmpdir

exit
 
