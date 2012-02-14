#! /bin/ksh

#------------------------------------------------------------------
#  verf_update.sh
#
#  Update the prodate record for the next data cycle.  Clean up the
#  WORKDIR if this is the last verf job to finish.
#------------------------------------------------------------------

set -ax
export list=$listvar

#------------------------------------------------------------------
# Set environment variables.

tmpdir=${WORKDIR}/done_${SUFFIX}.$PDATE
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir


#--------------------------------------------------------------------
# Set next date to process, copy to $TANKDIR/cycle

echo ${PDATE} > ./dum

if [ ! -d $TANKDIR ]; then
   mkdir -p $TANKDIR
fi

TANKDIR=$TANKDIR/cycle
if [ ! -d $TANKDIR ]; then
   mkdir -p $TANKDIR
fi

#$NCP ./dum $TANKDIR/prodate
${SCRIPTS}/set_prodate.sh $SUFFIX ${DATA_MAP} ${PDATE}


#--------------------------------------------------------------------
# If the IMGNDIR doesn't exist, then create it and its subdirectories,
# and add an imgdate file to indicate the next cycle to be processed. 

if [[ $RUN_ENVIR = para || $RUN_ENVIR = dev ]]; then

   if [ ! -d $IMGNDIR ]; then
      imgndirs="angle bcor bcoef cycle horiz time"

      for dir in ${imgndirs}; do   
         mkdir -p $IMGNDIR/$dir
      done

#      rm ./dum
#      echo $PDATE > ./dum 
#      $NCP ./dum $IMGNDIR/cycle/imgdate
      
   fi
fi


#--------------------------------------------------------------------
# Clean up and exit, rm $WORKDIR if this is the last verf job.
#

echo cleaning up $tmpdir
cd $tmpdir
cd ../
rm -rf $tmpdir

echo cleaning up $WORKDIR
count=`ls ${LOADLQ}/verf*_$SUFFIX* | wc -l`
complete=`grep "COMPLETED" ${LOADLQ}/verf*_$SUFFIX* | wc -l`

total=`expr $count - $complete`

if [[ $total -le 1 ]]; then
   cd $WORKDIR
   cd ../
   rm -rf $WORKDIR
fi

exit
 
