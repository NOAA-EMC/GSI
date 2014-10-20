#! /bin/ksh

set -ax
export list=$listvar


#------------------------------------------------------------------
# Set environment variables.
tmpdir=${stmproot}/${LOGNAME}/done_${SUFFIX}_${string}.$PDATE
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir


#--------------------------------------------------------------------
# Set next date to process
qdate=`$NDATE +06 $PDATE`
echo $qdate > ./dum
$NCP ./dum $TANKDIR/cycle/prodate


#--------------------------------------------------------------------
# Clean up and exit
cd $tmpdir
cd ../
rm -rf $tmpdir

exit
 
