#! /bin/ksh

set -ax
export list=$listvar

#------------------------------------------------------------------
# Set environment variables.
tmpdir=/stmpp1/Edward.Safford/done_${SUFFIX}.$PDATE
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir


#--------------------------------------------------------------------
# Set next date to process
qdate=`ndate +06 $PDATE`
echo $qdate > ./dum
$NCP ./dum $TANKDIR/cycle/prodate

#--------------------------------------------------------------------
# Copy latest 15 days worth of data to /sss../save directory
arch=/sss/emc/da/save/Edward.Safford/nbns/stats/convweb/copr

#--------------------------------------------------------------------
# Clean up and exit
cd $tmpdir
cd ../
rm -rf $tmpdir

exit
 
