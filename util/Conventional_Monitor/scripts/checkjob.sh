#!/bin/sh

set -ax

#--------------------------------------------------------------------
# Set environment variables

export SUFFIX=copr

export scripts=/u/wx20es/home/convweb/scripts_200707 
export LLQ=/u/wx20mi/bin/llq2
export SUB=/u/wx20mi/bin/sub
export NDATE=/nwprod/util/exec/ndate
export USER=wx20es

#--------------------------------------------------------------------
# Check status of monitoring job.  Is it already running?  If so, exit
# this script and wait for job to finish.

count=`$LLQ | grep wx20es | grep "$SUFFIX" | wc -l`
#count2=`$LLQ | grep wx20es | grep "${SUFFIX}_" | wc -l`
#count=` expr $count1 + $count2 `
if [[ $count -ne 0 ]] ; then
  sleep 50
/bin/sh $scripts/checkjob.sh
else
/bin/sh $scripts/check_convopr.sh
sleep 100
/bin/sh $scripts/checkjob.sh
fi

exit


