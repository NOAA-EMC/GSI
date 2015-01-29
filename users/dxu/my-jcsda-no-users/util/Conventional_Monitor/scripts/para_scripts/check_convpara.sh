#!/bin/sh

set -ax

#--------------------------------------------------------------------
# Set environment variables

export SUFFIX=cpara
export NET=gdas
tmpdir=/stmp/wx20es/check_conv${SUFFIX}

export TANKDIR=/u/wx20es/nbns/stats/convweb/${SUFFIX}
export LOGDIR=/ptmp/wx20es/logs/conv${SUFFIX}
export SCRIPTS=/u/wx20es/home/convweb/scripts
export LLQ=/u/wx20mi/bin/llq2
export SUB=/u/wx20mi/bin/sub
export NDATE=/nwprod/util/exec/ndate
##export NCP=/u/wx20mi/bin/ncp
export NCP=/bin/cp
export USER=wx20es
export PLOT=0
ACOUNT=RDAS-MTN

mkdir -p $TANKDIR
mkdir -p $LOGDIR

rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

#--------------------------------------------------------------------
# Check status of monitoring job.  Is it already running?  If so, exit
# this script and wait for job to finish.

count1=`$LLQ | grep wx20es | grep "_$SUFFIX" | wc -l`
count2=`$LLQ | grep wx20es | grep "${SUFFIX}_" | wc -l`
count=` expr $count1 + $count2 `
if [[ $count -ne 0 ]] ; then
   cd $tmpdir
   cd ../
   rm -rf $tmpdir
   exit
fi

#--------------------------------------------------------------------
# Get date of cycle to process.

$NCP $TANKDIR/cycle/prodate ./prodate
export PDATE=`cat 'prodate'`

sdate=`echo $PDATE|cut -c1-8`
export CYA=`echo $PDATE|cut -c9-10`

##export DATDIR=/nbns/global/wx20rt/prc
export DATDIR=/com/gfs/para/gdas.$sdate



#--------------------------------------------------------------------
# Based on cycle, turn on/off plotting.  (ALWAYS generate data files).
#if [[ "$CYA" = "00" ]];then
#   export PLOT=1
#fi


#--------------------------------------------------------------------
# If data is available, export variables, and submit driver for
# radiance monitoring jobs.

export list0=PDATE,TANKDIR,LOGDIR,SCRIPTS,SUB,NDATE,SUFFIX,NET,NCP,PLOT,USER,DATDIR,CYA,list0
if [[ -s $DATDIR/gdas1.t${CYA}z.cnvstat ]]; then
   /bin/sh $SCRIPTS/conv${SUFFIX}.sh
#   /bin/sh $SCRIPTS/convcpara2.sh
fi


#--------------------------------------------------------------------
# Clean up and exit
cd $tmpdir
cd ../
rm -rf $tmpdir

exit
