#!/bin/sh

#
#@ job_name=runpost
#@ account_no = GFS-MTN
##@ resources = ConsumableCpus(8) ConsumableMemory(1200)
#@ output = postoutdana$(jobid)
#@ error = posterrdana$(jobid)
#@ job_type = parallel
#@ class = dev
#@ node_usage = not_shared
#@ total_tasks = 16
#@ node = 1
#@ wall_clock_limit = 00:20:50
#@ preferences = Feature == "dev"
#@ network.MPI = csss,shared,us
#@ queue
#

set -x

export MP_LABELIO=yes

# CDATE is the cycle start date,
export CDATE=2010121112
export GDATE=`${ndate_dir}/ndate   -06 $CDATE`
export gyy=`echo $GDATE |cut -c1-4`
export gmm=`echo $GDATE |cut -c5-6`
export gdd=`echo $GDATE |cut -c7-8`
export gcyc=`echo $GDATE |cut -c9-10`
#export fhr=24
export yy=`echo $CDATE |cut -c1-4`
export mm=`echo $CDATE |cut -c5-6`
export dd=`echo $CDATE |cut -c7-8`
export cyc=`echo $CDATE |cut -c9-10`
local=/ptmp/wx23dc/tmp574_sigmap
#local=/global/noscrub/wx23dc
run=wx23dc_nogps.$CDATE
#rundir=$local/$run$mm$dd$cyc
rundir=$local/wx23dc_nogps.2010121112
cd $rundir
if [ ! -d $rundir ]; then
exit 8
fi

#/nwprod/ush/global_nceppost.sh
export POSTGPEXEC=/nwprod/exec/global_postgs
#for h in 36 48 60 72 84 96 108 120; do
#for h in f06; do
#/nwprod/ush/global_postgp.sh sig$h.$GDATE no no pgb$h.$GDATE pgi$h.$GDATE 360 181
#/nwprod/ush/global_postgp.sh sig$h.$GDATE no no pgb$h.$GDATE pgi$h.$GDATE 360 181
#done
#echo $?
for h in anl; do
#/nwprod/ush/global_postgp.sh sig$h.$CDATE no no pgb$h.$CDATE pgi$h.$CDATE 360 181
/nwprod/ush/global_postgp.sh sig$h no no pgb$h.$CDATE pgi$h.$CDATE 360 181
done
exit
