#!/bin/sh

#
#@ job_name=runpost
#@ account_no = GFS-MTN
#@ output = postoutdana$(jobid)
#@ error = posterrdana$(jobid)
#@ job_type = parallel
#@ class = dev
#@ group= dev
#@ node_usage = not_shared
#@ total_tasks = 16
#@ node = 1
#@ node_resources = ConsumableMemory(110GB)
#@ task_affinity = core(1)
#@ wall_clock_limit = 00:20:50
#@ network.MPI = sn_all,shared,us
#@ queue
#

set -x
mkdir /stmp/wx23dc/posttest
cd /stmp/wx23dc/posttest
CDATE=2012120318
cp /ptmp/glopara/prgm141ctl1/sigf09.gdas.$CDATE .
export MP_LABELIO=yes
export POSTGPEXEC=/nwprod/exec/global_postgs

/nwprod/ush/global_postgp.sh sigf09.gdas.$CDATE no no pgbf09.gdas.$CDATE pgbf09.gdas.$CDATE.idx 360 181

exit
