#!/bin/sh
#@ job_name=ss2gg
#@ error=ss2gg.err$(jobid)
#@ job_type=parallel
#@ network.MPI=sn_all,shared,us
#@ node_usage = not_shared
#@ tasks_per_node = 1
#@ node = 1
#@ node_resources = ConsumableMemory(110GB)
#@ task_affinity = core(1)
#@ class= dev
#@ group= dev
#@ account_no = GFS-MTN
#@ wall_clock_limit = 1:00:00
#@ notification=error
#@ queue
set -x

ss2gg=/global/save/wx20mi/bin/ss2gg
sighdr=/nwprod/exec/global_sighdr

rundir=/stmp/$USER/testss2gg
mkdir -p $rundir
cd $rundir
adate=2012100100
savedir=/ptmp/$USER/ens1148/$adate
#savedir=/global/shared/dump/$adate/gdas

#hh='anl f00 f06'
hh='anl'
for fhr in $hh; do
sigfile=sig$fhr.gdas.$adate
sigbin=$sigfile.bin
sigctl=$sigfile.ctl
nlon=`echo lonb |$sighdr $savedir/$sigfile`
nlat=`echo latb |$sighdr $savedir/$sigfile`
$ss2gg $savedir/$sigfile $sigbin $sigctl 4 $nlon $nlat

done
