#/bin/sh
#@ job_name=berror_stats
#@ error=stats.e$(jobid)
#@ job_type=parallel
#@ network.MPI=csss,shared,us
#@ class=mtb
#@ group=mtb
#@ account_no = MTB015-RES
#@ wall_clock_limit=00:45:00
#@ notification=error
#@ node_usage = not_shared
#@ tasks_per_node=48
#@ node = 2
#@ node_resources = ConsumableMemory(110GB)
#@ task_affinity = cpu(1)
#@ parallel_threads = 1
#@ queue

export XLFRTEOPTS="buffering=disable_all"
export MP_COREFILE_FORMAT=lite

exp=stats_254L64
base=/jcsda/save/wx20kd
calstats=$base/gsi/dtk-berr/util/NMC_Bkerror/sorc/calcstats.exe
sststats=$base/gsi/dtk-berr/util/NMC_Bkerror/fix/sst2dvar_stat0.5

datdir=/jcsda/noscrub/wx20kd/sig254

set -x
tmpdir=/ptmp/wx20kd/stats/$exp
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

rcp $calstats  ./stats.x
rcp $sststats  ./berror_sst

cat << EOF > stats.parm
 &NAMSTAT
   jcap=254,jcapin=254,jcapsmooth=254,nsig=64,nlat=258,nlon=512,maxcases=100,hybrid=.true.,smoothdeg=0.5,
   biasrm=.true.,vertavg=.true.,
 /
EOF

set +x

ls $datdir/

ls $datdir/sigf24.gfs254.pair0* >> infiles
ls $datdir/sigf48.gfs254.pair0* >> infiles

set -x
ln -s -f infiles fort.10

poe hpmcount ./stats.x < stats.parm  > gsistats.out
rc=$?

rm $tmpdir/fort.1*
rm $tmpdir/fort.2*
rm $tmpdir/fort.3*
rm $tmpdir/fort.4*
rm $tmpdir/fort.5*
rm $tmpdir/fort.6*
rm $tmpdir/fort.7*
rm $tmpdir/fort.8*
rm $tmpdir/fort.9*
rm $tmpdir/fort.0*

exit
