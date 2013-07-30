#/bin/sh
#@ job_name=berror_stats
#@ error=stats.e$(jobid)
#@ job_type=parallel
#@ network.MPI=csss,shared,us
#@ class=devhigh
#@ group=devonprod
#@ account_no = GDAS-T2O
#@ environment = CHECKONE=one; CHECKTWO=two
#@ wall_clock_limit=01:00:00
#@ notification=error
#@ node_usage = not_shared
#@ tasks_per_node=45
#@ node = 8
#@ node_resources = ConsumableMemory(110GB)
#@ task_affinity = cpu(1)
#@ parallel_threads = 1
#@ queue

export XLFRTEOPTS="buffering=disable_all"
export MP_COREFILE_FORMAT=lite

exp=stats_382L64
base=/global/save/wx20kd
calstats=$base/gsi/dtk-berr/util/NMC_Bkerror/sorc/calcstats.exe
sststats=$base/gsi/dtk-berr/util/NMC_Bkerror/fix/sst2dvar_stat0.5

datdir=/global/noscrub/wx20kd/sig382/

set -x
tmpdir=/ptmp/wx20kd/stats/$exp
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

rcp $calstats  ./stats.x
rcp $sststats  ./berror_sst

cat << EOF > stats.parm
 &NAMSTAT
   jcap=382,jcapin=382,jcapsmooth=382,nsig=64,nlat=386,nlon=768,maxcases=450,hybrid=.true.,smoothdeg=0.5,
   biasrm=.true.,vertavg=.true.,
 /
EOF

set +x

ls $datdir/sigf24.*.200705* >> infiles
ls $datdir/sigf48.*.200705* >> infiles

ls $datdir/sigf24.*.200706* >> infiles
ls $datdir/sigf48.*.200706* >> infiles

ls $datdir/sigf24.*.200707* >> infiles
ls $datdir/sigf48.*.200707* >> infiles

ls $datdir/sigf24.*.200708* >> infiles
ls $datdir/sigf48.*.200708* >> infiles

ls $datdir/sigf24.*.200709* >> infiles
ls $datdir/sigf48.*.200709* >> infiles

ls $datdir/sigf24.*.200710* >> infiles
ls $datdir/sigf48.*.200710* >> infiles

ls $datdir/sigf24.*.200711* >> infiles
ls $datdir/sigf48.*.200711* >> infiles

ls $datdir/sigf24.*.200712* >> infiles
ls $datdir/sigf48.*.200712* >> infiles

ls $datdir/sigf24.*.200801* >> infiles
ls $datdir/sigf48.*.200801* >> infiles

ls $datdir/sigf24.*.200802* >> infiles
ls $datdir/sigf48.*.200802* >> infiles

ls $datdir/sigf24.*.200803* >> infiles
ls $datdir/sigf48.*.200803* >> infiles

ls $datdir/sigf24.*.200804* >> infiles
ls $datdir/sigf48.*.200804* >> infiles

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
