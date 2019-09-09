#!/bin/ksh --login
#SBATCH -J bkgnmc_test
#SBATCH -t 0:30:00
#SBATCH --nodes=20 --ntasks-per-node=4
#SBATCH -q debug
#SBATCH -A gsienkf
#SBATCH -o test.out_slurm

set -x

exp=C192_enkf_ens
base=/scratch3/BMC/gsienkf/whitaker/sfgens

calstats=$base/calcstats.exe
sststats=/scratch4/NCEPDEV/da/save/Catherine.Thomas/gsi/fix/sst2dvar_stat0.5.ufs

datdir=$base

set -x

tmpdir=/scratch3/NCEPDEV/stmp1/$LOGNAME/nmc/$exp
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

export MPI_BUFS_PER_PROC=256
export MPI_BUFS_PER_HOST=256
export MPI_GROUP_MAX=256
export OMP_NUM_THREADS=1
export OMP_STACKSIZE=1024M
export I_MPI_ADJUST_GATHERV=3
export PSM2_MQ_RECVREQS_MAX=4000000

module load intel
module load impi

cp $calstats  ./stats.x
cp $sststats  ./berror_sst

date1='2016010200'
date2='2016010300'
date=$date1
/bin/rm -f infiles
touch infiles
# ens member files go first
while [ $date -le $date2 ]; do
  ls $datdir/sfg*${date}*mem* >> infiles
  date=`incdate $date 24`
done
# then corresponding ens mean files
date=$date1
while [ $date -le $date2 ]; do
  for filename in $datdir/sfg*${date}*mem*; do
     echo "$datdir/sfg_${date}_fhr06_ensmean" >> infiles
  done
  date=`incdate $date 24`
done

maxcases=`wc -l infiles | cut -f1 -d " "`

cat << EOF > stats.parm
 &NAMSTAT
   jcap=382,jcapin=382,jcapsmooth=382,nsig=64,nlat=386,nlon=768,maxcases=${maxcases},hybrid=.true.,smoothdeg=0.5,
   biasrm=.true.,vertavg=.true.,use_gfs_nemsio=.true.,use_enkf=.true.
 /
EOF

set -x
ln -s -f infiles fort.10

echo "I AM IN " $PWD

eval "srun $tmpdir/stats.x < stats.parm > nmcstats.out"

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
