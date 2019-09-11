#!/bin/ksh --login
#SBATCH -J bkgnmc_test
#SBATCH -t 6:00:00
#SBATCH --nodes=20 --ntasks-per-node=4
#SBATCH -q batch
#SBATCH -A da-cpu
#SBATCH -o test.out_slurm

set -x

exp=nmc_master_t254
base=/scratch4/NCEPDEV/da/save/$LOGNAME

calstats=$base/gsi/ProdGSI/util/NMC_Bkerror/sorc/calcstats.exe
sststats=$base/gsi/fix/sst2dvar_stat0.5.ufs

datdir=/scratch4/NCEPDEV/stmp4/Daryl.Kleist/bkerrdat_every3rd

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

cat << EOF > stats.parm
 &NAMSTAT
   jcap=254,jcapin=574,jcapsmooth=254,nsig=64,nlat=258,nlon=512,maxcases=100,hybrid=.true.,smoothdeg=0.5,
   biasrm=.true.,vertavg=.true.,use_gfs_nemsio=.false.,
 /
EOF

ls $datdir/sigf24.gfs.* >> infiles
ls $datdir/sigf48.gfs.* >> infiles


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
