#!/bin/sh
#SBATCH -J bkgnmc_test
#SBATCH -t 0:30:00
#SBATCH --nodes=80 --ntasks-per-node=1
#SBATCH -q debug
#SBATCH -A gsienkf
#SBATCH -o bkgnmc_test.out
#SBATCH -e bkgnmc_test.err

set -x

exp=C384_berror_enkfens_smooth0p5

datdir=/scratch2/NCEPDEV/stmp1/Jeffrey.S.Whitaker/staticB
calstats=/scratch2/BMC/gsienkf/whitaker/gsi/GSI-github-jswhit/util/NMC_Bkerror/sorc/calcstats.exe
sststats=/scratch1/NCEPDEV/da/Catherine.Thomas/gsi/fix/sst2dvar_stat0.5.ufs 

set -x

tmpdir=/scratch2/NCEPDEV/stmp1/$LOGNAME/$exp
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

/bin/cp -f $calstats  ./stats.x
/bin/cp -f $sststats  ./berror_sst

date1='2020070100'
date2='2020070100'
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
   jcap=766,jcapin=766,jcapsmooth=382,nsig=127,nlat=770,nlon=1536,maxcases=${maxcases},hybrid=.true.,smoothdeg=0.5,
   biasrm=.true.,vertavg=.true.,use_gfs_ncio=.true.,use_enkf=.true.
 /
EOF

set -x
ln -s -f infiles fort.10

echo "I AM IN " $PWD

source $MODULESHOME/init/sh
module purge
module load intel impi
module load netcdf

srun $tmpdir/stats.x 

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
