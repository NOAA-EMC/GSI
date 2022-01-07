#!/bin/sh
#SBATCH -q urgent
#SBATCH --clusters c4
#SBATCH -t 16:00:00
#SBATCH -A nggps_psd
#SBATCH --nodes=20 --ntasks-per-node=2
#SBATCH -J NMC_bkerror
#SBATCH -e NMC_bkerror.err
#SBATCH -o NMC_bkerror.out

set -x

exp=C384_berror_enkfensjuly_smooth0p5

datdir=/lustre/f2/scratch/Jeffrey.S.Whitaker/staticB
calstats=/lustre/f2/dev/esrl/Jeffrey.S.Whitaker/GSI-github-jswhit/util/NMC_Bkerror/sorc/calcstats.exe
sststats=/lustre/f2/dev/Jeffrey.S.Whitaker/staticB/sst2dvar_stat0.5.ufs 

set -x

tmpdir=/lustre/f2/scratch/$LOGNAME/$exp
#rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

export OMP_NUM_THREADS=8
export OMP_STACKSIZE=256M

module load intel
module load impi

/bin/cp -f $calstats  ./stats.x
/bin/cp -f $sststats  ./berror_sst

#date1='2020010100'
#date2='2020020100'
date1='2020070100'
date2='2020080100'
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
module load PrgEnv-intel/6.0.5
module load cray-netcdf
module load hdf5-netcdf

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
