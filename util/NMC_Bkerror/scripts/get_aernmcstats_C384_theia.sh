#!/bin/bash
#SBATCH -A da-cpu
#SBATCH -J FV3AeroNMCStats
#SBATCH -q batch
#SBATCH -o SLURM_%x.o%j
#SBATCH -e SLURM_%x.e%j
#SBATCH --export=ALL
#SBATCH --time=02:00:00
#SBATCH --nodes=8
#SBATCH --tasks-per-node=1

set -x
export NTHREADS=2

export exp="test_FV3AeroNMCStats_386"
export base=/scratch4/NCEPDEV/da/save/Cory.R.Martin/GSI/ProdGSI/util/NMC_Bkerror/
export calcstats=$base/sorc_aero/calcstats_aerosol.exe
export datadir=/scratch4/NCEPDEV/da/noscrub/Cory.R.Martin/FV3GFS-GSDChem/
export season='test'
export resin='C384'
export tmpdir=/scratch3/NCEPDEV/stmp1/Cory.R.Martin/FV3GFS-GSDChem/tmp.$exp.$season
export outdir=/scratch3/NCEPDEV/stmp1/Cory.R.Martin/FV3GFS-GSDChem/Berror.$exp.$season

case $season in
# note, this is not complete yet
  'MAM')
    export y4m2="201903 201904 201905"
    ;;
  'JJA')
    export y4m2="201903 201904 201905"
    ;;
  'SON')
    export y4m2="201903 201904 201905"
    ;;
  'DJF')
    export y4m2="201903 201904 201905"
    ;;
  'Year')
    export y4m2="201903 201904 201905"
    ;;
  'test')
    export y4m2="201904 201905"
    ;;
esac


if [ -d $tmpdir ]; then
  rm -rf $tmpdir
fi

mkdir -p $tmpdir
cd $tmpdir

cp $calcstats ./stats.x

#jcap=766,jcapin=766,jcapsmooth=766,nsig=64,nlat=386,nlon=768,maxcases=200,hybrid=.true.,smoothdeg=0.5,
cat << EOF > stats.parm
 &NAMSTAT
   jcap=766,jcapin=766,jcapsmooth=766,nsig=64,nlat=768,nlon=1536,maxcases=200,hybrid=.true.,smoothdeg=0.5,
   biasrm=.true.,vertavg=.true.,use_nemsio=.true.,modelname='fv3'
 /
EOF

for hh in 024 048; do
  for ymflag in $y4m2; do
    ls $datadir/$resin/$ymflag*.gfs.t*z.atmf"$hh".nemsio >> infiles
    #ls $datadir/gfs.$ymflag*/$resin/*.gfs.t*z.atmf"$hh".nemsio >> infiles
  done
done

ln -sf infiles fort.10

### load modules
source /apps/lmod/7.7.18/init/sh
# system installed
module load intel
module load impi
module load netcdf
module load grads
module load rocoto/1.3.0-RC3
# /contrib modules
module use -a /contrib/modulefiles
module load anaconda/anaconda2
# /contrib/da modules
module use -a /contrib/da/modulefiles
module load boost
module load eigen
# my modules
module use -a /scratch4/NCEPDEV/da/save/Cory.R.Martin/modulefiles
#   NCEPLIBS
module use -a /scratch3/NCEPDEV/nwprod/lib/modulefiles
module load nemsio
module load bacio
module load w3nco
module load crtm/v2.2.3
module load sp

export MPI_BUFS_PER_PROC=2048
export MPI_BUFS_PER_HOST=2048
export MPI_GROUP_MAX=256
export MPI_MEMMAP_OFF=1
export MP_STDOUTMODE=ORDERED
export OMP_NUM_THREADS=$NTHREADS
export KMP_STACKSIZE=512MB   #2048000
export KMP_AFFINITY=scatter
export APRUN="srun"
ulimit -s unlimited

$APRUN ./stats.x < stats.parm

if [ -s gsir4.berror_stats.gcv ] && [ -s bgstats_sp.grd ]; then
   echo "Generate NMC statistic error successfully"
   if [ ! -d $outdir ]; then
       mkdir $outdir
   else
       rm -rf $outdir
   fi
   mv gsir4.berror_stats.gcv $outdir/.
   mv bgstats_sp.grd $outdir/.
#   mv biascor.grd $outdir
else
   echo "Failed to generate NMC statistic error"
fi

exit
