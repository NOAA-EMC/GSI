#!/bin/bash
#SBATCH -J convert_diags_jedi 
#SBATCH -A da-cpu
#SBATCH -q batch 
#SBATCH --nodes=1
#SBATCH -t 2:00:00
#SBATCH –mail-user=$LOGNAME@noaa.gov
#SBATCH -o SLURM_%x.o%j
#SBATCH -e SLURM_%x.e%j
OutDir=$1

# load modules here used to compile GSI
source /apps/lmod/7.7.18/init/sh

#module purge
### load modules
# system installed
module load intel
module load impi
module load netcdf
# /contrib modules
module use -a /contrib/modulefiles
module load anaconda/anaconda3-5.3.1

IODACDir=/scratch4/NCEPDEV/da/save/Cory.R.Martin/JEDI/src/ioda-converters_2/build/bin

cd $IODACDir

mkdir -p $OutDir/obs
mkdir -p $OutDir/geoval

python ./proc_gsi_ncdiag.py -n 24 -o $OutDir/obs -g $OutDir/geoval $OutDir/GSI_diags > $OutDir/log.proc_gsi_ncdiag

# subset obs
python ./subset_files.py -n 24 -m $OutDir/obs -g $OutDir/geoval
python ./subset_files.py -n 24 -s $OutDir/obs -g $OutDir/geoval
