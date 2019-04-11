#!/bin/bash
#SBATCH -J convert_diags_jedi 
#SBATCH -A da-cpu
#SBATCH -q batch 
#SBATCH --nodes=1
#SBATCH -t 60:00
#SBATCH -o SLURM_%x.o%j
#SBATCH -e SLURM_%x.e%j
#SBATCH –mail-user=$LOGNAME@noaa.gov
OutDir=$1

# load modules here used to compile GSI
source /apps/lmod/7.7.18/init/sh

#module purge
### load modules
# system installed
module load intel
module load impi
module load netcdf
module load slurm
# /contrib modules
module use -a /contrib/modulefiles
module load anaconda/anaconda2

IODACDir=/scratch4/NCEPDEV/da/save/Cory.R.Martin/JEDI/src/ioda-converters_2/src/gsi-ncdiag

cd $IODACDir

mkdir -p $OutDir/obs
mkdir -p $OutDir/geovals

which python
python ./proc_gsi_ncdiag.py -n 24 -o $OutDir/obs -g $OutDir/geovals $OutDir/GSI_diags
