#!/bin/ksh

#Theia Job options
#SBATCH -o compout
#SBATCH -e comperr
#SBATCH -J covcalc
#SBATCH -q batch
#SBATCH --time=01:00:00
#SBATCH -A da-cpu
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=16

export OMP_NUM_THREADS=$SLURM_NTASKS
corrdir=$PWD
cd ${corrdir}
./run.sh 

