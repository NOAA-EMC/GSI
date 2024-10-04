#!/bin/ksh --login

# Set the queueing options 
#PBS -l procs=120
#PBS -l walltime=0:30:00
#PBS -A rtwbl
#PBS -q debug
#PBS -N wrf_gsi
#PBS -l partition=tjet
#PBS -j oe

set -x
np=$PBS_NP

# Load modules
module load intel
module load mvapich2
module load netcdf

set -x

cd /mnt/lfs3/projects/rtwbl/mhu/GSI_r1181/util/EnKF/enspreproc_regional.fd/run

/usr/bin/time mpiexec -envall -np ${np} /mnt/lfs3/projects/rtwbl/mhu/GSI_r1181/util/EnKF/enspreproc_regional.fd/enspreproc.x

exit 0
