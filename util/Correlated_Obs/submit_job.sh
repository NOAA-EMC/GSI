#!/bin/ksh

#Theia Job options
#PBS -o compout
#PBS -e comperr
#PBS -N covcalc
#PBS -q batch
#PBS -l walltime=04:00:00
#PBS -A cloud
#PBS -l nodes=1:ppn=16
#PBS -V

#WCOSS Job options
#BSUB -e comperr
#BSUB -o compout
#BSUB -J cov_calc
#BSUB -q dev2
#BSUB -openmp
#BSUB -n 16
#BSUB -W 01:00
#BSUB -R span[ptile=16]
#BSUB -P GFS-T2O
if [ ! -z "$PBS_NP" ] ; then
   export OMP_NUM_THREADS=$PBS_NP
fi
rundir=/scratch4/NCEPDEV/da/save/${USER}/GSI/trunk/util/Correlated_Obs
cd ${rundir}
./myrun.sh 

