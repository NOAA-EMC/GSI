#!/bin/ksh
#Number of processors to run cov_calc on
NP=16

#Theia Job options
#PBS -o compout
#PBS -e comperr
#PBS -N covcalc
#PBS -q batch
#PBS -l walltime=01:00:00
#PBS -A cloud
#PBS -l nodes=1:ppn=$NP

#WCOSS Job options
#BSUB -e comperr
#BSUB -o compout
#BSUB -J covcalc
#BSUB -q dev2
#BSUB -openmp
#BSUB -n $NP
#BSUB -W 01:00
#BSUB -R span[ptile=$NP]
#BSUB -P GFS-T2O

export OMP_NUM_THREADS=$NP
corrdir=/scratch4/NCEPDEV/da/save/${USER}/GSI/trunk/util/Correlated_Obs
cd ${corrdir}
./run.sh 

