#!/bin/ksh

#Theia Job options
#PBS -o compout
#PBS -e comperr
#PBS -N covcalc
#PBS -q batch
#PBS -l walltime=04:00:00
#PBS -A cloud
#PBS -l procs=1
#PBS -V

#WCOSS Job options
#BSUB -e comperr
#BSUB -o compout
#BSUB -J cov_calc
#BSUB -q dev
#BSUB -n 1
#BSUB -W 01:00
#BSUB -R affinity[core]
#BSUB -R span[ptile=1]
#BSUB -x
#BSUB -P GFS-T2O

rundir=/da/save/${USER}/GSI/Desroziers/util/Correlated_Obs/
cd ${rundir}
./run.sh 

