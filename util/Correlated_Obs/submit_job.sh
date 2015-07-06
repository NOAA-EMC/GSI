#!/bin/ksh

#Zeus Job options
#PBS -o compout_airs
#PBS -e comperr_airs
#PBS -N covcalc
#PBS -q batch
#PBS -l walltime=04:00:00
#PBS -l procs=1
#PBS -A cloud 
#PBS -V

#WCOSS Job options
#BSUB -a poe
#BSUB -e comperr
#BSUB -o compout
#BSUB -J cov_calc
#BSUB -q dev
#BSUB -n 1
#BSUB -W 04:00
#BSUB -R affinity[core]
#BSUB -R span[ptile=1]
#BSUB -x
#BSUB -P GFS-T2O

./run.sh

