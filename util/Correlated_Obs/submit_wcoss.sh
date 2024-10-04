#!/bin/ksh

#WCOSS Job options
#BSUB -e comperr
#BSUB -o compout
#BSUB -J covcalc
#BSUB -q dev
#BSUB -M 50
#BSUB -openmp
#BSUB -n 16
#BSUB -W 01:00
#BSUB -R span[ptile=16]
#BSUB -P GFS-T2O
export OMP_NUM_THREADS=$LSB_DJOB_NUMPROC
corrdir=$PWD
cd ${corrdir}
./run.sh 

