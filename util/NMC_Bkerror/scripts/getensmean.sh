#!/bin/sh
#SBATCH -J getensmean
#SBATCH -t 0:30:00
#SBATCH --nodes=10 --ntasks-per-node=8
#SBATCH -q debug
#SBATCH -A gsienkf
#SBATCH -o getensmean.out
analdate=2020070100
execdir=/scratch2/BMC/gsienkf/whitaker/scripts/da_scripts/exec_hera
fh=06
charfhr="fhr`printf %02i $fh`"
nanals=80
/bin/rm -f sfg_${analdate}_${charfhr}_ensmean
srun ${execdir}/getsigensmeanp_smooth.x ./ sfg_${analdate}_${charfhr}_ensmean sfg_${analdate}_${charfhr} ${nanals} sfg_${analdate}_${charfhr}_enssprd
