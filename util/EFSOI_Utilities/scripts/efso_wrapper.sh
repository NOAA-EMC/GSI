#!/bin/sh
# -------------------------------------
# Wrapper script for efso file assembly
# for a provided time period
# -------------------------------------
set -ux
ndate=/scratch4/NCEPDEV/da/save/Michael.Lueken/nwprod/util/exec/ndate

# range of dates based on arguments
bdate=$1
ename=$2
edate=$3
cdate=$bdate
eft1=24
eft2=6

# Experiment name assignment
#ename=$3

while [[ $cdate -le $edate ]]; do

  pdate=`$ndate -6 $cdate`

  # Assemble files from parallel experiment
  ./efso_file_assembly.sh ${cdate} ${ename} ${eft1} ${eft2}
  while [ ! -f complete_file_assembly_${cdate} ]; do
    sleep 5
  done

  # Advance ensemble of recentered and inflated analyses
  ./efso_advance_ensemble.sh ${cdate} ${ename} ${bdate}
  while [ ! -f complete_advance_ensemble_${cdate} ]; do
    sleep 5
  done

  # Perform 24 hour efso calculation
  ./efso_run.sh ${cdate} ${ename} ${eft1}
  while [ ! -f complete_run_${cdate}_${eft1} ]; do
    sleep 5
  done
 
  adate=`$ndate +6 $cdate`
  cdate=$adate
done
