#!/bin/sh
# ------------------------------------------------------------
# Script to advance ensemble to Evaluation Forecast Time (EFT)
# and calculate the ensemble mean forecast at EFT.
# ------------------------------------------------------------

# Load modules for Theia machine
if [ x$PBS_O_WORKDIR != x ]; then
  cd $PBS_O_WORKDIR
  module load intel
  module load impi
  OMP_NUM_THREADS=1
fi

# Print commands to screen
set -x

# ----------
# Parameters 
# ----------
YMDH=$1
ENAME=$2
SDATE=$3
NENS=80

# Set the relevant date information
NDATE=/scratch4/NCEPDEV/da/save/Michael.Lueken/nwprod/util/exec/ndate
PYMDH=`${NDATE} -6 ${YMDH}`

# Working directory
WDIR=/scratch4/NCEPDEV/stmp4/${LOGNAME}/tmp.osense.${ENAME}.${YMDH}_0.75
cd ${WDIR}

# ---------------------------------------
# Directories for advancing the ensemble.
# ---------------------------------------
ADVANCE_CURRENT_ROTDIR=/scratch4/NCEPDEV/da/noscrub/David.Groff/${ENAME}_fcst/$YMDH
ADVANCE_CURRENT_RUNDIR=/scratch4/NCEPDEV/stmp3/${LOGNAME}/RUNDIRS/${ENAME}_fcst/$YMDH
ADVANCE_PREVIOUS_ROTDIR=/scratch4/NCEPDEV/da/noscrub/David.Groff/${ENAME}_fcst/$PYMDH
ADVANCE_PREVIOUS_RUNDIR=/scratch4/NCEPDEV/stmp3/${LOGNAME}/ROTDIRS/${ENAME}_fcst/$PYMDH
SANDBOX_DIR=$HOME/svn-work/GDAS/scripts

# Ensemble mean definition, again hardwired
# to Dave Groff's working copy for time being.
# As instructed in README file, please point
# to your executable.
EXEC_ENKF_STATS=/scratch4/NCEPDEV/da/save/David.Groff/${LOGNAME}/GSI/EXP-efso_3/util/EnKF/gfs/src/getsigensmeanp_smooth_ncep.fd/getsigensmeanp_smooth.x

#------------------------------------------
# Extend ensemble of forecasts from 
# the current and previous analysis.
# Provides ensemble perturbations
# at Evaluation Forecast Time (EFT)  [Xft]
#------------------------------------------

# Make directories necessary for advancing the ensembles
mkdir -p $ADVANCE_CURRENT_ROTDIR
mkdir -p $ADVANCE_CURRENT_RUNDIR
mkdir -p $ADVANCE_PREVIOUS_ROTDIR
mkdir -p $ADVANCE_PREVIOUS_RUNDIR

# Copy the Initial conditions ICs
# for the two ensembles of forecasts
#mv siganl_${YMDH}* $ADVANCE_CURRENT_ROTDIR
mv sfcanl_${YMDH}_ni* $ADVANCE_CURRENT_ROTDIR
#mv siganl_${PYMDH}* $ADVANCE_PREVIOUS_ROTDIR
mv sfcanl_${PYMDH}_ni* $ADVANCE_PREVIOUS_ROTDIR
mv sanl_${YMDH}_ni* $ADVANCE_CURRENT_ROTDIR
mv sanl_${PYMDH}_ni* $ADVANCE_PREVIOUS_ROTDIR

# Hold until all previous forecast 
# advances are complete
qstat | grep fcst_nimem > complete_previous
while [ -s complete_previous ]; do
  sleep 5
  qstat | grep fcst_nimem > complete_previous
done

CURRENT_ENSMEAN=/scratch4/NCEPDEV/stmp4/${LOGNAME}/tmp.osense.${ENAME}.${YMDH}_0.75/sigf24_${YMDH}_ensmean
PREVIOUS_ENSMEAN=/scratch4/NCEPDEV/stmp4/${LOGNAME}/tmp.osense.${ENAME}.${PYMDH}_0.75/sigf24_${PYMDH}_ensmean
#PREVIOUS_ENSMEAN_CHECK=/scratch4/NCEPDEV/stmp4/${LOGNAME}/tmp.osense.${ENAME}.${PYMDH}_0.75/sigf30_${PYMDH}_ensmean
CURRENT_ENSMEAN2=/scratch4/NCEPDEV/stmp4/${LOGNAME}/tmp.osense.${ENAME}.${YMDH}_0.75/sigf06_${YMDH}_ensmean
CURRENT_MEMBER=/scratch4/NCEPDEV/da/noscrub/${LOGNAME}/${ENAME}_fcst/${YMDH}/sigf24_${YMDH}_nimem080
PREVIOUS_MEMBER=/scratch4/NCEPDEV/da/noscrub/${LOGNAME}/${ENAME}_fcst/${PYMDH}/sigf24_${PYMDH}_nimem080

CURRENT_ENSMEAN_CHECK=/scratch4/NCEPDEV/da/noscrub/${LOGNAME}/${ENAME}_fcst/${YMDH}/sigf24_${YMDH}_ensmean
PREVIOUS_ENSMEAN_CHECK=/scratch4/NCEPDEV/da/noscrub/${LOGNAME}/${ENAME}_fcst/${PYMDH}/sigf30_${PYMDH}_ensmean

# Extend ensemble of forecasts from analysis where necessary
if [ ! -f $CURRENT_ENSMEAN -a ! -f $CURRENT_MEMBER ] ; then
cat <<EOF > advance_current_ensemble.sh
#!/bin/sh --login
module load impi
module load anaconda
advance_ensemble.j $SANDBOX_DIR/param_EFSO.j $YMDH
exit
EOF
chmod 755 advance_current_ensemble.sh
./advance_current_ensemble.sh
#while [ ! -f $CURRENT_MEMBER ]; do
#  sleep 5
#done
fi

# Hold until all previous forecast 
# advances are complete
#qstat | grep sofcst > complete_previous
#while [ -s complete_previous ]; do
#  sleep 5
#  qstat | grep sofcst > complete_previous
#done

# Extend ensemble of forecasts from previous analysis where necessary
if [ ! -s $PREVIOUS_ENSMEAN -a ! -s $PREVIOUS_MEMBER ] ; then
cat <<EOF > advance_previous_ensemble.sh
#!/bin/sh --login
module load impi
module load anaconda
advance_ensemble.j $SANDBOX_DIR/param_EFSO.j $PYMDH
exit
EOF
chmod 755 advance_previous_ensemble.sh
./advance_previous_ensemble.sh
while [ ! -s $PREVIOUS_MEMBER ]; do
  sleep 5
done
else
cp /scratch4/NCEPDEV/stmp4/${LOGNAME}/tmp.osense.${ENAME}.${PYMDH}_0.75/sigf30_${PYMDH}* ${ADVANCE_PREVIOUS_ROTDIR}
#cp /scratch4/NCEPDEV/stmp4/${LOGNAME}/tmp.osense.${ENAME}.${PYMDH}_0.75/sigf12_${PYMDH}* ${ADVANCE_PREVIOUS_ROTDIR}
fi

rm complete_stats_curr_24
rm ensemble_stats.sh

# For the forecast from the (current) analysis calculate the ensemble mean.
if [ ! -f $CURRENT_ENSMEAN ] ; then
cat <<EOF > ensemble_stats.sh
#!/bin/sh --login
#PBS -N getsigensstatp
#PBS -A hybrid
#PBS -q batch
#PBS -l walltime=00:05:00
#PBS -l nodes=14:ppn=6
#PBS -j oe
if [ x\$PBS_O_WORKDIR != x ]; then
  cd \$PBS_O_WORKDIR
  module load intel
  module load impi
fi
module load impi
mpirun -np ${NENS} ${EXEC_ENKF_STATS} ${ADVANCE_CURRENT_ROTDIR}/ sigf24_${YMDH}_ensmean sigf24_${YMDH} ${NENS}
rc=\$?
echo "\${rc}" > complete_stats_curr_24
exit
EOF
qsub ensemble_stats.sh
while [ ! -s $CURRENT_ENSMEAN_CHECK ]; do
  sleep 5
done
fi

rm ensemble_stats.sh
rm complete_stats_curr_6

# For the forecast from analysis calculate 6 hour ensemble mean
if [ ! -f $CURRENT_ENSMEAN2 ] ; then
cat <<EOF > ensemble_stats.sh
#!/bin/sh --login
#PBS -N getsigensstatp
#PBS -A hybrid
#PBS -q debug
#PBS -l walltime=00:05:00
#PBS -l nodes=14:ppn=6
#PBS -j oe
if [ x\$PBS_O_WORKDIR != x ]; then
  cd \$PBS_O_WORKDIR
  module load intel
  module load impi
fi
module load impi
mpirun -np ${NENS} ${EXEC_ENKF_STATS} ${ADVANCE_CURRENT_ROTDIR}/ sigf06_${YMDH}_ensmean sigf06_${YMDH} ${NENS}
rc=\$?
echo "\${rc}" > complete_stats_curr_6
exit
EOF
#qsub ensemble_stats.sh
#while [ ! -s complete_stats_curr_6 ]; do
#  sleep 5
#done
fi

rm ensemble_stats.sh
rm complete_stats_previous_30

# For the forecasts from previous analysis, calculate the ensemble mean.
cat <<EOF > ensemble_stats.sh
#!/bin/sh --login
#PBS -N getsigensstatp
#PBS -A hybrid
#PBS -q batch
#PBS -l walltime=00:05:00
#PBS -l nodes=14:ppn=6
#PBS -j oe
if [ x\$PBS_O_WORKDIR != x ]; then
  cd \$PBS_O_WORKDIR
  module load intel
  module load impi
fi
module load impi
mpirun -np ${NENS} ${EXEC_ENKF_STATS} ${ADVANCE_PREVIOUS_ROTDIR}/ sigf30_${PYMDH}_ensmean sigf30_${PYMDH} ${NENS}
rc=\$?
echo "\${rc}" > complete_stats_previous_30
exit
EOF
if [ ! -s $PREVIOUS_ENSMEAN_CHECK ]; then
qsub ensemble_stats.sh
fi
while [ ! -s $PREVIOUS_ENSMEAN_CHECK ]; do
  sleep 5
done

rm ensemble_stats.sh
rm complete_stats_previous_12

cat <<EOF > ensemble_stats.sh
#!/bin/sh --login
#PBS -N getsigensstatp
#PBS -A hybrid
#PBS -q debug
#PBS -l walltime=00:05:00
#PBS -l nodes=14:ppn=6
#PBS -j oe
if [ x\$PBS_O_WORKDIR != x ]; then
  cd \$PBS_O_WORKDIR
  module load intel
  module load impi
fi
module load impi
mpirun -np ${NENS} ${EXEC_ENKF_STATS} ${ADVANCE_PREVIOUS_ROTDIR}/ sigf12_${PYMDH}_ensmean sigf12_${PYMDH} ${NENS}
rc=\$?
echo "\${rc}" > complete_stats_previous_12
exit
EOF
#qsub ensemble_stats.sh
#while [ ! -s complete_stats_previous_12 ]; do
#  sleep 5
#done

# Move forecasts from previous ROTDIR to $WDIR
mv $ADVANCE_PREVIOUS_ROTDIR/*ensmean $WDIR

# Move forecasts from current ROTDIR to WDIR
# (This will become a move after confirmation that the whole EFSO process works)
if [ ! -f $CURRENT_ENSMEAN ] ; then
cp $ADVANCE_CURRENT_ROTDIR/sigf24* $WDIR
#cp $ADVANCE_CURRENT_ROTDIR/sigf06* $WDIR
fi

SCRIPTS_DIR=/scratch4/NCEPDEV/da/save/David.Groff/David.Groff/GSI/jianjun_test_script/scripts_ncep
echo "\${rc}" > complete_advance_ensemble_${YMDH} 
cd ${SCRIPTS_DIR}
mv ${WDIR}/complete_advance_ensemble_${YMDH} ${SCRIPTS_DIR}

exit
