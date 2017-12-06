#!/bin/sh
# -------------------------------
# Script to assemble files needed
# to perform EFSO calculations
# -------------------------------

# Print commands to screen
set -x

# --------------------------------------------
# General definitions for both operational and
# parallel experiment context
# --------------------------------------------

# -------------------------------------------------------
# Parameters (might add use of EFT flexibility in future)
# -------------------------------------------------------
YMDH=$1
ENAME=$2
EFT=$3
EFT2=$4
NENS=80

# Working directory
WDIR=/scratch4/NCEPDEV/stmp4/${LOGNAME}/tmp.osense.${ENAME}.${YMDH}_0.75
mkdir -p ${WDIR}
rc=$?
if [ $rc -ne 0 ]; then
  "Cannot make ${WDIR}"
  exit
fi
cd ${WDIR}

# Scripts Directory
SCRIPTS_DIR=/scratch4/NCEPDEV/da/save/David.Groff/David.Groff/GSI/jianjun_test_script/scripts_ncep

# Set the relevant date information
NDATE=/scratch4/NCEPDEV/da/save/Michael.Lueken/nwprod/util/exec/ndate
PYMDH=`${NDATE} -6 ${YMDH}`
FYMDH=`${NDATE} +${EFT} ${YMDH}`
FYMDH2=`${NDATE} +${EFT2} ${YMDH}`
EFTP=`expr ${EFT} + 6`

# GSI fix file directory
FIXDIR=/home/${LOGNAME}/Work_Dir/GSI/EXP-efso_cycling/fix

# ---------------------------------------------------------------------
# Context specific definitions (i.e Operational or parallel experiment)
# ---------------------------------------------------------------------
if [ "$ENAME" = "Operational" ]; then
  # --------------------------------------------------
  # Necessary time information for Operational context
  # --------------------------------------------------
  YEAR="${YMDH:0:4}"
  MONTH="${YMDH:4:2}"
  DAY="${YMDH:6:2}"
  HOUR="${YMDH:8:2}"
  YEARMONTH="${YMDH:0:6}"
  PRIORYEAR="${PYMDH:0:4}"
  FCSTYEAR="${FYMDH:0:4}"
  # Previous year, month
  PRYM="${PYMDH:0:6}"
  # Fcst year, month
  FCYM="${FYMDH:0:6}"
  # Previous hour
  PHOUR="${PYMDH:8:2}"
  # (previous time) year, month, day
  PYMD="${PYMDH:0:8}"
  # year, month, day
  YMD="${YMDH:0:8}"
  # (forecast time) year, month, day
  FYMD="${FYMDH:0:8}"
  
  # Operations HSI_DIR settings
  HSI_DIR_PREVIOUS=/NCEPPROD/hpssprod/runhistory/rh${PRIORYEAR}/${PRYM}
  HSI_DIR_CURRENT=/NCEPPROD/hpssprod/runhistory/rh${YEAR}/${YEARMONTH}
  HSI_DIR_EFT=/NCEPPROD/hpssprod/runhistory/rh${FCSTYEAR}/${FCYM}

  # Specify files to be copied from HPSS
  HFILE1=${HSI_DIR_CURRENT}/${YMD}/com_gfs_prod_enkf.${YMD}_${HOUR}.anl.tar
  HFILE2=${HSI_DIR_CURRENT}/${YMD}/com_gfs_prod_gdas.${YMDH}.tar
  HFILE3=${HSI_DIR_EFT}/${FYMD}/com_gfs_prod_enkf.${FYMD}_${HOUR}.anl.tar
  HFILE4=${HSI_DIR_PREVIOUS}/${PYMD}/com_gfs_prod_enkf.${PYMD}_${PHOUR}.fcs.tar
  HFILE5=${HSI_DIR_CURRENT}/${YMD}/com_gfs_prod_enkf.${YMD}_${HOUR}.omg.tar
  HFILE6=${HSI_DIR_PREVIOUS}/${PYMD}/com_gfs_prod_enkf.${PYMD}_${PHOUR}.anl.tar
else 
  # Parallel experiment context
  HSI_DIR=/NCEPDEV/emc-da/1year/${LOGNAME}/WCOSS/${ENAME}
  HFILE1=${HSI_DIR}/${YMDH}gdas.enkf.anl.tar
  HFILE2=${HSI_DIR}/${YMDH}gdas.tar
  HFILE3=${HSI_DIR}/${FYMDH}gdas.enkf.anl.tar
  HFILE4=${HSI_DIR}/${PYMDH}gdas.enkf.fcs06.tar
  HFILE5=${HSI_DIR}/${YMDH}gdas.enkf.obs.tar
  HFILE6=${HSI_DIR}/${PYMDH}gdas.enkf.anl.tar
  HFILE7=${HSI_DIR}/${FYMDH2}gdas.enkf.anl.tar
fi

# ----------------------------------------------------
# gather files for observation sensitivity calculation
# ----------------------------------------------------

#htar -xmvf ${HFILE1} sanl_${YMDH}_ensmean osense_${YMDH}.dat
#exit

# 1) Link in necessary fix files
ln -s ${FIXDIR}/global_convinfo.txt            ./convinfo
ln -s ${FIXDIR}/global_satinfo.txt             ./satinfo
ln -s ${FIXDIR}/global_ozinfo.txt              ./ozinfo
ln -s ${FIXDIR}/global_hybens_locinfo.l${NLEV}.txt ./hybens_locinfo
ln -s ${FIXDIR}/global_satangbias.txt     ./satbias_angle

# 2) Collect files output from your parallel experiment

# Analysis at initial time is used to better locate
# the response of the forecast to the observation
# at forecast time (evolving localization).
htar -xmvf ${HFILE1} sanl_${YMDH}_ensmean osense_${YMDH}.dat
rc=$?
if [ $rc -ne 0 ]; then
  echo "Error: ${HFILE1}"
  exit 1
fi

# Get bias correction file.
if [ "$ENAME" = "Operational" ]; then
  htar -xmvf ${HFILE2} gdas1.t${HOUR}z.abias
  rc=$?
  if [ $rc -ne 0 ]; then
    echo "Error: ${HFILE2}"
    exit 1
  fi
  ln -s gdas1.t${HOUR}z.abias satbias_in
else
  htar -xmvf ${HFILE2} biascr.gdas.${YMDH}
  rc=$?
  if [ $rc -ne 0 ]; then
    echo "Error: ${HFILE2}"
    exit 1
  fi
  ln -s biascr.gdas.${YMDH} satbias_in
fi

# Analysis at forecast time, used to evaluate the forecast (i.e. necessary for ef + eg) 
htar -xmvf ${HFILE3} sanl_${FYMDH}_ensmean
#htar -xmvf ${HFILE7} sanl_${FYMDH2}_ensmean
rc=$?
if [ $rc -ne 0 ]; then
  echo "Error: ${HFILE3}"
  exit 1
fi

# first guess forecasts from $PYMDH, used for grid information
htar -xmvf ${HFILE4} sfg_${PYMDH}_fhr06_ensmean
rc=$?
if [ $rc -ne 0 ]; then
  echo "Error: ${HFILE4}"
  exit 1
fi

# Extract observation files and ICs needed to advance the ensembles
# a) Extract ensmean diag stats used as innovation in EFSO formula.
htar -xmvf ${HFILE5} radstat_${YMDH}_ensmean
htar -xmvf ${HFILE5} oznstat_${YMDH}_ensmean
htar -xmvf ${HFILE5} cnvstat_${YMDH}_ensmean
tar -xvf radstat_${YMDH}_ensmean
tar -xvf oznstat_${YMDH}_ensmean
tar -xvf cnvstat_${YMDH}_ensmean
if [ $rc -ne 0 ]; then
  echo "Error: ${HFILE5}"
  exit 1
fi
# b) Extract ensemble members from files to obtain 
#    the analysis perturbations in obs space (HX).
IMEM=1
while [ ${IMEM} -le ${NENS} ]; do
  if [ ${IMEM} -le 9 ]; then
    CMEM="00${IMEM}"
  elif [ ${IMEM} -le 99 ]; then
    CMEM="0${IMEM}"
  else
    CMEM="${IMEM}"
  fi
  # Initial condition files needed for generating two
  # sets of ensemble forecasts valid at the same time (see below)
  htar -xmvf ${HFILE1} sfcanl_${YMDH}_mem${CMEM}
  htar -xmvf ${HFILE6} sfcanl_${PYMDH}_mem${CMEM}
  mv sfcanl_${PYMDH}_mem${CMEM} sfcanl_${PYMDH}_nimem${CMEM}
  mv sfcanl_${YMDH}_mem${CMEM} sfcanl_${YMDH}_nimem${CMEM}
  htar -xmvf ${HFILE1} sanl_${YMDH}_nimem${CMEM}
  htar -xmvf ${HFILE6} sanl_${PYMDH}_nimem${CMEM}
  IMEM=`expr ${IMEM} + 1`
done
rc=$?
if [ $rc -ne 0 ]; then
  echo "Error: ${HFILE6}"
  exit 1
fi

echo "\${rc}" > complete_file_assembly_${YMDH}
cd ${SCRIPTS_DIR}
mv ${WDIR}/complete_file_assembly_${YMDH} ${SCRIPTS_DIR}

exit
