#!/bin/sh
# -------------------------------------------------
# Script for running observation impact computation
# using Global EnKF (GFS-EnKF) products
# -------------------------------------------------

# Load modules for Theia machine
if [ x$PBS_O_WORKDIR != x ]; then
  cd $PBS_O_WORKDIR
  module load intel
  module load impi
  OMP_NUM_THREADS=1
fi

# Print commands to screen
set -x

# -----------------------------
# Parameters for test case
# this needs to be interactive
# -----------------------------
YMDH=$1
ENAME=$2
EFT=$3
NENS=80
NLON=512         # Number of longitude
NLAT=256         # Number of latitude
NLEV=64          # Number of vertical levels
NVARS=5          # Number of variables

# Set the relevant date information
NDATE=/scratch4/NCEPDEV/da/save/Michael.Lueken/nwprod/util/exec/ndate
PYMDH=`${NDATE} -6 ${YMDH}`
FYMDH=`${NDATE} +${EFT} ${YMDH}`
EFTP=`expr ${EFT} + 6`

# Target area
MINLAT=${MINLAT:=-90}
MAXLAT=${MAXLAT:=90}
MINLON=${MINLON:=0}
MAXLON=${MAXLON:=360}

# Working directory
WDIR=/scratch4/NCEPDEV/stmp4/${LOGNAME}/tmp.osense.${ENAME}.${YMDH}_0.75
mkdir -p ${WDIR}
rc=$?
if [ $rc -ne 0 ]; then
  "Cannot make ${WDIR}"
  exit
fi
cd ${WDIR}

# --------
# Programs
# --------
#EXECENKF=/home/${LOGNAME}/Work_Dir/GSI/EXP-efso_3/src/enkf/global_enkf
#EXECENKF=/home/${LOGNAME}/Work_Dir/GSI/EXP-efso_cycling/src/enkf/global_enkf
EXECENKF=/home/${LOGNAME}/Work_Dir/GSI/EXP-efso_features/src/enkf/global_enkf
if [ ! -x ${EXECENKF} ] ; then
  echo "Invalid program for ${EXECENKF}"
  exit 1
fi
FIXDIR=/home/${LOGNAME}/Work_Dir/GSI/EXP-efso_cycling/fix
# Make links. Need to clean this up. These links should
# be tied to the GSI version applied in the parallel experiment
# will generalize later.
#ln -s ${FIXDIR}/global_convinfo.txt            ./convinfo
#ln -s ${FIXDIR}/global_satinfo.txt             ./satinfo
#ln -s ${FIXDIR}/global_ozinfo.txt              ./ozinfo
#ln -s ${FIXDIR}/global_hybens_locinfo.l${NLEV}.txt ./hybens_locinfo
#ln -s ${FIXDIR}/global_satangbias.txt     ./satbias_angle
#ln -s gdas1.t00z.abias                    ./satbias_in

# Create global_enkf namelist
cat <<EOF > enkf.nml
 &nam_enkf
  datestring="$YMDH",gdatestring="$PYMDH",datapath="$WDIR/",
  andataname="sanl_${FYMDH}_ensmean",
  analpertwtnh=0.85,analpertwtsh=0.85,analpertwttr=0.85,
  covinflatemax=1.e2,covinflatemin=1,pseudo_rh=.true.,iassim_order=0,
  corrlengthnh=2000,corrlengthsh=2000,corrlengthtr=2000,
  lnsigcutoffnh=2.0,lnsigcutoffsh=2.0,lnsigcutofftr=2.0,
  lnsigcutoffpsnh=2.0,lnsigcutoffpssh=2.0,lnsigcutoffpstr=2.0,
  lnsigcutoffsatnh=2.0,lnsigcutoffsatsh=2.0,lnsigcutoffsattr=2.0,
  obtimelnh=1.e30,obtimelsh=1.e30,obtimeltr=1.e30,
  saterrfact=1.0,numiter=1,
  sprd_tol=1.e30,paoverpb_thresh=0.98,
  nlons=${NLON},nlats=${NLAT},nlevs=${NLEV},nanals=${NENS},nvars=${NVARS},
  deterministic=.true.,sortinc=.true.,lupd_satbiasc=.false.,
  reducedgrid=.true.,readin_localization=.false.,
  simple_partition=.true.,letkf_flag=.false.,
  fso_flag=.true.,evalft=${EFT},wmoist=1.0,adrate=0.75,
  tar_minlat=${MINLAT},tar_maxlat=${MAXLAT},tar_minlon=${MINLON},tar_maxlon=${MAXLON},
  tar_minlev=1,tar_maxlev=64,
  univaroz=.false.,adp_anglebc=.true.,angord=4,use_edges=.false.,emiss_bc=.true.,
  $NAM_ENKF
 /
 &END
 &satobs_enkf
  sattypes_rad(1) = 'amsua_n15',     dsis(1) = 'amsua_n15',
  sattypes_rad(2) = 'amsua_n18',     dsis(2) = 'amsua_n18',
  sattypes_rad(3) = 'amsua_n19',     dsis(3) = 'amsua_n19',
  sattypes_rad(4) = 'amsub_n16',     dsis(4) = 'amsub_n16',
  sattypes_rad(5) = 'amsub_n17',     dsis(5) = 'amsub_n17',
  sattypes_rad(6) = 'amsua_aqua',    dsis(6) = 'amsua_aqua',
  sattypes_rad(7) = 'amsua_metop-a', dsis(7) = 'amsua_metop-a',
  sattypes_rad(8) = 'airs_aqua',     dsis(8) = 'airs281SUBSET_aqua',
  sattypes_rad(9) = 'hirs3_n17',     dsis(9) = 'hirs3_n17',
  sattypes_rad(10)= 'hirs4_n19',     dsis(10)= 'hirs4_n19',
  sattypes_rad(11)= 'hirs4_metop-a', dsis(11)= 'hirs4_metop-a',
  sattypes_rad(12)= 'mhs_n18',       dsis(12)= 'mhs_n18',
  sattypes_rad(13)= 'mhs_n19',       dsis(13)= 'mhs_n19',
  sattypes_rad(14)= 'mhs_metop-a',   dsis(14)= 'mhs_metop-a',
  sattypes_rad(15)= 'goes_img_g11',  dsis(15)= 'imgr_g11',
  sattypes_rad(16)= 'goes_img_g12',  dsis(16)= 'imgr_g12',
  sattypes_rad(17)= 'goes_img_g13',  dsis(17)= 'imgr_g13',
  sattypes_rad(18)= 'goes_img_g14',  dsis(18)= 'imgr_g14',
  sattypes_rad(19)= 'goes_img_g15',  dsis(19)= 'imgr_g15',
  sattypes_rad(20)= 'avhrr3_n18',    dsis(20)= 'avhrr3_n18',
  sattypes_rad(21)= 'avhrr3_metop-a',dsis(21)= 'avhrr3_metop-a',
  sattypes_rad(22)= 'avhrr3_n19',    dsis(22)= 'avhrr3_n19',
  sattypes_rad(23)= 'amsre_aqua',    dsis(23)= 'amsre_aqua',
  sattypes_rad(24)= 'ssmis_f16',     dsis(24)= 'ssmis_f16',
  sattypes_rad(25)= 'ssmis_f17',     dsis(25)= 'ssmis_f17',
  sattypes_rad(26)= 'ssmis_f18',     dsis(26)= 'ssmis_f18',
  sattypes_rad(27)= 'ssmis_f19',     dsis(27)= 'ssmis_f19',
  sattypes_rad(28)= 'ssmis_f20',     dsis(28)= 'ssmis_f20',
  sattypes_rad(29)= 'sndrd1_g11',    dsis(29)= 'sndrD1_g11',
  sattypes_rad(30)= 'sndrd2_g11',    dsis(30)= 'sndrD2_g11',
  sattypes_rad(31)= 'sndrd3_g11',    dsis(31)= 'sndrD3_g11',
  sattypes_rad(32)= 'sndrd4_g11',    dsis(32)= 'sndrD4_g11',
  sattypes_rad(33)= 'sndrd1_g12',    dsis(33)= 'sndrD1_g12',
  sattypes_rad(34)= 'sndrd2_g12',    dsis(34)= 'sndrD2_g12',
  sattypes_rad(35)= 'sndrd3_g12',    dsis(35)= 'sndrD3_g12',
  sattypes_rad(36)= 'sndrd4_g12',    dsis(36)= 'sndrD4_g12',
  sattypes_rad(37)= 'sndrd1_g13',    dsis(37)= 'sndrD1_g13',
  sattypes_rad(38)= 'sndrd2_g13',    dsis(38)= 'sndrD2_g13',
  sattypes_rad(39)= 'sndrd3_g13',    dsis(39)= 'sndrD3_g13',
  sattypes_rad(40)= 'sndrd4_g13',    dsis(40)= 'sndrD4_g13',
  sattypes_rad(41)= 'sndrd1_g14',    dsis(41)= 'sndrD1_g14',
  sattypes_rad(42)= 'sndrd2_g14',    dsis(42)= 'sndrD2_g14',
  sattypes_rad(43)= 'sndrd3_g14',    dsis(43)= 'sndrD3_g14',
  sattypes_rad(44)= 'sndrd4_g14',    dsis(44)= 'sndrD4_g14',
  sattypes_rad(45)= 'sndrd1_g15',    dsis(45)= 'sndrD1_g15',
  sattypes_rad(46)= 'sndrd2_g15',    dsis(46)= 'sndrD2_g15',
  sattypes_rad(47)= 'sndrd3_g15',    dsis(47)= 'sndrD3_g15',
  sattypes_rad(48)= 'sndrd4_g15',    dsis(48)= 'sndrD4_g15',
  sattypes_rad(49)= 'iasi_metop-a',  dsis(49)= 'iasi616_metop-a',
  sattypes_rad(50)= 'seviri_m08',    dsis(50)= 'seviri_m08',
  sattypes_rad(51)= 'seviri_m09',    dsis(51)= 'seviri_m09',
  sattypes_rad(52)= 'seviri_m10',    dsis(52)= 'seviri_m10',
  sattypes_rad(53)= 'amsua_metop-b', dsis(53)= 'amsua_metop-b',
  sattypes_rad(54)= 'hirs4_metop-b', dsis(54)= 'hirs4_metop-b',
  sattypes_rad(55)= 'mhs_metop-b',   dsis(15)= 'mhs_metop-b',
  sattypes_rad(56)= 'iasi_metop-b',  dsis(56)= 'iasi616_metop-b',
  sattypes_rad(57)= 'avhrr3_metop-b',dsis(56)= 'avhrr3_metop-b',
  sattypes_rad(58)= 'atms_npp',      dsis(58)= 'atms_npp',
  sattypes_rad(59)= 'cris_npp',      dsis(59)= 'cris_npp',
  $SATOBS_ENKF
 /
 &END
 &ozobs_enkf
  sattypes_oz(1) = 'sbuv2_n16',
  sattypes_oz(2) = 'sbuv2_n17',
  sattypes_oz(3) = 'sbuv2_n18',
  sattypes_oz(4) = 'sbuv2_n19',
  sattypes_oz(5) = 'omi_aura',
  sattypes_oz(6) = 'gome_metop-a',
  sattypes_oz(7) = 'gome_metop-b',
  sattypes_oz(8) = 'mls30_aura'
  $OZOBS_ENKF
 /
 &END
EOF

# Execute
cat <<EOF > enkf.sh
#!/bin/sh --login
if [ x\$PBS_O_WORKDIR != x ]; then
  cd \$PBS_O_WORKDIR
  module load intel
  module load impi
  OMP_NUM_THREADS=1
fi
#module load intel
#module load totalview
cd ${WDIR}
mpirun ${EXECENKF} < enkf.nml 1> log.out 2> log.err
rc=\$?
echo "\${rc}" > complete_enkf
exit
EOF
#qsub -A hybrid -N osense${YMDH} -l walltime=08:00:00,nodes=7:ppn=12 -q batch enkf.sh
qsub -A da-cpu -N osense${YMDH} -l walltime=08:00:00,nodes=7:ppn=12 -q batch enkf.sh
# Wait until completion
#while [ ! -f osense_${YMDH}.dat ]; do
#  sleep 5
#done

FSOI_OUTPUT_DIR=/scratch4/NCEPDEV/stmp4/David.Groff/FSOI_DATA
#mv osense_${YMDH}.dat ${FSOI_OUTPUT_DIR}/osense_${YMDH}_${EFT}.dat

SCRIPTS_DIR=/scratch4/NCEPDEV/da/save/David.Groff/David.Groff/GSI/jianjun_test_script/scripts_ncep
echo "\${rc}" > complete_run_${YMDH}_${EFT}
cd ${SCRIPTS_DIR}
mv ${WDIR}/complete_run_${YMDH}_${EFT} ${SCRIPTS_DIR}

#-----------------------------------------------------------
#----------------------------------------------------------
# Generate output files used in Ed Saffords io for plotting
#----------------------------------------------------------
# Read binary observation sensitivity
# file and write to an ascii file
#Write_ascii_file osense_${YMDH}.dat

# Call Ed Safford's plotting routine.
# Or simply copy ascii file to Ed's
# java script directory
#cp osense_ascii Ed_java_script
#------------------------------------------------------------

# save results
#cd ${WDIR}
#if [ ${MINLAT} -eq -90 ] && [ ${MAXLAT} -eq 90 ] && [ ${MINLON} -eq 0 ] && [ ${MAXLON} -eq 360 ]; then
#  cp osense_${YMDH}.dat obsf/sanl_${FYMDH}_ensmean obsa/sanl_${YMDH}_ensmean fcst/sfg_${YMDH}_fhr${EFT}_ensmean fcstp/sfg_${PYMDH}_fhr${EFTP}_ensmean ${ODIR}/.
#  cp log.out ${ODIR}/stdout_${YMDH}.txt
#  gzip osense_${YMDH}.dat
#  hsi "put ${WDIR}/osense_${YMDH}.dat.gz : ${HSI_DIR}"
#fi

exit
