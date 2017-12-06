#!/bin/sh
set -x
YMDH=$1
#DATDIR=/scratch4/NCEPDEV/da/noscrub/David.Groff/test_efsofcst/${YMDH}
DATDIR=/scratch4/NCEPDEV/da/noscrub/David.Groff/prnthdjf_fcst/${YMDH}
SCRIPTS_DIR=/scratch4/NCEPDEV/da/save/David.Groff/David.Groff/GSI/jianjun_test_script/scripts_ncep
cd $DATDIR
tar -cvf $YMDH.tar *
hsi "put $YMDH.tar : /NCEPDEV/emc-da/1year/David.Groff/WCOSS/prnthdjf_fcst/$YMDH.tar"
#hsi "put $YMDH.tar : /NCEPDEV/emc-da/1year/David.Groff/WCOSS/test_efsofcst/$YMDH.tar"
cd ${SCRIPTS_DIR}
rm -rf $DATDIR
echo "\${rc}" > complete_cleanup_${YMDH}
exit
