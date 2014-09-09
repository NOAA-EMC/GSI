#!/bin/sh
#  ... grads.setup for gwd with imbeded gs file              
#  ... the command line will run: scaler_zonal.setup
#  ... zonal_diff.setup 2008011500 hgtprs ecm gfs 1.0 00 
set -x
#
CDATE=2008052200
case1=NODATA
case2=QCSATWNDALLDATA
pgb1=/ptmp/wx23dc/out382/sigmap/cntrlnodata052200/pgbanl.$CDATE
pgb2=/ptmp/wx23dc/out382/sigmap/cntrlalldataqcsatwnd_052200/pgbanl.$CDATE
tmpdir=/ptmp/wx23dc/basicmaps
mkdir -p $tmpdir
cd $tmpdir

grib2ctl=/u/wx20mi/bin/grib2ctl.pl
$grib2ctl -verf $pgb1 > $case1.ctl
$grib2ctl -verf $pgb2 > $case2.ctl
gribmap -i $case1.ctl
gribmap -i $case2.ctl

for lev in 850 500 200; do
for var in HGTPRS TMPPRS UGRDPRS VGRDPRS PRMSLMSL; do
#
cat << EOF > map.gs
'open ${case1}.ctl';'open ${case2}.ctl'
*
'define diff=${var}.1-${var}.2'
'set display color white'
'set gxout shaded'
'set grads off'
'set black -0.1 0.1'
'd diff'
'cbarn'
'set gxout contour'
'set black -.1 .1'
'd diff'
'draw title ${case1}-${case2} $var ANL DIFFS AT $lev MB'
'printim $CDATE${case1}-${case2}_${var}diff${lev}.gif gif x700 y700'
*
'quit'
EOF

#
# begin grads setup
#    
/usrx/local/grads/bin/grads -blc "run map.gs"
done
done
echo 'FINISHED'
