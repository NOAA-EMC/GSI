#!/bin/sh
set -x
sdate=2009081200
exp1=cntrl081200
exp2=cntrlalldatanotypes081200
file1=/global/noscrub/wx23dc/$exp1
file2=/global/noscrub/wx23dc/$exp2
rundir=/stmp/$LOGNAME/diffmap/$sdate
rm -f $rundir/*
mkdir -p $rundir
cd $rundir
adate=`echo $sdate |cut -c 1-8`
anldate=`/global/save/wx23dc/scripts/datestamp.sh $adate 0`
cyc=`echo $sdate |cut -c 9-10`

/global/save/wx23dc/scripts/gfs1x1ctl.sh $file1 $rundir $exp1 $cyc pgbf00 $anldate $adate 11 12hr
/global/save/wx23dc/scripts/gfs1x1ctl.sh $file2 $rundir $exp2 $cyc pgbf00 $anldate $adate 11 12hr

ctl1=${exp1}.t${cyc}z.pgbf00.ctl
ctl2=${exp2}.t${cyc}z.pgbf00.ctl
tt=1

#Plot the difference between model runs at the same forecast time
fhr="f00 f12 f24 f36 f48 f60 f72 f84 f96 f108 f120"
#fhr="f00 f24 f48 f72 f96 f120"
for hr in $fhr; do
CDATE=$sdate
fhour=`echo $hr |cut -c 2-5 `

for var in HGTprs TMPprs WSPD;do
for lev in 1000 850 500 200; do
if [ $var = "TMPprs" ];   then varname="Temp (K)"; fi
if [ $var = "HGTprs" ];   then varname="HGT (m)" ; fi
if [ $var = "UGRDprs" ];  then varname="U (m/s)" ; fi
if [ $var = "VGRDprs" ];  then varname="V (m/s)" ; fi
if [ $var = "WSPD" ];  then varname="WSPD (knots)" ; fi
if [ $hr = "f96" -o  $hr = "f108" -o $hr = "f120" ]; then
clevs="-60 -30 -24 -18 -12 -6 -3 3 6 12 18 24 30 60"
else
clevs="-30 -15 -12 -9 -6 -3 -1.5 1.5 3 6 9 12 15 30"
fi
cat > $var${lev}_$hr.gs <<EOF 
'open $ctl1'
'open $ctl2'
'set grads off'
'run /u/wx24fy/bin/grads/white.gs'
'colors.gs'
'set mpdset hires'
'set mproj scaled'
'set lev $lev'
'set t $tt'
if ($var = "WSPD")
'define file1=mag(ugrdprs,vgrdprs)*1.94'
'define file2=mag(ugrdprs.2,vgrdprs.2)*1.94'
else
'define file1=${var}.1'
'define file2=${var}.2'
endif
'set gxout shaded'
'set clevs $clevs'
'set ccols 59 57 55 44 43 42 41 0 21 22 23 24 25 26 28'
'set black -0.2 0.2'
'set xlint 20'; 'set ylint 10'
'd file1-file2'
'cbarnew.gs 0.8 0 5.5 0.3'
'set gxout contour'
'set black -0.2 0.2'
'set cint 100'
'set clab off'
'set clopts 1 7'
*'set cthick 5'
*'set ccolor 1'
'd hgtprs.1'
'set clopts 3 7'
*'set cthick 5'
'set cint 100'
*'set ccolor 3'
'd hgtprs.2'
'set font 1'
'set strsiz 0.15 0.15'
'draw string 5.5 8.3 ${exp1}-${exp2} $lev hPa $varname DIFF AT $hr'
'draw string 5.5 8.1 ${exp1}=black and ${exp2}=green'
'printim $var${lev}_$hr.gif gif x700 y700'
'quit'
EOF
grads -bcl "run $var${lev}_$hr.gs"
done
done
tt=`expr $tt + 1`
done
#create animations
for var in HGTprs TMPprs WSPD;do
for lev in 1000 850 500 200 ; do
convert -delay 200 -loop 15 $var${lev}_f00.gif $var${lev}_f12.gif $var${lev}_f24.gif $var${lev}_f36.gif $var${lev}_f48.gif $var${lev}_f60.gif $var${lev}_f72.gif $var${lev}_f84.gif $var${lev}_f96.gif $var${lev}_f108.gif $var${lev}_f120.gif $var${lev}anim.gif
done
done
exit
