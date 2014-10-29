#!/bin/sh
set -x
sdate=2010100800
gdate=`${ndate_dir}/ndate  -06 $sdate`
datadir=/ptmp/wx23dc/pra
rundir=/ptmp/wx23dc/pra/maps
mkdir $rundir; cd $rundir
anl=${datadir}/pgbanl.$sdate
ges=${datadir}/pgbf06.$gdate
grib2ctl.pl $anl > anl.ctl
grib2ctl.pl -verf $ges > ges.ctl
gribmap -i anl.ctl
gribmap -i ges.ctl

cp *.ctl $rundir
tt=1
#Plot the difference between model runs at the same forecast time
#fhr="f00 f12 f24 f36 f48 f60 f72 f84 f96 f108 f120"
CDATE=$sdate
cyc=`echo $CDATE |cut -c 9-10`
for var in HGTprs TMPprs UGRDprs VGRDprs WSPD;do
for lev in 1000 850 500 200 ; do
if [ $var = "TMPprs" ];   then varname="Temp (K)"; fi
if [ $var = "HGTprs" ];   then varname="HGT (m)" ; fi
if [ $var = "UGRDprs" ];  then varname="U (m/s)" ; fi
if [ $var = "VGRDprs" ];  then varname="V (m/s)" ; fi
if [ $var = "WSPD" ];  then varname="WSPD (knots)" ; fi
#if [ $hr = "f120" ]; then
clevs=" -30 -15 -12 -9 -6 -3 -1.5 1.5 3 6 9 12 15 30"
cint=5
#else
#cint=1
#clevs="-5 -4 -3 -2 -1 -.5 .5 1 2 3 4 5"
#fi

cat > $var${lev}.gs <<EOF 
'open anl.ctl'
'open ges.ctl'
'set grads off'
'run /u/wx24fy/bin/grads/white.gs'
'colors.gs'
'set lat -90 0'
'set grid off'
'set mpdset hires'
*'set mproj scaled'
'set lev $lev'
if ($var = "WSPD")
'define anl=mag(ugrdprs,vgrdprs)*1.94'
'define ges=mag(ugrdprs.2,vgrdprs.2)*1.94'
else
'define anl=${var}.1'
'define ges=${var}.2'
endif
'set gxout shaded'
'set clevs $clevs'
'set ccols 59 57 55 44 43 42 41 0 21 22 23 24 25 26 28'
'set cint $cint'
'set black -0.2 0.2'
'set xlint 20'; 'set ylint 10'
'd anl-ges'
'cbarn'
'set gxout contour'
'set black -0.2 0.2'
'set cint 200'
'set clab on'
'set cthick 5'
'set ccolor 1'
'd hgtprs.1'
'set cthick 5'
'set ccolor 3'
'd hgtprs.2'
'set font 1'
'set strsiz 0.16 0.16'
'draw string 5.5 8.3 GFSANL-GFSGES at $lev hPa ($varname DIFF) $CDATE Dropout'
'draw string 5.5 8.1 GFSANL HGT=black and GFSGES HGT=green'
'printim $var${lev}.gif gif x700 y700'
'quit'
EOF
grads -bcl "run $var${lev}.gs"
done
done
exit
