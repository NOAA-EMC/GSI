#!/bin/sh
set -x
sdate=2009081200
exp1=cntrl081200
exp2=cntrlalldatanotypes081200
file1=/global/noscrub/wx23dc/$exp1
file2=/global/noscrub/wx23dc/$exp2
rundir=/stmp/$LOGNAME/diffmap/$sdate
rm -rf $rundir
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

fhr="f00 f12 f24 f36 f48 f60 f72 f84 f96 f108 f120"
#fhr="f00 f24 f48 f72 f96 f120"

for hr in $fhr; do
CDATE=$sdate
fhour=`echo $hr |cut -c 2-5 `
for area in gb nh sh nhe nhw she shw; do
if [ $area = "na" ];  then lat1=20 ; lat2=60; lon1=220; lon2=300; aname="North America"; fi        ;# North America
if [ $area = "nhe" ]; then lat1=0; lat2=90; lon1=-30  ; lon2=150; aname="Eastern Northern Hem."; fi        ;# Eastern Northern Hemisphere
if [ $area = "nhw" ]; then lat1=0; lat2=90; lon1=150  ; lon2=360; aname="Western Northern Hem."; fi        ;# Western Northern Hemisphere
if [ $area = "she" ]; then lat1=-90; lat2=0; lon1=-30  ; lon2=150; aname="Eastern Southern Hem."; fi        ;# Eastern Southern Hemisphere
if [ $area = "shw" ]; then lat1=-90; lat2=0; lon1=150  ; lon2=330; aname="Western Southern Hem."; fi        ;# Western Southern Hemisphere
if [ $area = "gb" ];  then lat1=-90; lat2=90; lon1=0  ; lon2=360; aname="Global"; fi        ;# Entire Globe
if [ $area = "nh" ];  then lat1=0; lat2=90; lon1=0  ; lon2=360; aname="Northern Hem."; fi        ;# Northern Hemisphere
if [ $area = "sh" ];  then lat1=-90; lat2=0; lon1=0  ; lon2=360; aname="Southern Hem."; fi        ;# Southern Hemisphere
if [ $area = "tr" ];  then lat1=-25; lat2=25; lon1=0  ; lon2=360; aname="Tropics"; fi        ;# tropics
if [ $area = "ua" ];  then lat1=10 ; lat2=75; lon1=-30; lon2=150 ; aname="Eurasia"; fi        ;# Eurasia
if [ $area = "as" ];  then lat1=10 ; lat2=55; lon1=60 ; lon2=130; aname="Asia"; fi        ;# asian
if [ $area = "eu" ];  then lat1=35 ; lat2=70; lon1=-15; lon2=60 ; aname="Europe"; fi        ;# Europe
if [ $area = "sa" ];  then lat1=-55 ; lat2=13; lon1=275; lon2=330; aname="South America"; fi        ;# South America
if [ $area = "af" ];  then lat1=-35; lat2=35; lon1=-25; lon2=60; aname="Africa"; fi        ;# Africa
for var in HGTprs TMPprs WSPD VDIF;do
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
cat > $var${lev}_$hr$area.gs <<EOF 
'open $ctl1'
'open $ctl2'
'set grads off'
'run /u/wx24fy/bin/grads/white.gs'
'colors.gs'
'set mpdset hires'
'set mproj scaled'
'set lev $lev'
'set lat $lat1 $lat2'
'set lon $lon1 $lon2'
'set t $tt'
if ($var = "WSPD")
  'define file1=mag(ugrdprs,vgrdprs)*1.94'
  'define file2=mag(ugrdprs.2,vgrdprs.2)*1.94'
endif
if ($var = "VDIF")
  'define vecdiff=mag(ugrdprs-ugrdprs.2,vgrdprs-vgrdprs.2)*1.94'
endif
if ($var = "HGTprs" | $var = "TMPprs")
  'define file1=${var}.1'
  'define file2=${var}.2'
endif
'set gxout shaded'
'set clevs $clevs'
'set ccols 59 57 55 44 43 42 41 0 21 22 23 24 25 26 28'
'set black -0.2 0.2'
'set xlint 20'; 'set ylint 10'
if ($var = "WSPD")
'd file1-file2'
endif
if ($var = "VDIF")
'd vecdiff'
'set gxout barb'
'd skip((ugrdprs-ugrdprs.2)*1.94,5,5);(vgrdprs-vgrdprs.2)*1.94'
endif
if ($var = "HGTprs" | $var = "TMPprs")
'd file1-file2'
endif
'cbarnew.gs 0.8 0 5.5 0.3'
if ($var != "VDIF")
'set gxout contour'
'set black -0.2 0.2'
'set cint 100'
'set clab off'
'set cthick 7'
'set ccolor 1'
'd hgtprs.1'
'set cint 100'
'set ccolor 3'
'set cstyle 3'
'd hgtprs.2'
'set font 1'
'set strsiz 0.12 0.12'
'draw string 5.5 8.3 $aname (${exp1}-${exp2}) $lev hPa $varname DIFF AT $hr'
'draw string 5.5 8.1 ${exp1}=black and ${exp2}=green'
else
'set strsiz 0.14 0.14'
'draw string 5.5 8.3 $aname (${exp1}-${exp2}) $lev hPa Vector DIFF AT $hr'
endif
'printim $var${lev}_$hr$area.gif gif x700 y700'
'quit'
EOF
grads -bcl "run $var${lev}_$hr$area.gs"
done;#level
done;#variable
done;#area
tt=`expr $tt + 1`
done;#forecast hour

#create animations
for var in HGTprs TMPprs WSPD VDIF;do
for lev in 1000 850 500 200 ; do
for area in gb nh sh nhe nhw tr na sa she shw; do
convert -delay 200 -loop 15 $var${lev}_f00$area.gif $var${lev}_f12$area.gif $var${lev}_f24$area.gif $var${lev}_f36$area.gif $var${lev}_f48$area.gif $var${lev}_f60$area.gif $var${lev}_f72$area.gif $var${lev}_f84$area.gif $var${lev}_f96$area.gif $var${lev}_f108$area.gif $var${lev}_f120$area.gif $var${lev}anim$area.gif
done
done
done
exit
