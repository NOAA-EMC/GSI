#!/bin/sh
set -x
sdate=2010092100
gdate=`${ndate_dir}/ndate -06 $sdate`
datadir=/ptmp/wx23dc/ecm
rundir=/ptmp/wx23dc/ecm/mapsregion
mkdir $rundir; cd $rundir
gfs=/ptmp/wx23dc/pra/pgbanl.$sdate
ecm=/ptmp/wx23dc/ecm/pgbanl25.$sdate
grib2ctl.pl $gfs > gfs.ctl
grib2ctl.pl $ecm > ecm.ctl
gribmap -i gfs.ctl
gribmap -i ecm.ctl

cp *.ctl $rundir
lat1=-80
lat2=-40
lon1=210
lon2=300
#Plot the difference between model runs at the same forecast time
#fhr="f00 f12 f24 f36 f48 f60 f72 f84 f96 f108 f120"
CDATE=$sdate
cyc=`echo $CDATE |cut -c 9-10`
for var in PRESsfc ;do
if [ $var = "PRESsfc" ];  then varname="Surface Pressure (Pa)" ; fi
clevs=" -30 -15 -12 -9 -6 -3 -1.5 1.5 3 6 9 12 15 30"
cint=5

cat > $var.gs <<EOF 
'open gfs.ctl'
'open ecm.ctl'
'set grads off'
'run /u/wx24fy/bin/grads/white.gs'
'colors.gs'
'set lat $lat1 $lat2'
'set lon $lon1 $lon2'
'set grid off'
'set mpdset hires'
*'set mproj scaled'
if ($var = "WSPD")
'define gfs=mag(ugrdprs,vgrdprs)*1.94'
'define ecm=mag(ugrdprs.2,vgrdprs.2)*1.94'
else
'define gfs=${var}.1'
'define ecm=${var}.2'
endif
'set gxout shaded'
'set clevs $clevs'
'set ccols 59 57 55 44 43 42 41 0 21 22 23 24 25 26 28'
'set cint $cint'
'set black -0.2 0.2'
'set xlint 20'; 'set ylint 10'
'd gfs-ecm'
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
'set strsiz 0.14 0.14'
'draw string 5.5 8.3 GFSANL-ECMANL ($varname DIFF) $CDATE Dropout'
'draw string 5.5 8.1 GFSANL HGT=black and ECMANL HGT=green'
'printim $var.gif gif x700 y700'
'quit'
EOF
grads -bcl "run $var.gs"
done
exit
