#!/bin/sh
set -x
export sorcdir=/global/save/$LOGNAME/scripts
export rundir=/stmp/$LOGNAME/plotwindcdas
export VDATE=2011052900 
export edate=29may2011
#export day="0"
export day="1"
export level="850 200"

datagfs=/ptmp/$LOGNAME/gfs/all        ;#location of GFS archive
dataecm=/global/shared/stat/ecm     ;#location of ECMWF archive

if [ ! -d $rundir ]; then
rm -rf $rundir;mkdir $rundir
fi
cd $rundir

export cyc=`echo $VDATE |cut -c 9-10`
export yy=`echo $VDATE |cut -c 1-4`
export mm=`echo $VDATE |cut -c 5-6`
export dd=`echo $VDATE |cut -c 7-8`

for dd in $day; do
export ff=`expr $dd \* 24`
rdate=`${ndate_dir}/ndate  -${ff} $VDATE`
tdate=`/global/save/wx23dc/scripts/datestamp.sh $rdate`
mdate=`/global/save/wx23dc/scripts/datestamp.sh $VDATE`

if [ ! -s $rundir/gfs${dd}.ctl ];then 
sed < ${sorcdir}/gfsdummy.ctl -e "s#datadir#${datagfs}#" -e "s#CDATE#${rdate}#" -e "s#EDATE#${tdate}#" > gfs${dd}.ctl
/usrx/local/grads/bin/gribmap -i gfs${dd}.ctl

sed < ${sorcdir}/ecmdummy.ctl -e "s#datadir#${dataecm}#" -e "s#CDATE#${rdate}#" -e "s#EDATE#${tdate}#" > ecm${dd}.ctl
/usrx/local/grads/bin/gribmap -i ecm${dd}.ctl

   if [ ! -s $rundir/gfs0.ctl ]; then
      sed < ${sorcdir}/gfsdummy.ctl -e "s#datadir#${datagfs}#" -e "s#CDATE#${VDATE}#" -e "s#EDATE#${mdate}#" > gfs0.ctl
      /usrx/local/grads/bin/gribmap -i gfs0.ctl

      sed < ${sorcdir}/ecmdummy.ctl -e "s#datadir#${dataecm}#" -e "s#CDATE#${VDATE}#" -e "s#EDATE#${mdate}#" > ecm0.ctl
      /usrx/local/grads/bin/gribmap -i ecm0.ctl
   fi
fi

if [ $dd -eq 0 ]; then
tmin=1
elif [ $dd -eq 1 ]; then
tmin=5
elif [ $dd -eq 2 ]; then
tmin=9
elif [ $dd -eq 3 ]; then
tmin=13
elif [ $dd -eq 4 ]; then
tmin=17
elif [ $dd -eq 5 ]; then
tmin=21
elif [ $dd -eq 6 ]; then
tmin=25
elif [ $dd -eq 7 ]; then
tmin=29
fi

for lev in $level; do
if [ $lev -eq 850 ];then
   mag=30
else
   mag=100
fi

cat << EOF >plotwinds${dd}_${lev}.gs
'open gfs${dd}.ctl'
'open ecm${dd}.ctl'
'open gfs0.ctl'
'open ecm0.ctl'
'set display color white'
'c'
'set t ${tmin}'
'set grads off'
'set lat -20 35'
'set lon 40 95'
'set mproj scaled'
'set lev ${lev}'
'define udiff=ugrdprs.1-ugrdprs.2'
'define vdiff=vgrdprs.1-vgrdprs.2'
'define ugfs=ugrdprs.1'
'define vgfs=vgrdprs.1'
'define uecm=ugrdprs.2'
'define vecm=vgrdprs.2'

*Create plot for GFS (top right)
'set parea 0.5 5.0 4.75 7.75'
'set map 1 1 8'
'set gxout contour'
'set cint 20'
'set ccolor 4'
'set cthick 5'
'set clopts 4 -1 0.12'
'd hgtprs.1'
'set gxout vector'
'set arrscl 0.5 ${mag}'
'set ccolor 1'
'set cthick 5'
'd skip(ugfs*1.94,4);vgfs*1.94'

*Create plot for ECMWF (lower left)
'set parea 0.5 5.0 1.0 4.0'
'set map 1 1 8'
'draw map'
'set gxout contour'
'set cint 20'
'set ccolor 4'
'set cthick 5'
'set clopts 4 -1 0.12'
'd hgtprs.2'
'set gxout vector'
'set arrscl 0.5 ${mag}'
'set ccolor 2'
'set cthick 5'
'd skip(uecm*1.94,4);vecm*1.94'

*Create DIFF plot for GFS-ECMWF (top right)
'set parea 5.7 10.2 4.75 7.75'
'set map 1 1 8'
'draw map'
'set gxout contour'
'set cint 10'
'set ccolor 4'
'set cthick 5'
'set clopts 4 -1 0.12'
'd hgtprs.1-hgtprs.2'
'set gxout vector'
'set arrscl 0.5 ${mag}'
'set ccolor 1'
'set cthick 5'
'd skip(udiff*1.94,4);vdiff*1.94'

*Create Verification plot (lower right)
'set parea 5.7 10.2 1.0 4.0'
'set map 1 1 8'
'draw map'
'set gxout vector'
'set arrscl 0.5 ${mag}'
'set ccolor 1'
'set cthick 5'
'd skip(ugrdprs.3(t=1)*1.94,4);vgrdprs.3(t=1)*1.94'
'set arrscl 0.5 ${mag}'
'set ccolor 2'
'set cthick 5'
'd skip(ugrdprs.4(t=1)*1.94,4);vgrdprs.4(t=1)*1.94'

'set string 1 bl 7'
'set strsiz 0.12 0.12'
'draw string 0.5 7.85 GFS'
'draw string 0.5 4.30 ECMWF'
'draw string 5.7 7.85 GFS-ECMWF'
'draw string 5.7 4.30 GFS(black) ECMWF(red) Verification'
'set string 3 tc 7'
'set strsiz 0.13 0.13'
'draw string 5.5 8.4 GFS and ECMWF ${lev} hPa Wind (knots) and Heights ${dd}-day Forecast'
'draw string 5.5 8.2 Forecast time:  ${tdate}  Valid time: ${mdate}'
'printim wind${dd}_${lev}.gif'
'quit'
EOF

grads -bcl "run plotwinds${dd}_${lev}.gs"

done ;#end of $level variable
done ;#end of $dd or day variable
exit
