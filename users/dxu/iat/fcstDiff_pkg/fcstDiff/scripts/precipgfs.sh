#!/bin/sh
set -x
export sorcdir=/global/save/$LOGNAME/scripts
export rundir=/stmp/$LOGNAME/plotprecip
export VDATE=2011052900 
export edate=29May2011
export day="1 3 5 7"

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

for da in $day; do
export ff=`expr $da \* 24`
rdate=`${ndate_dir}/ndate -${ff} $VDATE`
tdate=`/global/save/wx23dc/scripts/datestamp.sh $rdate`
mdate=`/global/save/wx23dc/scripts/datestamp.sh $VDATE`
#Copy files to rundir

if [ ! -s $rundir/gfs${da}.ctl ];then 
sed < ${sorcdir}/gfsdummy.ctl -e "s#datadir#${datagfs}#" -e "s#CDATE#${rdate}#" -e "s#EDATE#${tdate}#" > gfs${da}.ctl
/usrx/local/grads/bin/gribmap -i gfs${da}.ctl
sed < ${sorcdir}/ecmdummy.ctl -e "s#datadir#${dataecm}#" -e "s#CDATE#${rdate}#" -e "s#EDATE#${tdate}#" > ecm${da}.ctl
/usrx/local/grads/bin/gribmap -i ecm${da}.ctl
fi

if [ $da -eq 1 ]; then
tmin=2
elif [ $da -eq 2 ]; then
tmin=6
elif [ $da -eq 3 ]; then
tmin=10
elif [ $da -eq 4 ]; then
tmin=14
elif [ $da -eq 5 ]; then
tmin=18
elif [ $da -eq 6 ]; then
tmin=22
elif [ $da -eq 7 ]; then
tmin=26
fi
cat << EOF >plotprecip${da}.gs
'open gfs${da}.ctl'
'open ecm${da}.ctl'
'set display color white'
'c'
'set parea 0.5 5.0 4.75 8.0'
'set grads off'
'set lat -20 35'
'set lon 40 95'
'set mproj scaled'
'set map 1 1 6'
'rgbset2.gs'
'set gxout shaded'
'set clevs  .1  2  4  6  8  10   15  20 '
'set ccols 81 41 43 45 47  31  33  35  38'
'set xlint 10'
'set ylint 10'
'define precgfs=0.'
i=$tmin
j=i+3
while (i <= j)
*'define precgfs=precgfs+maskout(apcpsfc(t='%i'),landsfc-1)'
'define precgfs=precgfs+apcpsfc(t='%i')'
i=i+1
endwhile
'd precgfs'
'set parea 0.5 5.0 1.0 4.25'
'set gxout shaded'
'set clevs  .1  2  4  6  8  10   15  20 '
'set ccols 81 41 43 45 47  31  33  35  38'
i=${tmin}-1
say 'i= '%i' and j= '%j
if ($da=1)
*'define prececm=maskout(apcpsfc.2(t='%j'),landsfc.1-1)'
'define prececm=apcpsfc.2(t='%j')'
else
*'define prececm=maskout(apcpsfc.2(t='%j')-apcpsfc.2(t='%i'),landsfc.1-1)'
'define prececm=apcpsfc.2(t='%j')-apcpsfc.2(t='%i')'
endif
'set clevs  .1  2  4  6  8  10   15  20 '
'set ccols 81 41 43 45 47  31  33  35  38'
'd prececm'
'cbarn.gs 0.6 0 2.7 0.6'
'set parea 5.7 10 2.80 6.10'
'set clevs -50 -20 -10 -5 -1 1 5 10 20 50'
'set ccols 49 47 45 43 41 0 21 23 25 27 29'
'd precgfs-prececm'
'cbarn.gs 0.6 0 7.7 2.4'
'set string 59 bl 7'
'set strsiz 0.17 0.17'
'draw string 2.5 8.10 GFS'
'draw string 2.3 4.35 ECMWF'
'draw string 6.9 6.20 GFS-ECMWF'
'set strsiz 0.15 0.15'
'draw string 5.5 8.0 GFS and ECMWF ${da}-day Precip. Forecast'
'draw string 5.5 7.8 Forecast time:  ${tdate}'
'draw string 5.5 7.6 Valid time:     ${mdate}'
'draw string 5.5 7.4 Units: mm/day'
'printim gfsprecip${da}_${VDATE}.gif'
'quit'
EOF
grads -bcl "run plotprecip${da}.gs"
done ;#end of $da or day variable
exit
