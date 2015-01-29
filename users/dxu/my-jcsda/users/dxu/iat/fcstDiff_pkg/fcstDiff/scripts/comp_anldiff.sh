#!/bin/ksh
#  ... grads.setup for gwd with imbeded gs file              
#  ... the command line will run: scaler_zonal.setup
#  ... zonal_diff.setup 2008011500 hgtprs ecm gfs 1.0 00 
set -x
#
case=interp; #cntrl or interp
case1=cntrl
TMPGRADS=/ptmp/$LOGNAME/anldiffnondrop
mkdir -p $TMPGRADS
cd ${TMPGRADS}
#
grib2ctl=/u/wx20mi/bin/grib2ctl.pl
# data set control file
droplist="2008011100 2008020300 2008031800 2008042600 2008052200 2008011212 2008030312 2008031812 2008042512 2008062512"
nondroplist="2008011600 2008020400 2008022300 2008041800 2008050200 2008012012 2008032412 2008041612 2008042012 2008050212"
nexp=`echo $nondroplist |wc -w`
k=0

for CDATE in $nondroplist; do 
export scale_select=1.
export CYC=`echo $CDATE|cut -c9-10`
export CDY=`echo $CDATE|cut -c7-8`
export CMO=`echo $CDATE|cut -c5-6`
export GDATE=`${ndate_dir}/ndate -06 $CDATE`

if [ ! -s $TMPGRADS/pgbanlc.${CDATE} ]; then
cp /global/noscrub/wx23dc/${case1}${CMO}${CDY}${CYC}/pgbanl.${CDATE} $TMPGRADS/pgbanlc.${CDATE}
cp /global/noscrub/wx23dc/${case}${CMO}${CDY}${CYC}/pgbanl.${CDATE} $TMPGRADS/pgbanli.${CDATE}
fi

k=`expr $k + 1 `
if [ ! -s ${TMPGRADS}/${CDATE}c.ctl ]; then
$grib2ctl -verf ${TMPGRADS}/pgbanlc.${CDATE} > ${CDATE}c.ctl
gribmap -E -0 -i ${CDATE}c.ctl
$grib2ctl -verf ${TMPGRADS}/pgbanli.${CDATE} > ${CDATE}i.ctl
gribmap -E -0 -i ${CDATE}i.ctl
fi
export fileanl${k}=${TMPGRADS}/${CDATE}c.ctl
export fileges${k}=${TMPGRADS}/${CDATE}i.ctl

done ;#files complete


for var in hgtprs tmpprs; do 
for lev in 850 500 200; do
#
cat << EOF > ${var}${lev}.gs
'open $fileanl1';'open $fileanl2'
'open $fileanl3';'open $fileanl4'
'open $fileanl5';'open $fileanl6'
'open $fileanl7';'open $fileanl8'
'open $fileanl9';'open $fileanl10'
'open $fileges1';'open $fileges2'
'open $fileges3';'open $fileges4'
'open $fileges5';'open $fileges6'
'open $fileges7';'open $fileges8'
'open $fileges9';'open $fileges10'
*  setng up files
*
'set lev ${lev}'
'define anlmean=0.'
'define gesmean=0.'
i=1;j=11
while (i <= $nexp )
'define anl'%i'=${var}.'%i'(t=1)'
'define anlmean=anlmean+anl'%i'/${nexp} '
'define ges'%i'=${var}.'%j'(t=1)'
'define gesmean=gesmean+ges'%i'/${nexp} '
i=i+1;j=j+1
endwhile

'define diff=anlmean-gesmean'
'set display color white'
'set parea 0.5 10.5 1 7.5'
'run /global/save/wx23ja/grads/rgbset.gs'
'set grid off'
'set mproj scaled'

'set gxout shaded'
'set grads off'
'set xlint 30'; 'set ylint 10'
if($var = "hgtprs"); 'set clevs -25 -20 -15 -10 -5 5 10 15 20 25';endif
if($var = "tmpprs"); 'set clevs -3 -2 -1 -.5 -.25 .25 .5 1 2 3';endif
'set rbcols 39 37  35 33  31  0  61  63 65  67  69'
'd diff';'cbarn';'set gxout stat';'d diff'
range=sublin(result,11)
rms=subwrd(range,4)
fmt='%-6.2f'
rms=math_format(fmt,rms)
say 'rms = 'rms
'set gxout contour'
'set xlint 30'; 'set ylint 10'
'set grads off'
'set cstyle 1'
'set cthick 5'
'set clab off'
if($var = "hgtprs" & $lev <= "500");'set cint 50';endif
'set ccolor 2';'d anlmean'
if($var = "hgtprs" & $lev <= "500");'set cint 50';endif
'set ccolor 4'; 'd gesmean'
'set string 1 tc 7'
'set strsiz .15 .15'
'draw string 5.1 8.0 NonDropout Composite ANL DIFF CNTRL-INTERP ${lev}mb var=${var} rms='rms
'draw string 5.1 7.8 Contours: CNTRL(red) INTERP(blue)'
'printim COMPANLDIFF_${lev}_${var}.gif gif x700 y700'
*
'quit'
* 
* 
*
EOF

#
# begin grads setup
#    
/usrx/local/grads/bin/grads -blc "run ${var}${lev}.gs"
done
done
#/usrx/local/grads/bin/gxps  -c -i out.gr -o out.ps
#ls -l out.ps
# lpr -P phaser4 out.ps
