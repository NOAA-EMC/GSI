#!/bin/ksh
#  ... grads.setup for gwd with imbeded gs file              
#  ... the command line will run: scaler_zonal.setup
#  ... zonal_diff.setup 2008011500 hgtprs ecm gfs 1.0 00 
set -x
#
case=interp; #cntrl or interp
case1=cntrl
TMPGRADS=/ptmp/$LOGNAME/companldiffcrossnondrop
mkdir -p $TMPGRADS
cd ${TMPGRADS}
#
grib2ctl=/u/wx20mi/bin/grib2ctl.pl
# data set control file
droplist="2008011100 2008020300 2008031800 2008042600 2008052200 2008011212 2008030312 2008031812 2008042512 2008062512"
nodroplist="2008011600 2008020400 2008022300 2008041800 2008050200 2008012012 2008032412 2008041612 2008042012 2008050212"
nexp=`echo $droplist |wc -w`

k=0
for CDATE in $nodroplist; do 
export scale_select=1.
export CYC=`echo $CDATE|cut -c9-10`
export CDY=`echo $CDATE|cut -c7-8`
export CMO=`echo $CDATE|cut -c5-6`
if [ ! -s $TMPGRADS/pgbanli.${CDATE} ]; then
cp /global/noscrub/wx23dc/${case}${CMO}${CDY}${CYC}/pgbanl.${CDATE} $TMPGRADS/pgbanli.${CDATE}
$grib2ctl -verf ${TMPGRADS}/pgbanli.${CDATE} > ${CDATE}i.ctl
gribmap -E -0 -i ${CDATE}i.ctl
cp /global/noscrub/wx23dc/${case1}${CMO}${CDY}${CYC}/pgbanl.${CDATE} $TMPGRADS/pgbanlc.${CDATE}
$grib2ctl -verf ${TMPGRADS}/pgbanlc.${CDATE} > ${CDATE}c.ctl
gribmap -E -0 -i ${CDATE}c.ctl
fi
k=`expr $k + 1`
export fileanl${k}=${TMPGRADS}/${CDATE}i.ctl
export fileges${k}=${TMPGRADS}/${CDATE}c.ctl

done ;#files complete


for var in hgtprs tmpprs; do 
if [ $var = "hgtprs" ]; then
tmpclevs='set clevs -30 -25 -20 -15 -12 -9 -6 -3 3 6 9 12 15 20 25 30'
#tmpclevs='set clevs -80 -60 -40 -30 -20 -15 -10 -5 5 10 15 20 30 40 60 80'
tmpcols='set ccols  48  47  46  45  44  43  42  41 0 21 22 23 24 25 26 27 28'
tmpcint=3
else
tmpclevs='set clevs -5 -4 -3 -2 -1.5 -1 -.5 -.25 .25 .5 1 1.5 2 3 4 5'
tmpcols='set ccols 48 47 46 45  44  43  42  41 0 21 22 23 24 25 26 27 28'
tmpcint=.5
fi

#
cat << EOF > ${var}.gs
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
'set lat -90 90'
'set z 1 47'
'set x 1'
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

'define diff=gesmean-anlmean'
'set display color white'
'set parea 0.5 10.5 1 7.5'
'run /global/save/wx23ja/grads/rgbset.gs'
'set grid off'

'define zmag1=ave(anlmean,x=1,x=360,-b)'
'define zmag2=ave(gesmean,x=1,x=360,-b)'
'define mask=ave(maskout(zmag2-zmag1,-1*(lev-pressfc/100)),x=1,x=360,-b)'
*
'set gxout shaded'
'set grads off'
'$tmpclevs'
'$tmpcols'
'd mask'
'cbarn'
'set gxout contour'
'set cint $tmpcint' 
'set black -.1 .1'
'set ccolor 1'
'd mask'
'set string 1 tc 7'
'set strsiz .2 .2'
'draw string 5.7 8.2            CNTRLANL-INTERPANL '
'draw string 5.6 7.8 NonDropout Composite Zonal AVE DIFF var=${var}'
'printim ZAVEDIFCOMP_${case1}-${case}_${var}.gif gif x700 y700'
*
'quit'
* 
* 
*
EOF

#
# begin grads setup
#    
/usrx/local/grads/bin/grads -blc "run ${var}.gs"
done
#/usrx/local/grads/bin/gxps  -c -i out.gr -o out.ps
#ls -l out.ps
# lpr -P phaser4 out.ps
