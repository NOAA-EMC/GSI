#!/bin/ksh
#  ... grads.setup for gwd with imbeded gs file              
#  ... the command line will run: scaler_zonal.setup
#  ... zonal_diff.setup 2008011500 hgtprs ecm gfs 1.0 00 
set -x
#
case=ecmwf; #cntrl or interp
case1=gfs
TMPGRADS=/ptmp/$LOGNAME/companldiffdrop
mkdir -p $TMPGRADS
cd ${TMPGRADS}
#
grib2ctl=/u/wx20mi/bin/grib2ctl.pl
# data set control file
droplist="2008011100 2008020300 2008031800 2008042600 2008052200 2008011212 2008030312 2008031812 2008042512 2008062512"
nodroplist="2008011600 2008020400 2008022300 2008041800 2008050200 2008012012 2008032412 2008041612 2008042012 2008050212"
nexp=`echo $droplist |wc -w`

k=0
for CDATE in $droplist; do 
export scale_select=1.
export CYC=`echo $CDATE|cut -c9-10`
export CDY=`echo $CDATE|cut -c7-8`
export CMO=`echo $CDATE|cut -c5-6`
if [ ! -s $TMPGRADS/pgbanle.${CDATE} ]; then
cp /global/noscrub/wx23dc/ecmwfanldrop/ecmanl.${CDATE} $TMPGRADS/pgbanle.${CDATE}
$grib2ctl -verf ${TMPGRADS}/pgbanle.${CDATE} > ${CDATE}e.ctl
gribmap -E -0 -i ${CDATE}e.ctl
cp /global/noscrub/wx23dc/gfsanldrop/gfsanl.${CDATE} $TMPGRADS/pgbanlg.${CDATE}
$grib2ctl -verf ${TMPGRADS}/pgbanlg.${CDATE} > ${CDATE}g.ctl
sed < ${CDATE}g.ctl -e "s#zdef 26 levels#zdef 14 levels#" -e "s#1000 975 950 925 900 850 800 750 700 650 600 550 500 450 400 350 300 250 200 150 100 70 50 30 20 10#1000 925 850 700 500 400 300 250 200 150 100 50 20 10#" > ${CDATE}g2.ctl
gribmap -E -0 -i ${CDATE}g2.ctl
rm -f ${CDATE}g.ctl
fi
k=`expr $k + 1`
export fileecm${k}=${TMPGRADS}/${CDATE}e.ctl
export filegfs${k}=${TMPGRADS}/${CDATE}g2.ctl

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
'open $fileecm1';'open $fileecm2'
'open $fileecm3';'open $fileecm4'
'open $fileecm5';'open $fileecm6'
'open $fileecm7';'open $fileecm8'
'open $fileecm9';'open $fileecm10'
'open $filegfs1';'open $filegfs2'
'open $filegfs3';'open $filegfs4'
'open $filegfs5';'open $filegfs6'
'open $filegfs7';'open $filegfs8'
'open $filegfs9';'open $filegfs10'
*  setng up files
*
'set lat -90 90'
'set z 1 14'
'set x 1'
'define ecmmean=0.'
'define gfsmean=0.'
i=1;j=11
while (i <= $nexp )
'define ecm'%i'=${var}.'%i'(t=1)'
'define ecmmean=ecmmean+ecm'%i'/${nexp} '
'define gfs'%i'=${var}.'%j'(t=1)'
'define gfsmean=gfsmean+gfs'%i'/${nexp} '
i=i+1;j=j+1
endwhile

'define diff=gfsmean-ecmmean'
'set display color white'
'set parea 0.5 10.5 1 7.5'
'run /global/save/wx23ja/grads/rgbset.gs'
'set grid off'

'define zmag1=ave(ecmmean,x=1,x=360,-b)'
'define zmag2=ave(gfsmean,x=1,x=360,-b)'
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
'draw string 5.7 8.2 DROPOUT COMPOSITE ANALYSIS DIFFERENCE (GFS-ECMWF)'
'draw string 5.6 7.8 ZONAL AVERAGE DIFF var=${var}'
'printim ZAVEDIFCOMPdrop_${case1}-${case}_${var}.gif gif x700 y700'
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
