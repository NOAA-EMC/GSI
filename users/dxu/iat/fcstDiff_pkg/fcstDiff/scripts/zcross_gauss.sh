#!/bin/ksh
#  ... grads.setup for gwd with imbeded gs file              
#  ... the command line will run: scaler_zonal.setup
#  ... zonal_diff.setup 2008011500 hgtprs ecm gfs 1.0 00 
set -x
#
case=interpnodata; #cntrl or interp
TMPGRADS=/ptmp/$LOGNAME/${case}crossgauss
mkdir -p $TMPGRADS
cd ${TMPGRADS}
#tmpdir=/ptmp/wx23dc/out382/sigmap/${case}020300
tmpdir=/ptmp/wx23dc/out382/sigmap/nodata020300
#
grib2ctl=/u/wx20mi/bin/grib2ctl.pl
# data set control file
export CDATE=${1:-"2008020300"}
export scale_select=1.
export CYC=`echo $CDATE|cut -c9-10`
export CDY=`echo $CDATE|cut -c7-8`
export CMO=`echo $CDATE|cut -c5-6`

GDATE=`${ndate_dir}/ndate  -06 $CDATE`
#copygb -x -g2 /global/noscrub/wx23dc/${case}${CMO}${CDY}${CYC}/pgbanl.${CDATE} $TMPGRADS/pgbanl.${CDATE}
#copygb -x -g2 /global/noscrub/wx23dc/${case}ges/pgbf06.${GDATE} $TMPGRADS/pgbf06.${GDATE}
#
#$grib2ctl -verf ${TMPGRADS}/pgbanl.${CDATE} > anl.ctl
#sed <anl.ctl -e "s#zdef 47 levels#zdef 26 levels#" -e "s#1000 975 950 925 900 875 850 825 800 775 750 725 700 675 650 625 600 575 550 525 500 475 450 425 400 375 350 325 300 275 250 225 200 175 150 125 100 70 50 30 20 10 7 5 3 2 1#1000 975 950 925 900 850 800 750 700 650 600 550 500 450 400 350 300 250 200 150 100 70 50 30 20 10#"  >anl_b.ctl
#gribmap -E -i anl_b.ctl

#$grib2ctl -verf ${TMPGRADS}/pgbf06.${GDATE} > ges.ctl
#sed <ges.ctl -e "s#zdef 47 levels#zdef 26 levels#" -e "s#1000 975 950 925 900 875 850 825 800 775 750 725 700 675 650 625 600 575 550 525 500 475 450 425 400 375 350 325 300 275 250 225 200 175 150 125 100 70 50 30 20 10 7 5 3 2 1#1000 975 950 925 900 850 800 750 700 650 600 550 500 450 400 350 300 250 200 150 100 70 50 30 20 10#"  >ges_b.ctl
#gribmap -E -i ges_b.ctl

for var in TV T U V RH; do 
if [[ $var = "T" || $var = "TV" ]] ; then
tmpclevs='set clevs -5 -4 -3 -2 -1.5 -1 -.5 -.25 .25 .5 1 1.5 2 3 4 5'
tmpcols='set ccols 48 47 46 45  44  43  42  41 0 21 22 23 24 25 26 27 28'
tmpcint=.5
fi
if [[ $var = "U" || $var = "V" ]] ; then
tmpclevs='set clevs -10 -8 -6 -5 -4 -3 -2 -1  1 2 3 4 5 6 8 10'
tmpcols='set ccols 48 47 46 45  44  43  42  41 0 21 22 23 24 25 26 27 28'
tmpcint=1
fi
if [[ $var = "RH" || $var = "P" ]] ; then
tmpclevs='set clevs -60 -50 -40 -30 -20 -15 -10 -5  5 10 15 20 30 40 50 60'
tmpcols='set ccols 48 47 46 45  44  43  42  41 0 21 22 23 24 25 26 27 28'
tmpcint=5
fi
#
# write to local file -- the file we started in
# begin grads mapset.gs file creation
#
cat << EOF > mapset.gs
'open ${tmpdir}/siganl.ctl'
'open ${tmpdir}/sigges.ctl'
'set display color white'
'clear'
'set parea 0.5 10.5 1 7.5'
'run /global/save/wx23ja/grads/rgbset.gs'
'enable print out.gr'
'set grads off'
'set grid off'
'set lat -90 0'
'set z 1 64'
'set x 1'
'define mag1=${var}.1'
'define mag2=${var}.2'

'define zmag1=ave(mag1,x=1,x=768,-b)'
'define zmag2=ave(mag2,x=1,x=768,-b)'
*'define mask=ave(maskout(mag1-mag2,-1*(lev-ps/100)),x=1,x=768,-b)'
*
'set gxout shaded'
'set grads off'
'$tmpclevs'
'$tmpcols'
'd zmag1-zmag2'
'cbarn'
'set gxout contour'
'set cint $tmpcint' 
'set black -.1 .1'
'set ccolor 1'
'd zmag1-zmag2'
'set string 1 tc 7'
'set strsiz .2 .2'
'draw string 5.7 8.2 Case:${case} ANL-GES Time:${CDATE}'
'draw string 5.7 7.8 Zonal AVE DIFF var=${var}'
'print'
'printim ZAVEDIF_${case}_${CDATE}_${var}.gif gif x800 y600'
'quit'
* 
* 
*
EOF

#
# begin grads setup
#    
/usrx/local/grads/bin/grads -blc "run mapset.gs"
done
# lpr -P phaser4 out.ps
