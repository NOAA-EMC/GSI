#!/bin/ksh
#  ... grads.setup for gwd with imbeded gs file              
#  ... the command line will run: scaler_zonal.setup
#  ... zonal_diff.setup 2008011500 hgtprs ecm gfs 1.0 00 
set -x
#
case=cntrlw3data; #cntrl or interp
TMPGRADS=/ptmp/$LOGNAME/${case}crossdroppgb
mkdir -p $TMPGRADS
cd ${TMPGRADS}
#
grib2ctl=/u/wx20mi/bin/grib2ctl.pl
# data set control file
export CDATE=${1:-"2008020300"}
export scale_select=1.
export CYC=`echo $CDATE|cut -c9-10`
export CDY=`echo $CDATE|cut -c7-8`
export CMO=`echo $CDATE|cut -c5-6`
savedir=/ptmp/$LOGNAME/out382/sigmap/${case}${CMO}${CDY}${CYC}
#savedir=/ptmp/$LOGNAME/out382/sigmap/nodata${CMO}${CDY}${CYC}
GDATE=`${ndate_dir}/ndate -06 $CDATE`
#copygb -x -g2 /global/noscrub/wx23dc/${case}${CMO}${CDY}${CYC}/pgbanl.${CDATE} $TMPGRADS/pgbanl.${CDATE}
#copygb -x -g2 /global/noscrub/wx23dc/${case}ges/pgbf06.${GDATE} $TMPGRADS/pgbf06.${GDATE}
#
$grib2ctl -verf ${savedir}/pgbanl.${CDATE} > anl.ctl
#sed <anl.ctl -e "s#zdef 47 levels#zdef 26 levels#" -e "s#1000 975 950 925 900 875 850 825 800 775 750 725 700 675 650 625 600 575 550 525 500 475 450 425 400 375 350 325 300 275 250 225 200 175 150 125 100 70 50 30 20 10 7 5 3 2 1#1000 975 950 925 900 850 800 750 700 650 600 550 500 450 400 350 300 250 200 150 100 70 50 30 20 10#"  >anl_b.ctl
gribmap -E -i anl.ctl

$grib2ctl -verf ${savedir}/pgbf06.${GDATE} > ges.ctl
#sed <ges.ctl -e "s#zdef 47 levels#zdef 26 levels#" -e "s#1000 975 950 925 900 875 850 825 800 775 750 725 700 675 650 625 600 575 550 525 500 475 450 425 400 375 350 325 300 275 250 225 200 175 150 125 100 70 50 30 20 10 7 5 3 2 1#1000 975 950 925 900 850 800 750 700 650 600 550 500 450 400 350 300 250 200 150 100 70 50 30 20 10#"  >ges_b.ctl
gribmap -E -i ges.ctl

for var in hgtprs tmpprs; do 
if [ $var = "hgtprs" ]; then
tmpclevs='set clevs -80 -60 -40 -30 -20 -15 -10 -5 5 10 15 20 30 40 60 80'
tmpcols='set ccols  48  47  46  45  44  43  42  41 0 21 22 23 24 25 26 27 28'
tmpcint=5
else
tmpclevs='set clevs -5 -4 -3 -2 -1.5 -1 -.5 -.25 .25 .5 1 1.5 2 3 4 5'
tmpcols='set ccols 48 47 46 45  44  43  42  41 0 21 22 23 24 25 26 27 28'
tmpcint=.5
fi

#
# write to local file -- the file we started in
# begin grads mapset.gs file creation
#
cat << EOF > mapset.gs
'open ${TMPGRADS}/anl.ctl'
'open ${TMPGRADS}/ges.ctl'
*  seting up files
*
'set display color white'
'clear'
'set parea 0.5 10.5 1 7.5'
*'q file 1'
*'q file 2'
*
'run /global/save/wx23ja/grads/rgbset.gs'
'enable print out.gr'
'set grads off'
*'set grid off'
*
'enable print out.gr'
*
'set grads off'
'set grid off'
'set lat -90 0'
*'set mpdset mres'
'set z 1 47'
'set x 1'
'define mag1=${var}.1'
'define mag2=${var}.2'
** 'define tmean=ave(difmag,t=1,t=32)'
*'define tmean=ave((${1}.1-${1}.2),t=1,t=32)'

*'define zmag1=ave(mag1,x=1,x=144,-b)'
*'define zmag2=ave(mag2,x=1,x=144,-b)'
*'define zmdif=ave(${var_select}.2-${var_select}.1,x=1,x=144,-b)'
*'define mask=ave(maskout(mag1-mag2,-1*(lev-pressfc/100)),x=1,x=144,-b)'
'define zmag1=ave(mag1,x=1,x=360,-b)'
'define zmag2=ave(mag2,x=1,x=360,-b)'
'define mask=ave(maskout(mag1-mag2,-1*(lev-pressfc/100)),x=1,x=360,-b)'
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
'draw string 5.7 8.2 Case:${case} ANL-GES Time:${CDATE}'
'draw string 5.7 7.8 PGB Zonal AVE DIFF var=${var}'
'print'
'printim PGBZAVEDIF_${case}_${CDATE}_${var}.gif gif x800 y600'
*
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
#/usrx/local/grads/bin/gxps  -c -i out.gr -o out.ps
#ls -l out.ps
# lpr -P phaser4 out.ps
