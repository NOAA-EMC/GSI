#!/bin/sh
set -ax
date
export list=$listvar


##  the script to plot scater image for temperature

export tmpdir_plothorz=/stmp/wx20xs/plothorz_uv/$SUFFIX
export savedir=${TANKDIR}/horz_hist

### makdirectory web site machine to show the results
#ssh -l wd20xs rzdm.ncep.noaa.gov "mkdir -p ${WEBDIR}/horz/${CYA}"
ssh -l $WSUSER $WS "mkdir -p ${WEBDIR}/horz/${CYA}"

rm -rf $tmpdir_plothorz
mkdir -p $tmpdir_plothorz
cd $tmpdir_plothorz
mkdir -p $PDATE
cd $PDATE

export GDATE=$(/nwprod/util/exec/ndate -06 $PDATE)

grib2ctl.pl $DATDIR/pgbanl.$PDATE > anal.ctl
gribmap -i anal.ctl -0
  grib2ctl.pl -verf $DATDIR/pgbf06.$GDATE > guess.ctl
  gribmap -i guess.ctl

### some grads tools
cp /u/wx20xs/home/grads/gslib/rgbset2.gs ./rgbset2.gs
cp /u/wx20xs/home/grads/gslib/page.gs ./page.gs
cp /u/wx20xs/home/grads/gslib/defint.gs ./defint.gs


for type in uv   
do

eval stype=\${${type}_TYPE} 
eval nreal=\${nreal_${type}} 
exec=read_${type}

## decoding the dignostic file

for dtype in ${stype}
do

for cycle in ges anl
do

### determine what kind data to plotted: 1: all data, 0: assimilated, -1: rejected
### or not assimilated

if [ "$dtype" = 'uv220' ]; then

cp $CTLDIR/uvmandlev.ctl ./${dtype}.ctl
cp $gscripts/plot_uvallev_horz.gs ./plot_${dtype}.gs
nt=1

elif  [ "$dtype" = 'uv223' -o "$dtype" = 'uv224' -o "$dtype" = 'uv228' ]; then

cp $CTLDIR/uvsig.ctl ./${dtype}.ctl
cp $gscripts/plot_uvallev_horz.gs ./plot_${dtype}.gs
nt=1

elif  [ "$dtype" = 'uv221' -o "$dtype" = 'uv230' -o "$dtype" = 'uv231' -o "$dtype" = 'uv232' -o "$dtype" = 'uv233' ]; then

cp $CTLDIR/uvallev.ctl  ./${dtype}.ctl
cp $gscripts/plot_uvallev_horz.gs ./plot_${dtype}.gs
nt=1

elif [ "$dtype" = 'uv242' -o "$dtype" = 'uv243'  -o "$dtype" = 'uv245' -o "$dtype" = 'uv246' -o "$dtype" = 'uv247' -o "$dtype" = 'uv248' -o "$dtype" = 'uv249' -o "$dtype" = 'uv250' -o "$dtype" = 'uv251' -o "$dtype" = 'uv252' -o "$dtype" = 'uv253' -o "$dtype" = 'uv254' -o "$dtype" = 'uv255' -o "$dtype" = 'uv256' -o "$dtype" = 'uv257' -o "$dtype" = 'uv258' ]; then

cp $CTLDIR/uvallev.ctl ./${dtype}.ctl
cp $gscripts/plot_uvsatwind_horz.gs ./plot_${dtype}.gs
nt=1

elif [ "$dtype" = 'uv280' -o "$dtype" = 'uv281' -o "$dtype" = 'uv282' -o "$dtype" = 'uv284'  -o "$dtype" = 'uv287' ]; then

cp $CTLDIR/uvsfc11.ctl ./${dtype}.ctl
cp $gscripts/plot_uvsfc_horz.gs ./plot_${dtype}.gs
if [ $cycle = ges ]; then
nt=`tail -1 $tmpdir/$PDATE/stdout_diag2grads_${dtype}_ges`
fi

elif [ "$dtype" = 'uv229' ]; then

cp $CTLDIR/uvsfc7.ctl ./${dtype}.ctl
cp $gscripts/plot_uvsfc_horz.gs ./plot_${dtype}.gs
if [ $cycle = ges ]; then
nt=`tail -1 $tmpdir/$PDATE/stdout_diag2grads_${dtype}_ges`
fi

fi


if [  ! -s $savedir/$cycle/${dtype}_grads.${PDATE} ]; then
break
fi 

sdir=" dset $savedir/$cycle/${dtype}_grads.${PDATE}"
title="title  ${dtype}  ${cycle}"
sed -e "s/^title.*/${title}/" ${dtype}.ctl >tmp.ctl
echo $sdir >${dtype}_grads_${cycle}.ctl
cat tmp.ctl >>${dtype}_grads_${cycle}.ctl
rm -f tmp.ctl
rm -f ${dtype}.ctl

done         ## done with cycle

if [  ! -s $savedir/$cycle/${dtype}_grads.${PDATE} ]; then
continue
fi

stnmap -i ${dtype}_grads_ges.ctl 

### set up plot variables

sed -e "s/XSIZE/$xsize/" \
    -e "s/YSIZE/$ysize/" \
    -e "s/PLOTFILE/$dtype/" \
    -e "s/RDATE/$PDATE/" \
    -e "s/HINT/${hint}/" \
    -e "s/NT/$nt/" \
    -e "s/DINDEX/$dindex/" \
   plot_${dtype}.gs >plothorz_${dtype}.gs


echo 'quit' |grads -blc " run plothorz_${dtype}.gs" 





#scp *png wd20xs@rzdm.ncep.noaa.gov:${WEBDIR}/horz/${CYA}/.
scp *png ${WSUSER}@${WS}:${WEBDIR}/horz/${CYA}/.
rm -f *png

done      ### dtype loop 
done      ### type loop

cd $tmpdir_plothorz
rm -rf $PDATE
cd ..
rm -rf $tmpdir_plothorz 

exit

