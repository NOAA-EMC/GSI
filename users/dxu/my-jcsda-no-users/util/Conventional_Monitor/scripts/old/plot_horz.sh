#!/bin/sh
set -ax
date
export list=$listvar


##  the script to plot scater image for temperature

export tmpdir_plothorz=/stmp/wx20xs/plothorz/$SUFFIX
CYA=`echo $PDATE|cut -c9-10`
export savedir=${TANKDIR}/horz_hist
### makdirectory web site machine to show the results
#ssh -l wd20xs rzdm.ncep.noaa.gov "mkdir -p ${WEBDIR}/horz/${CYA}"
#ssh -l wd20xs lnx42.ncep.noaa.gov "mkdir -p ${WEBDIR}/horz/${CYA}"
ssh -l $WSUSER $WS "mkdir -p ${WEBDIR}/horz/${CYA}"
#export xsize=x1100
#export ysize=y1000

rm -rf $tmpdir_plothorz
mkdir -p $tmpdir_plothorz
cd $tmpdir_plothorz
rm -rf *
mkdir -p $PDATE
cd $PDATE

export GDATE=$(/nwprod/util/exec/ndate -06 $PDATE)
cp $DATDIR/pgbanl.$PDATE pgbanl.$PDATE
cp $DATDIR/pgbf06.$GDATE pgbf06.$GDATE
grib2ctl.pl pgbanl.$PDATE > anal.ctl
gribmap -i anal.ctl -0
  grib2ctl.pl -verf pgbf06.$GDATE > guess.ctl
  gribmap -i guess.ctl

### some grads tools
cp /u/wx20xs/home/grads/gslib/rgbset2.gs ./rgbset2.gs 
cp /u/wx20xs/home/grads/gslib/page.gs ./page.gs 
cp /u/wx20xs/home/grads/gslib/defint.gs ./defint.gs 


for type in ps q t   
do

eval stype=\${${type}_TYPE} 
eval nreal=\${nreal_${type}} 

## decoding the dignostic file

for dtype in ${stype}
do

for cycle in ges anl
do


### build the control file for the data

if [ "$dtype" = 'ps180' -o "$dtype" = 'ps181' -o  "$dtype" = 'ps183' -o "$dtype" = 'ps187'  ]; then

cp $CTLDIR/pstime.ctl ./${dtype}.ctl
cp $GSCRIPTS/plot_ps_horz.gs ./plot_${dtype}.gs

if [ $cycle = ges ]; then
nt=`tail -1 $tmpdir/$PDATE/stdout_diag2grads_${dtype}_ges`
fi

elif [ "$dtype" = 'ps120' ]; then

cp $CTLDIR/pssfc.ctl ./${dtype}.ctl
cp $GSCRIPTS/plot_ps_horz.gs ./plot_${dtype}.gs
nt=1

elif [ "$dtype" = 't120' ]; then

cp $CTLDIR/tmandlev.ctl ./${dtype}.ctl
cp $GSCRIPTS/plot_tallev_horz.gs ./plot_${dtype}.gs
nt=1

elif [ "$dtype" = 't180' -o "$dtype" = 't181' -o "$dtype" = 't182' -o "$dtype" = 't183' -o "$dtype" = 't187'  ]; then

cp $CTLDIR/tsfc.ctl ./${dtype}.ctl
cp $GSCRIPTS/plot_tsfc_horz.gs ./plot_${dtype}.gs
nt=1

elif [ "$dtype" = 't130' -o "$dtype" = 't131' -o "$dtype" = 't132' -o "$dtype" = 't133' ]; then

cp $CTLDIR/tallev.ctl ./${dtype}.ctl
cp $GSCRIPTS/plot_tallev_horz.gs ./plot_${dtype}.gs
nt=1

elif [ "$dtype" = 'q120' ]; then

cp $CTLDIR/qmandlev.ctl ./${dtype}.ctl
cp $GSCRIPTS/plot_qallev_horz.gs ./plot_${dtype}.gs
nt=1
elif [ "$dtype" = 'q180' -o "$dtype" = 'q181' -o  "$dtype" = 'q183' -o "$dtype" = 'q187'  ];then
cp $CTLDIR/qsfc.ctl ./${dtype}.ctl
cp $GSCRIPTS/plot_qsfc_horz.gs ./plot_${dtype}.gs
if [ $cycle = ges ]; then
nt=`tail -1 $tmpdir/$PDATE/stdout_diag2grads_${dtype}_ges`
fi

elif [ "$dtype" = 'q130' -o "$dtype" = 'q131' -o "$dtype" = 'q132' -o "$dtype" = 'q133' ]; then
cp $CTLDIR/qallev.ctl ./${dtype}.ctl
cp $GSCRIPTS/plot_qallev_horz.gs ./plot_${dtype}.gs
nt=1

elif [ "$dtype" = 'uv220' ]; then

cp $CTLDIR/uvmandlev.ctl ./${dtype}.ctl
cp $GSCRIPTS/plot_uvallev_horz.gs ./plot_${dtype}.gs
nt=1

elif  [ "$dtype" = 'uv223' -o "$dtype" = 'uv224' -o "$dtype" = 'uv228' ]; then

cp $CTLDIR/uvsig.ctl ./${dtype}.ctl
cp $GSCRIPTS/plot_uvallev_horz.gs ./plot_${dtype}.gs
nt=1

elif  [ "$dtype" = 'uv221' -o "$dtype" = 'uv230' -o "$dtype" = 'uv231' -o "$dtype" = 'uv232' -o "$dtype" = 'uv233' ]; then

cp $CTLDIR/uvallev.ctl  ./${dtype}.ctl
cp $GSCRIPTS/plot_uvallev_horz.gs ./plot_${dtype}.gs
nt=1

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
   plot_${dtype}.gs >plothorz_${dtype}.gs



echo 'quit' |grads -blc " run plothorz_${dtype}.gs" 

#scp *png wd20xs@rzdm.ncep.noaa.gov:${WEBDIR}/horz/${CYA}/.
scp *png ${WSUSER}@${WS}:${WEBDIR}/horz/${CYA}/.
rm -f *png

done      ### dtype loop 

done      ### type loop

cd $tmpdir_plothorz
 
rm -rf  $PDATE

cd ..
rm -rf $tmpdir_plothorz


exit

