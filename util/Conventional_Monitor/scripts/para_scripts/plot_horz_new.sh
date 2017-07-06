#!/bin/sh
set -ax
date
export list=$listvar


##  the script to plot scater image for temperature

export tmpdir_plothorz=/ptmp/wx20es/plothorz

### put into local machine to show the results

export WSHOME=$WSWEB/horz/$cdump
export WSSAVE=$WSHOME/save
export WSMAP=$WSHOME/map

mkdir -p $tmpdir_plothorz
cd $tmpdir_plothorz
rm -rf *
mkdir -p $rdate
cd $rdate

export GDATE=$(/nwprod/util/exec/ndate -06 $rdate)
grib2ctl.pl $datadir/pgbanl.$cdump.$rdate > anal.ctl
gribmap -i anal.ctl -0
  grib2ctl.pl -verf $datadir/pgbf06.$cdump.$GDATE > guess.ctl
  gribmap -i guess.ctl



for type in ps q t   
#for type in ps   
do

eval stype=\${${type}_TYPE} 
eval nreal=\${nreal_${type}} 

## decoding the dignostic file

for dtype in ${stype}
do

### determine what kind data to plotted: 1: all data, 0: assimilated, -1: rejected
### or not assimilated
#dindex=0

#if [ "$dtype" = 'ps183' -o "$dtype" = 't181' -o "$dtype" = 't182' -o "$dtype" = 't183' -o "$dtype" = 't187' -o "$dtype" = 'q130' -o "$dtype" = 'q133' -o "$dtype" = 'q181' -o "$dtype" = 'q182' -o "$dtype" = 'q183' -o "$dtype" = 'q187' -o "$dtype" = 'uv228' -o "$dtype" = 'uv247' -o "$dtype" = 'uv248' -o "$dtype" = 'uv249' -o "$dtype" = 'uv250' -o "$dtype" = 'uv251' -o "$dtype" = 'uv254' -o "$dtype" = 'uv256' -o "$dtype" = 'uv281' -o "$dtype" = 'uv284' -o "$dtype" = 'uv286' -o "$dtype" = 'uv287' ]; then
#
#dindex=-1

#fi

for cycle in ges anl
do

### build the control file for the data

if [ "$dtype" = 'ps180' -o "$dtype" = 'ps181' -o  "$dtype" = 'ps183' -o "$dtype" = 'ps187'  ]; then

cp $ctldir/pstime.ctl ./${dtype}.ctl
cp $gscripts/plot_ps_horz.gs ./plot_${dtype}.gs

if [ $cycle = ges ]; then
nt=`tail -1 $tmpdir/$rdate/stdout_diag2grads_${dtype}_ges`
fi

elif [ "$dtype" = 'ps120' ]; then

cp $ctldir/pssfc.ctl ./${dtype}.ctl
cp $gscripts/plot_ps_horz.gs ./plot_${dtype}.gs
nt=1

elif [ "$dtype" = 't120' ]; then

cp $ctldir/tmandlev.ctl ./${dtype}.ctl
cp $gscripts/plot_tallev_horz.gs ./plot_${dtype}.gs
nt=1

elif [ "$dtype" = 't180' -o "$dtype" = 't181' -o "$dtype" = 't182' -o "$dtype" = 't183' -o "$dtype" = 't187'  ]; then

cp $ctldir/tsfc.ctl ./${dtype}.ctl
cp $gscripts/plot_tsfc_horz.gs ./plot_${dtype}.gs
nt=1

elif [ "$dtype" = 't130' -o "$dtype" = 't131' -o "$dtype" = 't132' -o "$dtype" = 't133' ]; then

cp $ctldir/tallev.ctl ./${dtype}.ctl
cp $gscripts/plot_tallev_horz.gs ./plot_${dtype}.gs
nt=1

elif [ "$dtype" = 'q120' ]; then

cp $ctldir/qmandlev.ctl ./${dtype}.ctl
cp $gscripts/plot_qallev_horz.gs ./plot_${dtype}.gs
nt=1
elif [ "$dtype" = 'q180' -o "$dtype" = 'q181' -o  "$dtype" = 'q183' -o "$dtype" = 'q187'  ];then
cp $ctldir/qsfc.ctl ./${dtype}.ctl
cp $gscripts/plot_qsfc_horz.gs ./plot_${dtype}.gs
if [ $cycle = ges ]; then
nt=`tail -1 $tmpdir/$rdate/stdout_diag2grads_${dtype}_ges`
fi

elif [ "$dtype" = 'q130' -o "$dtype" = 'q131' -o "$dtype" = 'q132' -o "$dtype" = 'q133' ]; then
cp $ctldir/qallev.ctl ./${dtype}.ctl
cp $gscripts/plot_qallev_horz.gs ./plot_${dtype}.gs
nt=1

elif [ "$dtype" = 'uv220' ]; then

cp $ctldir/uvmandlev.ctl ./${dtype}.ctl
cp $gscripts/plot_uvallev_horz.gs ./plot_${dtype}.gs
nt=1

elif  [ "$dtype" = 'uv223' -o "$dtype" = 'uv224' -o "$dtype" = 'uv228' ]; then

cp $ctldir/uvsig.ctl ./${dtype}.ctl
cp $gscripts/plot_uvallev_horz.gs ./plot_${dtype}.gs
nt=1

elif  [ "$dtype" = 'uv221' -o "$dtype" = 'uv230' -o "$dtype" = 'uv231' -o "$dtype" = 'uv232' -o "$dtype" = 'uv233' ]; then

cp $ctldir/uvallev.ctl  ./${dtype}.ctl
cp $gscripts/plot_uvallev_horz.gs ./plot_${dtype}.gs
nt=1

elif [ "$dtype" = 'uv242' -o "$dtype" = 'uv243'  -o "$dtype" = 'uv245' -o "$dtype" = 'uv246' -o "$dtype" = 'uv247' -o "$dtype" = 'uv248' -o "$dtype" = 'uv249' -o "$dtype" = 'uv250' -o "$dtype" = 'uv251' -o "$dtype" = 'uv252' -o "$dtype" = 'uv253' -o "$dtype" = 'uv254' -o "$dtype" = 'uv255' -o "$dtype" = 'uv256' -o "$dtype" = 'uv257' -o "$dtype" = 'uv258' ]; then

cp $ctldir/uvallev.ctl ./${dtype}.ctl
cp $gscripts/plot_uvsatwind_horz.gs ./plot_${dtype}.gs
nt=1

elif [ "$dtype" = 'uv280' -o "$dtype" = 'uv281' -o "$dtype" = 'uv282' -o "$dtype" = 'uv284'  -o "$dtype" = 'uv287' ]; then

cp $ctldir/uvsfc11.ctl ./${dtype}.ctl
cp $gscripts/plot_uvsfc_horz.gs ./plot_${dtype}.gs
if [ $cycle = ges ]; then
nt=`tail -1 $tmpdir/$rdate/stdout_diag2grads_${dtype}_ges`
fi

elif [ "$dtype" = 'uv229' ]; then

cp $ctldir/uvsfc7.ctl ./${dtype}.ctl
cp $gscripts/plot_uvsfc_horz.gs ./plot_${dtype}.gs
if [ $cycle = ges ]; then
nt=`tail -1 $tmpdir/$rdate/stdout_diag2grads_${dtype}_ges`
fi

fi


if [  ! -s $savedir/$cycle/${dtype}_grads.${rdate} ]; then
break
fi 

sdir=" dset $savedir/$cycle/${dtype}_grads.${rdate}"
title="title  ${dtype}  ${cycle}"
sed -e "s/^title.*/${title}/" ${dtype}.ctl >tmp.ctl
echo $sdir >${dtype}_grads_${cycle}.ctl
cat tmp.ctl >>${dtype}_grads_${cycle}.ctl
rm -f tmp.ctl
rm -f ${dtype}.ctl

done         ## done with cycle

if [  ! -s $savedir/$cycle/${dtype}_grads.${rdate} ]; then
continue
fi

stnmap -i ${dtype}_grads_ges.ctl 

### set up plot variables

#sed -e "s/XSIZE/$xsize/" \
#    -e "s/YSIZE/$ysize/" \
#    -e "s/PLOTFILE/$dtype/" \
#    -e "s/RDATE/$rdate/" \
#    -e "s/HINT/${hint}/" \
#    -e "s/NT/$nt/" \
#    -e "s/DINDEX/$dindex/" \
#   plot_${dtype}.gs >plothorz_${dtype}.gs

sed -e "s/XSIZE/$xsize/" \
    -e "s/YSIZE/$ysize/" \
    -e "s/PLOTFILE/$dtype/" \
    -e "s/RDATE/$rdate/" \
    -e "s/HINT/${hint}/" \
    -e "s/NT/$nt/" \
   plot_${dtype}.gs >plothorz_${dtype}.gs


echo 'quit' |grads -blc " run plothorz_${dtype}.gs" 





## #put into local machine
cd $tmpdir_plothorz
export TARMAP=horz.${rdate}.tar
export ZIPMAP=${TARMAP}.gz

tar -cvf $TARMAP ${rdate}/*png
gzip -f $TARMAP


#--- preparation
  echo "yes" | ssh -l $WSUSER $WS "date"
  ssh -l $WSUSER $WS "mkdir -p ${WSSAVE}"
  ssh -l $WSUSER $WS "mkdir -p ${WSMAP}"


cat << EOF > convert.sh
  #! /bin/sh
  set -evx
  cd $WSMAP
  gzip -cd $WSSAVE/$ZIPMAP > tmp.tar
  tar xvf tmp.tar
  rm -f tmp.tar 
EOF

scp $ZIPMAP ${WSUSER}@${WS}:${WSSAVE}
scp convert.sh ${WSUSER}@${WS}:${WSMAP}
ssh -l $WSUSER $WS "cd ${WSMAP} ; at -f ./convert.sh now "

rm -f *tar*

cd $rdate
#rm -f *png

done      ### dtype loop 
done      ### type loop

cd $tmpdir_plothorz
rm -f *tar*
 
#rm -rf  $rdate

cd $tmpdir
#rm -rf $rdate


exit

