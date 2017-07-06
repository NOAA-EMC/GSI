#!/bin/sh
set -ax
date
export list=$listvar


##  the script to plot scater image for temperature

export tmpdir_plothist=/stmp/wx20es/plothist/$SUFFIX
#export CYA=`echo $PDATE|cut -c1-10`
export CYA=`echo $PDATE|cut -c9-10`
#ssh -l wd20xs rzdm.ncep.noaa.gov "mkdir -p ${WEBDIR}/hist/${CYA}"
#ssh -l wd20xs lnx42.ncep.noaa.gov "mkdir -p ${WEBDIR}/hist/${CYA}"
#ssh -l  $WSUSER $WS "mkdir -p ${WEBDIR}/hist/${CYA}"

### put into local machine to show the results

rm -rf $tmpdir_plothist
mkdir -p $tmpdir_plothist
cd $tmpdir_plothist
mkdir -p $PDATE
cd $PDATE


for type in ps q t uv  
do

eval stype=\${${type}_TYPE} 
eval nreal=\${nreal_${type}} 
exec=read_${type}.x

## decoding the dignostic file

for dtype in ${stype}
do
mtype=`echo ${dtype} | cut -f1 -d_`
subtype=`echo ${dtype} | cut -f2 -d_`

rm -f fileout
for cycle in ges anl
do

### read scatter data for histgram file

/bin/sh  $SCRIPTS/read_scatter.sh $SUFFIX $dtype $mtype $subtype $PDATE $FIXDIR $nreal $exec $type $cycle $TANKDIR/horz_hist/$cycle $EXEDIR 


####



### build the control file for the data

if [  -s $TANKDIR/horz_hist/$cycle/${dtype}.scater.${PDATE} ];then
cp $CTLDIR/hist_${type}.ctl ./hist_${dtype}.ctl

nlev=`head -1 stdout_${dtype}_${cycle}.${PDATE} `

sdir=" dset ^out_${dtype}_${cycle}.${PDATE}"
title="title  ${dtype}  ${cycle}"
xdef="xdef $nlev   linear 1 1 "
sed -e "s/^title.*/${title}/" hist_${dtype}.ctl >tmp.ctl
sed -e "s/^xdef.*/${xdef}/" tmp.ctl >tmp1.ctl
echo $sdir >${cycle}_${dtype}.ctl
cat tmp1.ctl >>${cycle}_${dtype}.ctl
rm -f tmp.ctl
rm -f tmp1.ctl


tail -3 stdout_${dtype}_${cycle}.${PDATE} >>fileout

fi

done         ## done with cycle

### set up plot variables

if [  -s $TANKDIR/horz_hist/$cycle/${dtype}.scater.${PDATE} ];then

cp $GSCRIPTS/plot_hist.gs ./plot_hist.gs 

sed -e "s/XSIZE/$xsize/" \
    -e "s/YSIZE/$ysize/" \
    -e "s/PLOTFILE/$dtype/" \
    -e "s/SDATE/$PDATE/" \
   plot_hist.gs >plothist_${dtype}.gs


echo 'quit' |grads -blc " run plothist_${dtype}.gs" 


if [ "${type}" = 'uv' ]; then

for uvtype in u v
do

rm -f fileout
for cycle in ges anl 
do

nlev=`head -1 stdout_${dtype}_${uvtype}_${cycle}.${PDATE} `

sdir=" dset ^out_${dtype}_${uvtype}_${cycle}.${PDATE}"
title="title  ${dtype}_${uvtype}  ${cycle}"
xdef="xdef $nlev   linear 1 1 "
sed -e "s/^title.*/${title}/" hist_${dtype}.ctl >tmp.ctl
sed -e "s/^xdef.*/${xdef}/" tmp.ctl >tmp1.ctl
echo $sdir >${cycle}_${dtype}_${uvtype}.ctl
cat tmp1.ctl >>${cycle}_${dtype}_${uvtype}.ctl
rm -f tmp.ctl
rm -f tmp1.ctl

tail -3 stdout_${dtype}_${uvtype}_${cycle}.${PDATE} >>fileout

done

### set up plot variables
cp $GSCRIPTS/plot_hist.gs ./plot_hist.gs

sed -e "s/XSIZE/$xsize/" \
    -e "s/YSIZE/$ysize/" \
    -e "s/PLOTFILE/${dtype}_${uvtype}/" \
    -e "s/SDATE/$PDATE/" \
   plot_hist.gs >plothist_${dtype}_${uvtype}.gs


echo 'quit' |grads -blc " run plothist_${dtype}_${uvtype}.gs"

done      ### uvtype loop
fi

fi
done      ### dtype loop 

##scp *hist*.png wd20xs@rzdm.ncep.noaa.gov:${WEBDIR}/hist/${CYA}/.
##scp *hist*.png wd20xs@lnx42.ncep.noaa.gov:${WEBDIR}/hist/${CYA}/.
##scp *hist*.png $WSUSER@$WS:${WEBDIR}/hist/${CYA}/.
#cat <<EOF > ftpdat
#lcd ${tmpdir_plothist}/${PDATE}
#cd ${WEBDIR}
#mkdir ${WEBDIR}/hist/${CYA}
#cd ${WEBDIR}/hist/${CYA}
#prompt
#bin
#mput *hist*.png
#quit
#EOF
#
#   ftp -v rzdm.ncep.noaa.gov < ftpdat

#rm -f *hist*.png

   mkdir -p ${IMGNDIR}/hist/${CYA}
   cp -f ${IMGNDIR}/hist/${CYA}/.

done      ### type loop



#cd $tmpdir_plothist
#rm -rf *
#cd ..
#rm -rf $tmpdir_plothist

exit

