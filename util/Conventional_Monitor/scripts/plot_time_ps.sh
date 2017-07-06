#!/bin/sh
set -ax
date
export list=$listvar

plotdir=${STMP}/plottime_ps_$SUFFIX

rm -rf $plotdir
mkdir -p $plotdir

cd $plotdir

##  the script to plot scater image for temperature

export CYA=`echo $PDATE|cut -c9-10`
##ssh -l wd20xs emcrzdm.ncep.noaa.gov "mkdir ${WEBDIR}/time/${CYA}"
#ssh -l ${WSUSER} ${WS}  "mkdir ${WEBDIR}/time/${CYA}"

### put into local machine to show the results

 cp $tmpdir/$PDATE/*ps* .

echo 'quit' |grads -bpc "run $GSCRIPTS/plotstas_time_count_ps.gs"

##scp *region1*png ${WSUSER}@${WS}:${WEBDIR}/time/${CYA}/.
##scp *region2*png ${WSUSER}@${WS}:${WEBDIR}/time/${CYA}/.
##scp *region3*png ${WSUSER}@${WS}:${WEBDIR}/time/${CYA}/.
##scp *region4*png ${WSUSER}@${WS}:${WEBDIR}/time/${CYA}/.
##scp *region5*png ${WSUSER}@${WS}:${WEBDIR}/time/${CYA}/.
##scp *region6*png ${WSUSER}@${WS}:${WEBDIR}/time/${CYA}/.
##scp *region7*png ${WSUSER}@${WS}:${WEBDIR}/time/${CYA}/.
##scp *region8*png ${WSUSER}@${WS}:${WEBDIR}/time/${CYA}/.
##scp *region9*png ${WSUSER}@${WS}:${WEBDIR}/time/${CYA}/.
##scp *region10*png ${WSUSER}@${WS}:${WEBDIR}/time/${CYA}/.

mkdir -p ${IMGNDIR}/pngs/time/${CYA}
cp -f *region1_*png ${IMGNDIR}/pngs/time/${CYA}/.
cp -f *region10_*png ${IMGNDIR}/pngs/time/${CYA}/.
cp -f *region2*png ${IMGNDIR}/pngs/time/${CYA}/.
cp -f *region3*png ${IMGNDIR}/pngs/time/${CYA}/.
cp -f *region4*png ${IMGNDIR}/pngs/time/${CYA}/.
cp -f *region5*png ${IMGNDIR}/pngs/time/${CYA}/.
cp -f *region6*png ${IMGNDIR}/pngs/time/${CYA}/.
cp -f *region7*png ${IMGNDIR}/pngs/time/${CYA}/.
cp -f *region8*png ${IMGNDIR}/pngs/time/${CYA}/.
cp -f *region9*png ${IMGNDIR}/pngs/time/${CYA}/.

rm -f *region1*png
rm -f *region2*png
rm -f *region3*png
rm -f *region4*png
rm -f *region5*png
rm -f *region6*png
rm -f *region7*png
rm -f *region8*png
rm -f *region9*png


echo 'quit' |grads -bpc "run $GSCRIPTS/plotstas_time_bias_ps.gs"

echo 'quit' |grads -bpc "run $GSCRIPTS/plotstas_time_count_ps.gs"

mkdir -p ${IMGNDIR}/pngs/time/${CYA}
cp -f *region1_*png ${IMGNDIR}/pngs/time/${CYA}/.
cp -f *region10_*png ${IMGNDIR}/pngs/time/${CYA}/.
cp -f *region2*png ${IMGNDIR}/pngs/time/${CYA}/.
cp -f *region3*png ${IMGNDIR}/pngs/time/${CYA}/.
cp -f *region4*png ${IMGNDIR}/pngs/time/${CYA}/.
cp -f *region5*png ${IMGNDIR}/pngs/time/${CYA}/.
cp -f *region6*png ${IMGNDIR}/pngs/time/${CYA}/.
cp -f *region7*png ${IMGNDIR}/pngs/time/${CYA}/.
cp -f *region8*png ${IMGNDIR}/pngs/time/${CYA}/.
cp -f *region9*png ${IMGNDIR}/pngs/time/${CYA}/.

##scp *region1*png ${WSUSER}@${WS}:${WEBDIR}/time/${CYA}/.
##scp *region2*png ${WSUSER}@${WS}:${WEBDIR}/time/${CYA}/.
##scp *region3*png ${WSUSER}@${WS}:${WEBDIR}/time/${CYA}/.
##scp *region4*png ${WSUSER}@${WS}:${WEBDIR}/time/${CYA}/.
##scp *region5*png ${WSUSER}@${WS}:${WEBDIR}/time/${CYA}/.
##scp *region6*png ${WSUSER}@${WS}:${WEBDIR}/time/${CYA}/.
##scp *region7*png ${WSUSER}@${WS}:${WEBDIR}/time/${CYA}/.
##scp *region8*png ${WSUSER}@${WS}:${WEBDIR}/time/${CYA}/.
##scp *region9*png ${WSUSER}@${WS}:${WEBDIR}/time/${CYA}/.
##scp *region10*png ${WSUSER}@${WS}:${WEBDIR}/time/${CYA}/.
#rm -f ftpdat
#cat <<EOF > ftpdat
#lcd ${plotdir}
#cd ${WEBDIR}
#mkdir ${WEBDIR}/time/${CYA}
#cd ${WEBDIR}/time/${CYA}
#prompt
#bin
#mput *region1_*png
#mput *region10_*png
#mput *region2*png
#mput *region3*png
#mput *region4*png
#mput *region5*png
#mput *region6*png
#mput *region7*png
#mput *region8*png
#mput *region9*png
#quit
#EOF


##scp *region1*png wd20xs@emcrzdm.ncep.noaa.gov:${WEBDIR}/time/${CYA}/.
##scp *region2*png wd20xs@emcrzdm.ncep.noaa.gov:${WEBDIR}/time/${CYA}/.
##scp *region3*png wd20xs@emcrzdm.ncep.noaa.gov:${WEBDIR}/time/${CYA}/.
##scp *region4*png wd20xs@emcrzdm.ncep.noaa.gov:${WEBDIR}/time/${CYA}/.
###scp *region5*png wd20xs@emcrzdm.ncep.noaa.gov:${WEBDIR}/time/${CYA}/.
##scp *region6*png wd20xs@emcrzdm.ncep.noaa.gov:${WEBDIR}/time/${CYA}/.
##scp *region7*png wd20xs@emcrzdm.ncep.noaa.gov:${WEBDIR}/time/${CYA}/.
##scp *region8*png wd20xs@emcrzdm.ncep.noaa.gov:${WEBDIR}/time/${CYA}/.
##scp *region9*png wd20xs@emcrzdm.ncep.noaa.gov:${WEBDIR}/time/${CYA}/.
#ftp -v emcrzdm.ncep.noaa.gov < ftpdat
##ftp -v rzdm.ncep.noaa.gov < ftpdat

##rm -f *region1*png
##rm -f *region2*png
##rm -f *region3*png
##rm -f *region4*png
##rm -f *region5*png
##rm -f *region6*png
##rm -f *region7*png
##rm -f *region8*png
##rm -f *region9*png


##echo 'quit' |grads -bpc "run $GSCRIPTS/plotstas_time_bias2_ps.gs"


#cd ..
#rm -rf $plotdir


exit

