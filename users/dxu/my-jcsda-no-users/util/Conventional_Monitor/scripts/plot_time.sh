#!/bin/sh

#----------------------------------------------------------
#
#  plot_time.sh
#
#----------------------------------------------------------
set -ax
date
export list=$listvar

type=$1

plotdir=${STMP}/plottime_${type}_${SUFFIX}

rm -rf $plotdir
mkdir -p $plotdir

cd $plotdir

##  the script to plot scater image for temperature

export CYA=`echo $PDATE|cut -c9-10`

### put into local machine to show the results

cp $tmpdir/$PDATE/*${type}* .
cp ${GSCRIPTS}/plotstas_time_count.gs ./plotstas_time_count.gs
cp ${GSCRIPTS}/plotstas_time_bias.gs ./plotstas_time_bias.gs
cp ${GSCRIPTS}/plotstas_time_bias2.gs ./plotstas_time_bias2.gs
cp ${GSCRIPTS}/page.gs ./page.gs

 sed -e "s/DTYPE/$type/" \
 plotstas_time_count.gs > plotstas_time_count_${type}.gs
echo 'quit' |grads -bpc "run plotstas_time_count_${type}.gs"

##scp *region1_*png ${WSUSER}@${WS}:${WEBDIR}/time/${CYA}/.
##scp *region10_*png ${WSUSER}@${WS}:${WEBDIR}/time/${CYA}/.
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
cp -f *region1_*png  ${IMGNDIR}/pngs/time/${CYA}/.
cp -f *region10_*png ${IMGNDIR}/pngs/time/${CYA}/.
cp -f *region2*png   ${IMGNDIR}/pngs/time/${CYA}/.
cp -f *region3*png   ${IMGNDIR}/pngs/time/${CYA}/.
cp -f *region4*png   ${IMGNDIR}/pngs/time/${CYA}/.
cp -f *region5*png   ${IMGNDIR}/pngs/time/${CYA}/.
cp -f *region6*png   ${IMGNDIR}/pngs/time/${CYA}/.
cp -f *region7*png   ${IMGNDIR}/pngs/time/${CYA}/.
cp -f *region8*png   ${IMGNDIR}/pngs/time/${CYA}/.
cp -f *region9*png   ${IMGNDIR}/pngs/time/${CYA}/.

rm -f *region1_*png
rm -f *region10_*png
rm -f *region2*png
rm -f *region3*png
rm -f *region4*png
rm -f *region5*png
rm -f *region6*png
rm -f *region7*png
rm -f *region8*png
rm -f *region9*png

 sed -e "s/DTYPE/$type/" \
 plotstas_time_bias.gs > plotstas_time_bias_${type}.gs
echo 'quit' |grads -bpc "run plotstas_time_bias_${type}.gs"

# sed -e "s/DTYPE/$type/" \
# plotstas_time_bias2.gs > plotstas_time_bias2_${type}.gs
#echo 'quit' |grads -bpc "run plotstas_time_bias2_${type}.gs"

mkdir -p ${IMGNDIR}/pngs/time/${CYA}
cp -f *region1_*png  ${IMGNDIR}/pngs/time/${CYA}/.
cp -f *region10_*png ${IMGNDIR}/pngs/time/${CYA}/.
cp -f *region2*png   ${IMGNDIR}/pngs/time/${CYA}/.
cp -f *region3*png   ${IMGNDIR}/pngs/time/${CYA}/.
cp -f *region4*png   ${IMGNDIR}/pngs/time/${CYA}/.
cp -f *region5*png   ${IMGNDIR}/pngs/time/${CYA}/.
cp -f *region6*png   ${IMGNDIR}/pngs/time/${CYA}/.
cp -f *region7*png   ${IMGNDIR}/pngs/time/${CYA}/.
cp -f *region8*png   ${IMGNDIR}/pngs/time/${CYA}/.
cp -f *region9*png   ${IMGNDIR}/pngs/time/${CYA}/.

##scp *region1_*png ${WSUSER}@${WS}:${WEBDIR}/time/${CYA}/.
##scp *region10_*png ${WSUSER}@${WS}:${WEBDIR}/time/${CYA}/.
##scp *region2*png ${WSUSER}@${WS}:${WEBDIR}/time/${CYA}/.
##scp *region3*png ${WSUSER}@${WS}:${WEBDIR}/time/${CYA}/.
##scp *region4*png ${WSUSER}@${WS}:${WEBDIR}/time/${CYA}/.
##scp *region5*png ${WSUSER}@${WS}:${WEBDIR}/time/${CYA}/.
##scp *region6*png ${WSUSER}@${WS}:${WEBDIR}/time/${CYA}/.
##scp *region7*png ${WSUSER}@${WS}:${WEBDIR}/time/${CYA}/.
##scp *region8*png ${WSUSER}@${WS}:${WEBDIR}/time/${CYA}/.
##scp *region9*png ${WSUSER}@${WS}:${WEBDIR}/time/${CYA}/.
##scp *region10*png ${WSUSER}@${WS}:${WEBDIR}/time/${CYA}/.
##scp *region1*png wd20xs@emcrzdm.ncep.noaa.gov:${WEBDIR}/time/${CYA}/.
##scp *region2*png wd20xs@emcrzdm.ncep.noaa.gov:${WEBDIR}/time/${CYA}/.
##scp *region3*png wd20xs@emcrzdm.ncep.noaa.gov:${WEBDIR}/time/${CYA}/.
##scp *region4*png wd20xs@emcrzdm.ncep.noaa.gov:${WEBDIR}/time/${CYA}/.
###scp *region5*png wd20xs@emcrzdm.ncep.noaa.gov:${WEBDIR}/time/${CYA}/.
##scp *region6*png wd20xs@emcrzdm.ncep.noaa.gov:${WEBDIR}/time/${CYA}/.
##scp *region7*png wd20xs@emcrzdm.ncep.noaa.gov:${WEBDIR}/time/${CYA}/.
##scp *region8*png wd20xs@emcrzdm.ncep.noaa.gov:${WEBDIR}/time/${CYA}/.
##scp *region9*png wd20xs@emcrzdm.ncep.noaa.gov:${WEBDIR}/time/${CYA}/.
###scp *region10*png wd20xs@emcrzdm.ncep.noaa.gov:${WEBDIR}/time/${CYA}/.
#
##rm -f *region1_*png
##rm -f *region10_*png
##rm -f *region2*png
##rm -f *region3*png
##rm -f *region4*png
##rm -f *region5*png
##rm -f *region6*png
##rm -f *region7*png
##rm -f *region8*png
##rm -f *region9*png
##rm -f *region10*png



cd ..
#rm -rf $plotdir


exit

