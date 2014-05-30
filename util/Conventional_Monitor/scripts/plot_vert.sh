#!/bin/sh
#-------------------------------------------------------
#
#  plot_vert.sh
#
#-------------------------------------------------------

set -ax
date
export list=$listvar

type=$1

plotdir=${STMP}/plotvert_${type}_${SUFFIX}

rm -rf $plotdir

mkdir -p $plotdir

cd $plotdir

##  the script to plot scater image for temperature

export CYA=`echo $PDATE|cut -c9-10`
export hour=$CYA
export dday=`echo $PDATE|cut -c7-8`

 cp $tmpdir/$PDATE/*${type}* .

cp $GSCRIPTS/plotstas_vert_count.gs ./plotstas_vert_count.gs
cp $GSCRIPTS/plotstas_vert_bias.gs  ./plotstas_vert_bias.gs
cp $GSCRIPTS/plotstas_vert_bias2.gs ./plotstas_vert_bias2.gs
cp $GSCRIPTS/page.gs                ./page.gs
cp $GSCRIPTS/rgbset2.gs             ./rgbset2.gs
cp $GSCRIPTS/setvpage.gs            ./setvpage.gs

  sed -e "s/HOUR/$hour/" \
    -e "s/DDAY/$dday/" \
    -e "s/DTYPE/$type/" \
   plotstas_vert_count.gs >plotvert_count_${type}.gs
echo 'quit' |grads -blc plotvert_count_${type}.gs

sed -e "s/HOUR/$hour/" \
    -e "s/DDAY/$dday/" \
    -e "s/DTYPE/$type/" \
   plotstas_vert_bias.gs >plotvert_bias_${type}.gs
echo 'quit' |grads -blc plotvert_bias_${type}.gs

#sed -e "s/HOUR/$hour/" \
#    -e "s/DDAY/$dday/" \
#    -e "s/DTYPE/$type/" \
#   plotstas_vert_bias2.gs >plotvert_bias2_${type}.gs
#echo 'quit' |grads -blc plotvert_bias2_${type}.gs

##scp *png wd20xs@rzdm.ncep.noaa.gov:${WEBDIR}/vert/${CYA}/.
##scp *png ${WSUSER}@${WS}:${WEBDIR}/vert/${CYA}/.

mkdir -p ${IMGNDIR}/pngs/vert/${CYA}
cp -f *.png ${IMGNDIR}/pngs/vert/${CYA}/.

rm -f *png

#cd ..
#rm -rf $plotdir


exit

