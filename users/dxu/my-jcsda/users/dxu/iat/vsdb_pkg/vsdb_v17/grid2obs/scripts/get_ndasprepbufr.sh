#!/bin/ksh
set -x

#  this script looks for ndas or nam prepbufr.

DATE=${1:-${vdate:-$(date +%Y%m%d%H)}}

export NWPROD=${NWPROD:-/nwprod}
export ndate=${ndate:-$NWPROD/util/exec/ndate}
export hpsstar=${HPSSTAR:-/u/Fanglin.Yang/bin/hpsstar}    
export COM=${HPSSPROD:-/NCEPPROD/hpssprod/runhistory}
export RUNDIR=${RUNDIR:-/stmpd2/$LOGNAME/g2o}
cd $RUNDIR || exit 8

YYYY=`echo $DATE |cut -c 1-4 `
YYYYMM=`echo $DATE |cut -c 1-6 `
PDY=`echo $DATE |cut -c 1-8 `
HH=`echo $DATE |cut -c 1-10 `

for xh in 00 03 06 09 12 15 18 21; do
 xdate=$($ndate +$xh $DATE )
 eval YYYY${xh}=`echo $xdate |cut -c 1-4 `
 eval YYYYMM${xh}=`echo $xdate |cut -c 1-6 `
 eval PDY${xh}=`echo $xdate |cut -c 1-8 `
 eval HH${xh}=`echo $xdate |cut -c 9-10 `
done

case $HH00 in
 00) hpss1=$COM/rh${YYYY12}/${YYYYMM12}/${PDY12}/com_nam_prod_ndas.${PDY12}${HH12}.bufr.tar
     hpss2=$COM/rh${YYYY06}/${YYYYMM06}/${PDY06}/com_nam_prod_ndas.${PDY06}${HH06}.bufr.tar
     hpss3=$COM/rh${YYYY00}/${YYYYMM00}/${PDY00}/com_nam_prod_nam.${PDY00}${HH00}.bufr.tar;;
 03) hpss1=$COM/rh${YYYY09}/${YYYYMM09}/${PDY09}/com_nam_prod_ndas.${PDY09}${HH09}.bufr.tar
     hpss2=$COM/rh${YYYY03}/${YYYYMM03}/${PDY03}/com_nam_prod_ndas.${PDY03}${HH03}.bufr.tar;;
 06) hpss1=$COM/rh${YYYY12}/${YYYYMM12}/${PDY12}/com_nam_prod_ndas.${PDY12}${HH12}.bufr.tar
     hpss2=$COM/rh${YYYY06}/${YYYYMM06}/${PDY06}/com_nam_prod_ndas.${PDY06}${HH06}.bufr.tar
     hpss3=$COM/rh${YYYY00}/${YYYYMM00}/${PDY00}/com_nam_prod_nam.${PDY00}${HH00}.bufr.tar;;
 09) hpss1=$COM/rh${YYYY09}/${YYYYMM09}/${PDY09}/com_nam_prod_ndas.${PDY09}${HH09}.bufr.tar
     hpss2=$COM/rh${YYYY03}/${YYYYMM03}/${PDY03}/com_nam_prod_ndas.${PDY03}${HH03}.bufr.tar;;
 12) hpss1=$COM/rh${YYYY12}/${YYYYMM12}/${PDY12}/com_nam_prod_ndas.${PDY12}${HH12}.bufr.tar
     hpss2=$COM/rh${YYYY06}/${YYYYMM06}/${PDY06}/com_nam_prod_ndas.${PDY06}${HH06}.bufr.tar
     hpss3=$COM/rh${YYYY00}/${YYYYMM00}/${PDY00}/com_nam_prod_nam.${PDY00}${HH00}.bufr.tar;;
 15) hpss1=$COM/rh${YYYY09}/${YYYYMM09}/${PDY09}/com_nam_prod_ndas.${PDY09}${HH09}.bufr.tar
     hpss2=$COM/rh${YYYY03}/${YYYYMM03}/${PDY03}/com_nam_prod_ndas.${PDY03}${HH03}.bufr.tar;;
 18) hpss1=$COM/rh${YYYY12}/${YYYYMM12}/${PDY12}/com_nam_prod_ndas.${PDY12}${HH12}.bufr.tar
     hpss2=$COM/rh${YYYY06}/${YYYYMM06}/${PDY06}/com_nam_prod_ndas.${PDY06}${HH06}.bufr.tar
     hpss3=$COM/rh${YYYY00}/${YYYYMM00}/${PDY00}/com_nam_prod_nam.${PDY00}${HH00}.bufr.tar;;
 21) hpss1=$COM/rh${YYYY09}/${YYYYMM09}/${PDY09}/com_nam_prod_ndas.${PDY09}${HH09}.bufr.tar
     hpss2=$COM/rh${YYYY03}/${YYYYMM03}/${PDY03}/com_nam_prod_ndas.${PDY03}${HH03}.bufr.tar;;
esac

case $HH00 in
 00) date1=${PDY12}
     date2=${PDY06}
     date3=${PDY00};;
 03) date1=${PDY09}
     date2=${PDY03};;
 06) date1=${PDY12}
     date2=${PDY06}
     date3=${PDY00};;
 09) date1=${PDY09}
     date2=${PDY03};;
 12) date1=${PDY12}
     date2=${PDY06}
     date3=${PDY00};;
 15) date1=${PDY09}
     date2=${PDY03};;
 18) date1=${PDY12}
     date2=${PDY06}
     date3=${PDY00};;
 21) date1=${PDY09}
     date2=${PDY03};;
esac


case $HH00 in
 00) ndas1=ndas.t${HH12}z.prepbufr.tm12
     ndas2=ndas.t${HH06}z.prepbufr.tm06
     ndas3=nam.t${HH00}z.prepbufr.tm00;;
 03) ndas1=ndas.t${HH09}z.prepbufr.tm09
     ndas2=ndas.t${HH03}z.prepbufr.tm03;;
 06) ndas1=ndas.t${HH12}z.prepbufr.tm12
     ndas2=ndas.t${HH06}z.prepbufr.tm06
     ndas3=nam.t${HH00}z.prepbufr.tm00;;
 09) ndas1=ndas.t${HH09}z.prepbufr.tm09
     ndas2=ndas.t${HH03}z.prepbufr.tm03;;
 12) ndas1=ndas.t${HH12}z.prepbufr.tm12
     ndas2=ndas.t${HH06}z.prepbufr.tm06
     ndas3=nam.t${HH00}z.prepbufr.tm00;;
 15) ndas1=ndas.t${HH09}z.prepbufr.tm09
     ndas2=ndas.t${HH03}z.prepbufr.tm03;;
 18) ndas1=ndas.t${HH12}z.prepbufr.tm12
     ndas2=ndas.t${HH06}z.prepbufr.tm06
     ndas3=nam.t${HH00}z.prepbufr.tm00;;
 21) ndas1=ndas.t${HH09}z.prepbufr.tm09
     ndas2=ndas.t${HH03}z.prepbufr.tm03;;
esac


if [ -s /com/nam/prod/ndas.${date1}/$ndas1 ]; then
  cp /com/nam/prod/ndas.${date1}/$ndas1 .
else
  $hpsstar get $hpss1 ./$ndas1
fi

if [ -s $ndas1 ]; then
  mv $ndas1 prepda.$DATE
else
  if [ -s /com/nam/prod/ndas.${date2}/$ndas2 ]; then
    cp /com/nam/prod/ndas.${date2}/$ndas2 .
  else
    $hpsstar get $hpss2 ./$ndas2
  fi
  if [ -s $ndas2 ]; then
    mv $ndas2 prepda.$DATE
  else
    if [ -s /com/nam/prod/ndas.${date3}/$ndas3 ]; then
      cp /com/nam/prod/ndas.${date3}/$ndas3 .
    else
      $hpsstar get $hpss3 ./$ndas3
    fi
    mv $ndas3 prepda.$DATE
  fi
fi

if [ -s prepda.$DATE ] ; then
 chmod 700 prepda.$DATE
else
 echo "ndas prepda.$DATE not found in $0"
fi

exit
