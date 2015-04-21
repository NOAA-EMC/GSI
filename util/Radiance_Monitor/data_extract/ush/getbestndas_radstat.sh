echo start getbestndas_radstat.sh

set -ax

DATE=$1
tmpdir=$2
COM=$3

mkdir -p $tmpdir

rm -f $tmpdir/radstat.*
rm -f $tmpdir/satbiasc.*

rm -f datem00
rm -f datem12

echo $DATE > $tmpdir/datem00
DATEM12=`${NDATE} +12 $DATE`
echo $DATEM12 > $tmpdir/datem12

PDY00=`cut -c 1-8 $tmpdir/datem00`
HH00=`cut -c 9-10 $tmpdir/datem00`
PDY12=`cut -c 1-8 $tmpdir/datem12`
HH12=`cut -c 9-10 $tmpdir/datem12`

rm -f $tmpdir/datem00
rm -f $tmpdir/datem12

#
#  $USE_TM is defined in the parm/RadMon_user_settings file as tm12.
#
case $HH00 in
 00) ndas1=$COM/ndas.$PDY12/ndas.t${HH12}z.radstat.${USE_TM}
     bias1=$COM/ndas.$PDY12/ndas.t${HH12}z.satbiasc.${USE_TM};;
 06) ndas1=$COM/ndas.$PDY12/ndas.t${HH12}z.radstat.${USE_TM}
     bias1=$COM/ndas.$PDY12/ndas.t${HH12}z.satbiasc.${USE_TM};;
 12) ndas1=$COM/ndas.$PDY12/ndas.t${HH12}z.radstat.${USE_TM}
     bias1=$COM/ndas.$PDY12/ndas.t${HH12}z.satbiasc.${USE_TM};;
 18) ndas1=$COM/ndas.$PDY12/ndas.t${HH12}z.radstat.${USE_TM}
     bias1=$COM/ndas.$PDY12/ndas.t${HH12}z.satbiasc.${USE_TM};;
esac

if [ -s $ndas1 ]
then
  cp $ndas1 $tmpdir/radstat.${DATE}
  cp $bias1  $tmpdir/satbias.${DATE}
fi

 chmod 700 $tmpdir/*

echo end getbestndas_radstat.sh

exit
