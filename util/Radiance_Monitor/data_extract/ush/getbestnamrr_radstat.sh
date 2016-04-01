echo start getbestnamrr_radstat.sh

set -ax

DATE=$1
tmpdir=$2
COM=$3

mkdir -p $tmpdir

rm -f $tmpdir/radstat.*
rm -f $tmpdir/satbias.*

rm -f datem00
rm -f datem06

echo $DATE > $tmpdir/datem00
DATEM06=`${NDATE} +06 $DATE`
echo $DATEM06 > $tmpdir/datem06

PDY00=`cut -c 1-8 $tmpdir/datem00`
HH00=`cut -c 9-10 $tmpdir/datem00`
PDY06=`cut -c 1-8 $tmpdir/datem06`
HH06=`cut -c 9-10 $tmpdir/datem06`

rm -f $tmpdir/datem00
rm -f $tmpdir/datem06

#
#  $USE_TM is defined in the parm/RadMon_user_settings file as tm06.
#
case $HH00 in
 00) rstat=$COM/namrr.$PDY06/namrr.t06z.radstat.tm06
     bias1=$COM/namrr.$PDY06/namrr.t06z.satbias.tm06;;
 01) rstat=$COM/namrr.$PDY06/namrr.t06z.radstat.tm05
     bias1=$COM/namrr.$PDY06/namrr.t06z.satbias.tm05;;
 02) rstat=$COM/namrr.$PDY06/namrr.t06z.radstat.tm04
     bias1=$COM/namrr.$PDY06/namrr.t06z.satbias.tm04;;
 03) rstat=$COM/namrr.$PDY06/namrr.t06z.radstat.tm03
     bias1=$COM/namrr.$PDY06/namrr.t06z.satbias.tm03;;
 04) rstat=$COM/namrr.$PDY06/namrr.t06z.radstat.tm02
     bias1=$COM/namrr.$PDY06/namrr.t06z.satbias.tm02;;
 05) rstat=$COM/namrr.$PDY06/namrr.t06z.radstat.tm01
     bias1=$COM/namrr.$PDY06/namrr.t06z.satbias.tm01;;
 06) rstat=$COM/namrr.$PDY06/namrr.t12z.radstat.tm06
     bias1=$COM/namrr.$PDY06/namrr.t12z.satbias.tm06;;
 07) rstat=$COM/namrr.$PDY06/namrr.t12z.radstat.tm05
     bias1=$COM/namrr.$PDY06/namrr.t12z.satbias.tm05;;
 08) rstat=$COM/namrr.$PDY06/namrr.t12z.radstat.tm04
     bias1=$COM/namrr.$PDY06/namrr.t12z.satbias.tm04;;
 09) rstat=$COM/namrr.$PDY06/namrr.t12z.radstat.tm03
     bias1=$COM/namrr.$PDY06/namrr.t12z.satbias.tm03;;
 10) rstat=$COM/namrr.$PDY06/namrr.t12z.radstat.tm02
     bias1=$COM/namrr.$PDY06/namrr.t12z.satbias.tm02;;
 11) rstat=$COM/namrr.$PDY06/namrr.t12z.radstat.tm01
     bias1=$COM/namrr.$PDY06/namrr.t12z.satbias.tm01;;
 12) rstat=$COM/namrr.$PDY06/namrr.t18z.radstat.tm06
     bias1=$COM/namrr.$PDY06/namrr.t18z.satbias.tm06;;
 13) rstat=$COM/namrr.$PDY06/namrr.t18z.radstat.tm05
     bias1=$COM/namrr.$PDY06/namrr.t18z.satbias.tm05;;
 14) rstat=$COM/namrr.$PDY06/namrr.t18z.radstat.tm04
     bias1=$COM/namrr.$PDY06/namrr.t18z.satbias.tm04;;
 15) rstat=$COM/namrr.$PDY06/namrr.t18z.radstat.tm03
     bias1=$COM/namrr.$PDY06/namrr.t18z.satbias.tm03;;
 16) rstat=$COM/namrr.$PDY06/namrr.t18z.radstat.tm02
     bias1=$COM/namrr.$PDY06/namrr.t18z.satbias.tm02;;
 17) rstat=$COM/namrr.$PDY06/namrr.t18z.radstat.tm01
     bias1=$COM/namrr.$PDY06/namrr.t18z.satbias.tm01;;
 18) rstat=$COM/namrr.$PDY06/namrr.t00z.radstat.tm06
     bias1=$COM/namrr.$PDY06/namrr.t00z.satbias.tm06;;
 19) rstat=$COM/namrr.$PDY06/namrr.t00z.radstat.tm05
     bias1=$COM/namrr.$PDY06/namrr.t00z.satbias.tm05;;
 20) rstat=$COM/namrr.$PDY06/namrr.t00z.radstat.tm04
     bias1=$COM/namrr.$PDY06/namrr.t00z.satbias.tm04;;
 21) rstat=$COM/namrr.$PDY06/namrr.t00z.radstat.tm03
     bias1=$COM/namrr.$PDY06/namrr.t00z.satbias.tm03;;
 22) rstat=$COM/namrr.$PDY06/namrr.t00z.radstat.tm02
     bias1=$COM/namrr.$PDY06/namrr.t00z.satbias.tm02;;
 23) rstat=$COM/namrr.$PDY06/namrr.t00z.radstat.tm01
     bias1=$COM/namrr.$PDY06/namrr.t00z.satbias.tm01;;
esac

if [ -s $rstat ]
then
  cp $rstat  $tmpdir/radstat.${DATE}
  cp $bias1  $tmpdir/satbias.${DATE}
fi

 chmod 700 $tmpdir/*

echo end getbestnamrr_radstat.sh

exit
