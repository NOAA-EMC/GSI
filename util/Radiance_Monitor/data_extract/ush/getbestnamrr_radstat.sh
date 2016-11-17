echo start getbestnamrr_radstat.sh

function usage {
  echo "Usage:  getbestnamrr_radstat.sh DATE workdir COM rgnhh rgntm"
  echo "            DATE   : processing date"
  echo "            workdir: working (scratch) directory "
  echo "            COM    : radstat location"
  echo "            rgnHH  : desired regional radstat file hour"
  echo "            rgnTM  : desired regional radstat file time step"
}


nargs=$#
if [[ $nargs -ne 5 ]]; then
   usage
   exit 1
fi


   set -ax

   DATE=$1
   tmpdir=$2
   COM=$3
   rgnHH=$4
   rgnTM=$5

   mkdir -p $tmpdir
  
   HH00=`echo $DATE | cut -c 9-10`
   PDY=`echo $DATE | cut -c 1-8`

   DATEM06=`${NDATE} +06 $DATE`
   PDY06=`echo $DATEM06 | cut -c 1-8`

   ##
   ##  $USE_TM is defined in the parm/RadMon_user_settings file as tm06.
   ##
#   case $HH00 in
#    00) rstat=$COM/namrr.$PDY06/namrr.t06z.radstat.tm06
#        bias1=$COM/namrr.$PDY06/namrr.t06z.satbias.tm06;;
#    01) rstat=$COM/namrr.$PDY06/namrr.t06z.radstat.tm05
#        bias1=$COM/namrr.$PDY06/namrr.t06z.satbias.tm05;;
#    02) rstat=$COM/namrr.$PDY06/namrr.t06z.radstat.tm04
#        bias1=$COM/namrr.$PDY06/namrr.t06z.satbias.tm04;;
#    03) rstat=$COM/namrr.$PDY06/namrr.t06z.radstat.tm03
#        bias1=$COM/namrr.$PDY06/namrr.t06z.satbias.tm03;;
#    04) rstat=$COM/namrr.$PDY06/namrr.t06z.radstat.tm02
#        bias1=$COM/namrr.$PDY06/namrr.t06z.satbias.tm02;;
#    05) rstat=$COM/namrr.$PDY06/namrr.t06z.radstat.tm01
#        bias1=$COM/namrr.$PDY06/namrr.t06z.satbias.tm01;;
#    06) rstat=$COM/namrr.$PDY06/namrr.t12z.radstat.tm06
#        bias1=$COM/namrr.$PDY06/namrr.t12z.satbias.tm06;;
#    07) rstat=$COM/namrr.$PDY06/namrr.t12z.radstat.tm05
#        bias1=$COM/namrr.$PDY06/namrr.t12z.satbias.tm05;;
#    08) rstat=$COM/namrr.$PDY06/namrr.t12z.radstat.tm04
#        bias1=$COM/namrr.$PDY06/namrr.t12z.satbias.tm04;;
#    09) rstat=$COM/namrr.$PDY06/namrr.t12z.radstat.tm03
#        bias1=$COM/namrr.$PDY06/namrr.t12z.satbias.tm03;;
#    10) rstat=$COM/namrr.$PDY06/namrr.t12z.radstat.tm02
#        bias1=$COM/namrr.$PDY06/namrr.t12z.satbias.tm02;;
#    11) rstat=$COM/namrr.$PDY06/namrr.t12z.radstat.tm01
#        bias1=$COM/namrr.$PDY06/namrr.t12z.satbias.tm01;;
#    12) rstat=$COM/namrr.$PDY06/namrr.t18z.radstat.tm06
#        bias1=$COM/namrr.$PDY06/namrr.t18z.satbias.tm06;;
#    13) rstat=$COM/namrr.$PDY06/namrr.t18z.radstat.tm05
#        bias1=$COM/namrr.$PDY06/namrr.t18z.satbias.tm05;;
#    14) rstat=$COM/namrr.$PDY06/namrr.t18z.radstat.tm04
#        bias1=$COM/namrr.$PDY06/namrr.t18z.satbias.tm04;;
#    15) rstat=$COM/namrr.$PDY06/namrr.t18z.radstat.tm03
#        bias1=$COM/namrr.$PDY06/namrr.t18z.satbias.tm03;;
#    16) rstat=$COM/namrr.$PDY06/namrr.t18z.radstat.tm02
#        bias1=$COM/namrr.$PDY06/namrr.t18z.satbias.tm02;;
#    17) rstat=$COM/namrr.$PDY06/namrr.t18z.radstat.tm01
#        bias1=$COM/namrr.$PDY06/namrr.t18z.satbias.tm01;;
#    18) rstat=$COM/namrr.$PDY06/namrr.t00z.radstat.tm06
#        bias1=$COM/namrr.$PDY06/namrr.t00z.satbias.tm06;;
#    19) rstat=$COM/namrr.$PDY06/namrr.t00z.radstat.tm05
#        bias1=$COM/namrr.$PDY06/namrr.t00z.satbias.tm05;;
#    20) rstat=$COM/namrr.$PDY06/namrr.t00z.radstat.tm04
#        bias1=$COM/namrr.$PDY06/namrr.t00z.satbias.tm04;;
#    21) rstat=$COM/namrr.$PDY06/namrr.t00z.radstat.tm03
#        bias1=$COM/namrr.$PDY06/namrr.t00z.satbias.tm03;;
#    22) rstat=$COM/namrr.$PDY06/namrr.t00z.radstat.tm02
#        bias1=$COM/namrr.$PDY06/namrr.t00z.satbias.tm02;;
#    23) rstat=$COM/namrr.$PDY06/namrr.t00z.radstat.tm01
#        bias1=$COM/namrr.$PDY06/namrr.t00z.satbias.tm01;;
#   esac

   if [[ $rgnHH = "t00z" && $rgnTM != "tm00" ]]; then
      rstat=$COM/namrr.$PDY06/namrr.${rgnHH}.radstat.${rgnTM}
      bias1=$COM/namrr.$PDY06/namrr.${rgnHH}.satbias.${rgnTM}
   else
      rstat=$COM/namrr.$PDY/namrr.${rgnHH}.radstat.${rgnTM}
      bias1=$COM/namrr.$PDY/namrr.${rgnHH}.satbias.${rgnTM}
   fi

   if [ -s $rstat ]; then
      cp $rstat  $tmpdir/radstat.${DATE}
      cp $bias1  $tmpdir/satbias.${DATE}
   fi

   chmod 700 $tmpdir/*


   echo end getbestnamrr_radstat.sh

exit
