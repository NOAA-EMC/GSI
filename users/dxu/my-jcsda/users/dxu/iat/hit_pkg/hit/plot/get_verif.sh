#!/bin/ksh

if [ $# -lt 1 ]; then
  echo " "
  echo " !!! ERROR: You must enter at least 5 arguments:"
  echo " !!!"
  echo " !!! USAGE: `basename $0` -i interval -t vtype -f afile -d ymdh -u userid model1 .... [model(n)]"
  echo " "
  echo "     interval = track verification interval (6,12,24)"
  echo "     vtype    = type of verification length (next72, previous120, beginning to now, etc...)"
  echo "     afile    = atcfname in format of al042001, ep112002, etc...."
  echo "     ymdh     = first starting time of date"
  echo "     userid   = wx20tm, wx23ph, wx20tr, etc...."
  echo " "
  exit 8
fi

# set -x

while getopts i:t:f:d:u: opt
do
  case $opt in
    i) interval="$OPTARG";;
    t) vtype="$OPTARG";;
    f) afile="$OPTARG";;
    d) ymdh="$OPTARG";;
    u) userid="$OPTARG";;
    \?) err=1;;
  esac
done

export netdir=${netdir:-/stmp/$LOGNAME/tracks/pru12c}
export ndate=$scrdir/ndate.x

# echo " "
# echo "After while loop, vtype=    $vtype"
# echo "                  afile=    $afile"
# echo "                  interval= $interval"
# echo "                  ymdh=     $ymdh"
# echo " "

#---------------------------------------------
# First, sort the atcf file, grep out only the
# CARQ cards for 35 kt winds for the "0" lead 
# time and put these into a temporary 
# verification file.

atcffile=a${afile}.dat
grep "CARQ,   0" $atcffile                    |\
      awk 'substr($0,65,1) ~ /3/ {print $0}'  |\
      awk '{
        lat10 = substr($0,36,3)
        latns = substr($0,39,1)
        if (latns == "N")  ylat = lat10/10.0
        if (latns == "S")  ylat = lat10/-10.0

        lon10 = substr($0,42,4)
        lonew = substr($0,46,1)
        if (lonew == "W")  xlon = lon10/10.0
        if (lonew == "E")  xlon = 360. - lon10/10.0

        printf ("%10s  %4s  %3d  %5.1f  %5.1f  %2s%2s%2s  %10s\n",
        substr($0,9,10),
        substr($0,25,4),
        substr($0,31,3),
        ylat,
        xlon,
        substr($0,1,2),substr($0,5,2),substr($0,11,2),
        substr($0,150,10))
      }' >${netdir}/vfile.temp

case $interval in
  "every6")  hrint=6;;
  "every12") hrint=12;;
  "every24") hrint=24;;
esac

#---------------------------------------------
# Based on what type of verification plotting 
# has been chosen, set up the beginning and 
# the end times for the range to grep out.

case $vtype in

  "next72")  begymdh=${ymdh}
             endymdh=`$ndate 72 ${begymdh}`
             iymdh=${begymdh};;

  "next84")  begymdh=${ymdh}
             endymdh=`$ndate 84 ${begymdh}`
             iymdh=${begymdh};;

  "next96")  begymdh=${ymdh}
             endymdh=`$ndate 96 ${begymdh}`
             iymdh=${begymdh};;

  "next120") begymdh=${ymdh}
             endymdh=`$ndate 120 ${begymdh}`
             iymdh=${begymdh};;

  "next144") begymdh=${ymdh}
             endymdh=`$ndate 144 ${begymdh}`
             iymdh=${begymdh};;

  "next168") begymdh=${ymdh}
             endymdh=`$ndate 168 ${begymdh}`
             iymdh=${begymdh};;

  "prev72")  begymdh=`$ndate -72 ${ymdh}`
             endymdh=${ymdh}
             iymdh=${begymdh};;

  "prev96")  begymdh=`$ndate -96 ${ymdh}`
             endymdh=${ymdh}
             iymdh=${begymdh};;

  "prev120") begymdh=`$ndate -120 ${ymdh}`
             endymdh=${ymdh}
             iymdh=${begymdh};;

  "prev144") begymdh=`$ndate -144 ${ymdh}`
             endymdh=${ymdh}
             iymdh=${begymdh};;

  "prev168") begymdh=`$ndate -168 ${ymdh}`
             endymdh=${ymdh}
             iymdh=${begymdh};;

  "begtonow") begymdh=` cat ${netdir}/vfile.temp         |\
                head -1                                  |\
                awk '{printf ("%10s\n",$1)}'`
              endymdh=${ymdh}
              iymdh=${begymdh};;

  "onlynow") endymdh=${ymdh}
             iymdh=${ymdh};;

  "remainder") endymdh=` cat ${netdir}/vfile.temp        |\
               tail -1                                   |\
               awk '{printf ("%10s\n",$1)}'`
               iymdh=${ymdh};;

  "entire")  begymdh=` cat ${netdir}/vfile.temp          |\
               head -1                                   |\
               awk '{printf ("%10s\n",$1)}'`
             endymdh=` cat ${netdir}/vfile.temp          |\
               tail -1                                   |\
               awk '{printf ("%10s\n",$1)}'`
             iymdh=${begymdh};;

esac


# echo "before loop, iymdh= $iymdh, endymdh= $endymdh, hrint= $hrint"

case $vtype in 

  prev72|prev96|prev120|prev144|prev168|begtonow)

    # Start with the current date and go backwards.  If we did it the
    # other way, and started at the beginning of the storm, then 
    # depending on the hour interval chosen, we might not actually plot
    # the verification for the current time, which is critical.
     
    rm ${netdir}/vfile.flipped
    eymdh=${endymdh}
    while [ ${eymdh} -ge ${iymdh} ]
    do
      grep ${eymdh} ${netdir}/vfile.temp >>${netdir}/vfile.flipped
      eymdh=`$ndate -${hrint} $eymdh`
    done

    cat ${netdir}/vfile.flipped ;;

  entire)

    # The key here is to make sure we include the current, analysis time.
    # If we just start from the beginning, then depending on the hour 
    # interval chosen, we might not actually plot the verification for 
    # the current time.  So we need to do this in 2 parts.  The first 
    # part is to get from the current time back to the beginning.  The
    # next part is to get from the current time to the end.

    rm ${netdir}/vfile.flipped
    eymdh=${ymdh}
    while [ ${eymdh} -ge ${iymdh} ]
    do
      grep ${eymdh} ${netdir}/vfile.temp >>${netdir}/vfile.flipped
      eymdh=`$ndate -${hrint} $eymdh`
    done

    cat ${netdir}/vfile.flipped  >${netdir}/vfile.begtonow

    rm ${netdir}/vfile.nowtoend
    iymdh=${ymdh}
    while [ $iymdh -le $endymdh ]
    do
      grep ${iymdh} ${netdir}/vfile.temp >>${netdir}/vfile.nowtoend
      iymdh=`$ndate $hrint $iymdh`
    done

    cat ${netdir}/vfile.begtonow ${netdir}/vfile.nowtoend ;;

  *)

    while [ $iymdh -le $endymdh ]
    do
      grep ${iymdh} ${netdir}/vfile.temp
      iymdh=`$ndate $hrint $iymdh`
    done;;

esac
