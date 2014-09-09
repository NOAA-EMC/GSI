#!/bin/ksh

if [ $# -lt 1 ]; then 
  echo " "
  echo " !!! ERROR: You must enter at least 5 arguments:"
  echo " !!!"
  echo " !!! USAGE: `basename $0` -t fcst_type -d ymdh -f atcfname -u userid model1 .... [model(n)]"
  echo " "
  echo "     fcst_type = single, mult06, mult12, mult24"
  echo "     ymdh      = In format of 2001091012, 2002081400, etc...."
  echo "     atcfname  = In format of al042001, ep112002, etc...."
  echo "     model     = GFDL, LBAR, OFCL, AVNO, etc....."
  echo "     userid    = wx20tm, wx23ph, wx20tr, etc...."
  echo " "
  exit 8
fi

while getopts t:f:d:u: opt
do
  case $opt in
    t) ftype="$OPTARG";;
    f) afile="$OPTARG";;
    d) ymdh="$OPTARG";;
    u) userid="$OPTARG";;
    \?) err=1;;
  esac
done

export netdir=${netdir:-/stmp/$LOGNAME/tracks/pru12c}
export ndate=$scrdir/ndate.x

# echo " "
# echo "After while loop, ftype= $ftype"
# echo "                  afile= $afile"
# echo "                   ymdh= $ymdh"
# echo " "

let shiftnum=OPTIND-1
shift $shiftnum

modlist=""

#---------------------------------
# Process the list of models that
# were entered on the input line.

modct=0
while [ $# -gt 0 ] 
do

  let modct=modct+1
  model=$1

  modlist="$modlist $model"

  shift

done

# echo " "
# echo "After while loop, modlist follows: "
# echo " "
# echo "$modlist"
# echo " "

#--------------------------------
# grep out the dates we'll be using
# and then grep the models out of 
# there and into a temp flist file.

atcffile=a${afile}.dat

if [ ${ftype} = 'single' ]; then

  grep ${ymdh} ${atcffile} >${netdir}/tempfile

  for model in $modlist
  do

    grep -i "${model}," ${netdir}/tempfile |\
      awk '{
              lat10 = substr($0,36,3)
              latns = substr($0,39,1)
              if (latns == "N")  ylat = lat10/10.0
              if (latns == "S")  ylat = lat10/-10.0

              lon10 = substr($0,42,4)
              lonew = substr($0,46,1)
              if (lonew == "W")  xlon = lon10/10.0
              if (lonew == "E")  xlon = 360. - lon10/10.0

              printf ("%10s  %4s  %3d  %5.1f  %5.1f  %2s%2s%2s\n",
              substr($0,9,10),
              substr($0,25,4),
              substr($0,31,3),
              ylat,
              xlon,
              substr($0,1,2),substr($0,5,2),substr($0,11,2))
      }' 

  done 

else

  if [ $modct -gt 1 ]; then
    echo " "
    echo "!!! ERROR: For displaying multiple forecasts from multiple"
    echo "!!!        times, you can only pick one model, and you"
    echo "!!!        have picked ${modct}:"
    echo "!!!     "
    echo "!!!        $modlist"
    echo " "
    exit 8
  fi

  grep -i "${model}," ${atcffile} |\
    awk '{
           lat10 = substr($0,36,3)
           latns = substr($0,39,1)
           if (latns == "N")  ylat = lat10/10.0
           if (latns == "S")  ylat = lat10/-10.0

           lon10 = substr($0,42,4)
           lonew = substr($0,46,1)
           if (lonew == "W")  xlon = lon10/10.0
           if (lonew == "E")  xlon = 360. - lon10/10.0

           printf ("%10s  %4s  %3d  %5.1f  %5.1f  %2s%2s%2s\n",
           substr($0,9,10),
           substr($0,25,4),
           substr($0,31,3),
           ylat,
           xlon,
           substr($0,1,2),substr($0,5,2),substr($0,11,2))
     }' >${netdir}/tempfile

  begymdh=${ymdh}
  endymdh=` cat ${netdir}/tempfile |\
             tail -1               |\
             awk '{print $1}'`

  case $ftype in
    "mult06") hrint=6;;
    "mult12") hrint=12;;
    "mult24") hrint=24;;
  esac

  if [ -s ${netdir}/tempfile2 ]; then 
    rm    ${netdir}/tempfile2
    touch ${netdir}/tempfile2
  fi

  iymdh=${begymdh}
  while [ $iymdh -le $endymdh ]
  do
    grep ${iymdh} ${netdir}/tempfile >>${netdir}/tempfile2
    iymdh=`$ndate $hrint $iymdh`
  done

  cat ${netdir}/tempfile2

fi
