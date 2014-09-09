#!/bin/ksh
set -x

# get experimental GFS forecasts and GDAS prepbufr data online or from HPSS archive.          
# if experimental GDAS prepbufr does not exist, get operational GDAS prepbufr.

comout=${1:-/stmp/$LOGNAME/g2o/00Z/prsl19}        ;#temporary directory
exp=${2:-prsl19}                                  ;#experiment name
cdate=${3:-2010090100}                            ;#forecast cycle         
vlength=${4:-120}                                 ;#verification end hour of forecast
fhout=${5:-6}                                     ;#output frequency
vhlist=${6:-"00 06 12 18"}                        ;#prepbufr from gdas analysis cycles

hpsstar=${HPSSTAR:-/u/Fanglin.Yang/bin/hpsstar}
chost=`echo $(hostname) |cut -c 1-1`
exp_dir=${exp_dir:-/global/hires/glopara/archive} ;#experiment directory
hpssdir=${hpssdir:-/NCEPDEV/1year/hpsspara/runhistory/glopara}  ;#HPSS archive directory
gdas_prepbufr_arch=${gdas_prepbufr_arch:-/global/noscrub/Fanglin.Yang/prepbufr/gdas}
scppgb=${scppgb:-"NO"}                            ;#copy pgb files from other computer?
CLIENT=${CLIENT:-"gyre"}                          ;#name of another computer
cdump=${cdump:-".gfs."}                           ;#file dump format
fsub=${fsub:-f}                                   ;# string in pgb fcst file after pg

if [ ! -s $comout ]; then mkdir -p $comout ;fi
cd $comout  ||exit 8

IDAY=`echo $cdate |cut -c 1-8`
yyyy=`echo $cdate |cut -c 1-4 `
mm=`echo $cdate |cut -c 5-6 `
dd=`echo $cdate |cut -c 7-8 `
fcyc=`echo $cdate |cut -c 9-10 `

errexp=0
ffcst=00
while [ $ffcst -le $vlength ] ; do
  filein=$exp_dir/pgb${fsub}${ffcst}${cdump}${IDAY}${fcyc}
  fileout=pgbf${ffcst}.${exp}.${IDAY}${fcyc}
  rm $fileout
  if [ -s $filein ]; then
    ln -fs  $filein $fileout
  elif [ $scppgb = YES ]; then
   scp -p ${LOGNAME}@${CLIENT}:$filein $fileout
  fi
  if [ ! -s $fileout ]; then errexp=1 ; fi
  ffcst=$((ffcst+fhout))
  if [ $ffcst -lt 10 ]; then ffcst=0$ffcst ; fi
done

#errgdas=0
#for vcyc in $vhlist; do
#  filein=$exp_dir/prepbufr.gdas.${IDAY}${vcyc}
#  fileout=prepbufr.gdas.${IDAY}${vcyc}  
#  if [ -s $filein ]; then
#   ln -fs $filein $fileout
#  elif [ $scppgb = YES ]; then
#    scp -p ${LOGNAME}@${CLIENT}:$filein $fileout
#  fi
#  if [ ! -s $fileout ]; then errgdas=1 ; fi
#done

errgdas=0
for vcyc in $vhlist; do
  fileina=/com/gfs/prod/gdas.$IDAY/gdas1.t${vcyc}z.prepbufr
  fileinb=${gdas_prepbufr_arch}/prepbufr.gdas.${IDAY}${vcyc}
  fileout=prepbufr.gdas.${IDAY}${vcyc}
  if [ -s $fileina ]; then
    ln -fs $fileina $fileout
  elif [ -s $fileinb ]; then
    ln -fs $fileinb $fileout
  else
   errgdas=1
  fi
done


#--------------------------------------------------------
#--------------------------------------------------------
#--get data from HPSS archive if they do not exist online
if [ $errexp -ne 0 ]; then
 rm pgbflist
 >pgbflist
 ffcst=00        
 while [ $ffcst -le $vlength ] ; do
  echo pgb${fsub}${ffcst}${cdump}${IDAY}${fcyc} >>pgbflist
  ffcst=`expr $ffcst + $fhout `
  if [ $ffcst -lt 10 ]; then ffcst=0$ffcst ; fi
 done

#--extract data and rename files 
 $hpsstar get $hpssdir/$exp/${cdate}gfs.tar `cat pgbflist `
if [ $? -ne 0 ]; then
 rm gethpss.sh
cat > gethpss.sh <<EOF
  cd $comout
  $hpsstar get $hpssdir/$exp/${cdate}gfs.tar \`cat pgbflist \`
EOF
 chmod a+x gethpss.sh
 $SUBJOB -a $ACCOUNT -q $CUE2FTP -p 1/1/S -r 1024/1 -t 00:30:00 -j gethpss \
    -o gethpss.out $comout/gethpss.sh
  nsleep=0; tsleep=30; msleep=40
  while test ! -s pgb${fsub}${vlength}${cdump}${IDAY}${fcyc} -a $nsleep -lt $msleep;do
    sleep $tsleep ; nsleep=`expr $nsleep + 1`
  done
fi
 ffcst=00
 while [ $ffcst -le $vlength ] ; do
  if [ $cdump != .${exp}. ]; then 
   mv pgb${fsub}${ffcst}${cdump}${IDAY}${fcyc} pgbf${ffcst}.${exp}.$cdate
  fi
  ffcst=`expr $ffcst + $fhout `
  if [ $ffcst -lt 10 ]; then ffcst=0$ffcst ; fi
 done
fi

#-get operational GDAS prepbufr files 
if [ $errgdas -ne 0 ]; then
 hpssdir_opsgdas=/NCEPPROD/hpssprod/runhistory/rh${yyyy}/${yyyy}${mm}/${yyyy}${mm}${dd}
 for vcyc in $vhlist; do
  if [ ! -s prepbufr.gdas.${yyyy}${mm}${dd}${vcyc} ]; then
   $hpsstar get ${hpssdir_opsgdas}/com_gfs_prod_gdas.${yyyy}${mm}${dd}${vcyc}.tar   ./gdas1.t${vcyc}z.prepbufr
   mv gdas1.t${vcyc}z.prepbufr prepbufr.gdas.${yyyy}${mm}${dd}${vcyc}
  fi
 done
fi

exit
