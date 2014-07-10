#!/bin/sh
set -x
#--This script converts CMA GRAPES forecasts in binary 
#  format into GRIB1 format (NCEP grib1 table).
#  Fanglin Yang (NOAA/NCEP/EMC, fanglin.yang@noaa.gov)
#  December 2011.
#

export exp=${1:-noheat}             ;#experiment name 
export DATEST=${2:-20090701}      ;#starting date
export DATEND=${3:-20090731}      ;#ending date
export cyc=${4:-12}               ;#forecast cycle
export fhmax=${5:-192}            ;#maximum forecast hours
export fhout=${6:-24}             ;#forecast output frequency 
export nlats=${7:-180}            ;#latitudinal points
export nlon=${8:-360}             ;#longitudinal points
export nlev=${9:-29}              ;#vertical layers    

export datasrc=${datasrc:-/dgpfs/fs5/nwp_ex/hujl/grapes/DATABAK/FCST_results}   ;#source in binary format
export exp_dir=${exp_dir:-/u/zhaobin/zhaobin/yangfanglin/testrun}    ;#grib1 output directory      
export rundir=${rundir:-/u/zhaobin/zhaobin/yangfanglin/testrun/tmp/bin2grid}  ;#temporary running directory
if [ ! -s $exp_dir/$exp ]; then mkdir -p $exp_dir/$exp ; fi
if [ ! -s $rundir ]; then mkdir -p $rundir  ; fi
cd $rundir || exit 8
rm postvar* pgbf* 

export NWPROD=${NWPROD:-/u/zhaobin/zhaobin/yangfanglin/vsdb/nwprod}
export ndate=${ndate:-$NWPROD/util/exec/ndate}
export PGM=$NWPROD/util/exec/bin2grib1_grapes.x 

#-------------------------------------------
cdate=${DATEST}${cyc}
while [ $cdate -le ${DATEND}${cyc} ]; do

#--link input
rm fort.*
filein=${datasrc}/${exp}/postvar${cdate}
 if [ -s $filein ]; then
  ln -fs  $filein fort.10
  if [ -s card.txt ]; then rm card.txt ; fi
cat >card.txt <<EOF
$cdate
0 $fhmax $fhout
$nlev $nlats $nlon
1000
962
925 
887
850
800
750
700
650
600
550
500
450
400
350
300
275
250
225
200
175
150
125
100
70
50
30
20
10
EOF
  eval $PGM < card.txt
 else
  echo " $filein missing"
 fi

#--rename output
fh=00
while [ $fh -le $fhmax ]; do
 mv  pgbf$fh   ${exp_dir}/${exp}/pgbf${fh}.grapes.${cdate}
 fh=`expr $fh + $fhout `
 [[ $fh -lt 10 ]] && fh=0$fh
done

cdate=$($ndate +24 $cdate)
done

exit
