#!/bin/ksh
set -x

##-----------------------------------------------------------------------------------------------------
## Fanglin Yang, 09-23-2011
## Purpose: Precipitation threat score verification over CONUS.
## 1. works for 00Z and 12Z cycles of forecast, any output frequency, either with or without a precip bucket.
## 2. Since the observation is for 24-hr accumulation from 12Z to 12Z, then
##    a). for 00Z-cyc forecast:  validation for 12-36 36-60, 60-84 ... hours of accumulated precip.
##    b). for 12Z-cyc forecast:  validation for 00-24 24-48, 48-72 ... hours of accumulated precip.
##-----------------------------------------------------------------------------------------------------

#
exp=$1                                    ;#experiment name
sdate=$2                                  ;#yyyymmddhh
edate=$3                                  ;#yyyymmddhh

file_type=${4:-${file_type:-"flx"}}       ;#file format ${file_type}f${fhr}${cdump}${CDATE}
precip_type=${5:-${precip_type:-"PRATE"}} ;#rates--PRATE, accumulated--APCP
fhout=${6:-${fhout:-6}}                   ;#forecast output frequency in hours
bucket=${7:-${bucket:-6}}                 ;#accumulation bucket in hours. bucket=0 -- continuous accumulation

cdump=${cdump:-".gfs."}
cyc=`echo $sdate |cut -c 9-10`            ;#forecast cycle to verify
start_hour=${start_hour:-$cyc}
fhe=${fhend:-84}                          ;#verification length
exps=`echo $exp |cut -c 1-8`              ;#limit length of experiment name to 8

rundir=${rundir:-/stmp/$LOGNAME/mkup_precip}
mkdir -p $rundir
#
DATDIR=$rundir/$exp          
COMSTAT=${ARCDIR:-$rundir}
COMDAT=${scrdir:-/global/save/wx24fy/VRFY/vsdb/precip}/data
COMEXE=${scrdir:-/global/save/wx24fy/VRFY/vsdb/precip}/exec

OBSPCP=${OBSPCP:-/global/shared/stat/OBSPRCP}
PRECIP_DATA_PREFIX=${PRECIP_DATA_PREFIX:-usa-dlyprcp}

NWPROD=${NWPROD:-/nwprod}
ndate=${ndate:-$NWPROD/util/exec/ndate}
wgrib=${wgrib:-$NWPROD/util/exec/wgrib}

## mean prcip rate in a period
if [ $precip_type = PRATE ]; then kpds5=59 ; fi
## accumulated precip for period
if [ $precip_type = APCP ]; then kpds5=61 ; fi

CDATE=$sdate
while [ $CDATE -le $edate ]; do
  STYMD=`echo $CDATE | cut -c1-8`

  TMPDIR=$rundir/$CDATE
  mkdir -p $TMPDIR; cd $TMPDIR  ||exit 8
  rm *
  LOCDIR=$TMPDIR/$exps
  if [ -s $LOCDIR ]; then rm -rf $LOCDIR; fi
  mkdir -p $LOCDIR


  fh=0
  while [ $fh -lt $fhe ]; do
    fhp=`expr $fh + $fhout `
    nxx=`expr $fhp + $cyc + 6 `
    xx=$(( (nxx/24)*24 )) 
    cdatex=`$ndate -$xx $CDATE`
    ymdx=`echo $cdatex |cut -c 1-8 `

    if [ $fh -lt 10 ]; then fh=0$fh ; fi
    if [ $fhp -lt 10 ]; then fhp=0$fhp ; fi
    filein=$DATDIR/${file_type}f${fhp}${cdump}$cdatex 
    fileout=$LOCDIR/${exps}_${ymdx}${cyc}_${fh}_${fhp}
    $wgrib $filein | grep "$precip_type" | $wgrib -i -grib $filein -o $fileout

    fh=`expr $fh + $fhout `
  done


#-- for continuous accumulation without buckets
 if [ $bucket -eq 0 ]; then
  fh=0
  while [ $fh -lt $fhe ]; do
    fhp=`expr $fh + $fhout `
    nxx=`expr $fhp + $cyc + 30 `
    xx=$(( (nxx/24)*24 )) 
    cdatex=`$ndate -$xx $CDATE`
    ymdx=`echo $cdatex |cut -c 1-8 `

    if [ $fh -lt 10 ]; then fh=0$fh ; fi
    if [ $fhp -lt 10 ]; then fhp=0$fhp ; fi
    filein=$DATDIR/${file_type}f${fhp}${cdump}$cdatex 
    fileout=$LOCDIR/${exps}_${ymdx}${cyc}_${fh}_${fhp}
    $wgrib $filein | grep "$precip_type" | $wgrib -i -grib $filein -o $fileout

    fh=`expr $fh + $fhout `
  done
 fi



  PRECIP_DATA_FILE=${PRECIP_DATA_PREFIX}-$STYMD
  cp $OBSPCP/${PRECIP_DATA_FILE} .

#-- 1.0 is bias correction facter 
cat <<nameEOF >input_runv
model_info_file
$COMDAT/pcpmask                                
$TMPDIR                 
$PRECIP_DATA_FILE
$start_hour
$kpds5
1.0
nameEOF

ngrid=-1        ;# grid type, obsolete. -1 MEANS 1ST;  144*73 resolution is 2; 360*181 resolution is 3
ncyc=1          ;# number of cycles, possibily 4 for GFS

cat <<modelEOF >model_info_file
$exps    
$ngrid   
$ncyc
$cyc
$fhout
$fhe 
$bucket
done
modelEOF

### obs_box.dat contains observation analysis at each grid points
### stat.out contains verification output

  if [ -s obs_box.dat ]; then
   rm -f obs_box.dat
  fi

  if [ $STYMD -le 20111122 -o $STYMD -eq 20111212 ]; then
   $COMEXE/PVRFY_bf20111122      <input_runv
  else
   $COMEXE/PVRFY      <input_runv
  fi

  if [ $cyc -eq 0 ]; then
   cp stat.out $COMSTAT/${exp}_rain_$STYMD
  else
   cp stat.out $COMSTAT/${exp}_rain_$STYMD${cyc}
  fi

 CDATE=`$ndate +24 $CDATE`
done
