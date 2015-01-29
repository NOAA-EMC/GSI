#!/bin/ksh
set -x

## extract gfs forecasts from hpss archive 
# Fanglin Yang

gfs=${1:-gfs}                               
fcsttmp=${2:-/stmp/$LOGNAME/$gfs/prod/${gfs}.20110815}   
inpymdh=${3:-2011081500}                    
nend=${4:-384}                               ;#forecast length in hours
nint=${5:-6}                                ;#output frequency in hours
curdir=`pwd`
nstart=0

PDY=` echo $inpymdh | cut -c1-8`
cyc=` echo $inpymdh | cut -c9-10`
yy=`     echo $PDY | cut -c3-4`
mm=`     echo $PDY | cut -c5-6`
dd=`     echo $PDY | cut -c7-8`
hh=${cyc}
yyyy=`   echo $PDY | cut -c1-4`
yymmdd=` echo $PDY | cut -c3-8`
ymdh=${PDY}${hh}

outdir=$fcsttmp
hpssdir=/NCEPPROD/hpssprod/runhistory/rh${yyyy}/${yyyy}${mm}/${yyyy}${mm}${dd}          ;#2.5-deg, bufr etc
hpssdir1=/NCEPPROD/1year/hpssprod/runhistory/rh${yyyy}/${yyyy}${mm}/${yyyy}${mm}${dd}   ;#1-deg, 0.5-deg pgb files
hpssdir2=/NCEPPROD/2year/hpssprod/runhistory/rh${yyyy}/${yyyy}${mm}/${yyyy}${mm}${dd}   ;#sigma, sfc flux etc


uname -a
echo "hpssdir= $hpssdir"
if [ ! -d $outdir ]; then mkdir -p $outdir; fi

cd $outdir
rm gdas1 pgrbf
#>gdas1$cyc
>pgrbf$cyc

#echo ./gdas1.t${hh}z.pgrbanl            >>gdas1$cyc   
#echo ./gdas1.t${hh}z.pgrbf00            >>gdas1$cyc   


ihr=$nstart
until [ $ihr -gt $nend ]; do
if [ $ihr -lt 10 ]; then ihr=0$ihr; fi
echo ./${gfs}.t${hh}z.pgrbf${ihr}           >>pgrbf$cyc   
  #if [ $ihr -le 180 ]; then echo ./${gfs}.t${hh}z.pgrbf${ihr}.2p5deg; fi           >>pgrbf$cyc   
  #if [ $ihr -gt 180 ]; then echo ./${gfs}.t${hh}z.pgrbf${ihr}; fi           >>pgrbf$cyc   
ihr=` expr $ihr + $nint `
done


#hpsstar get $hpssdir/com_${gfs}_prod_gdas.${ymdh}.tar `cat gdas1$cyc`
hpsstar get $hpssdir/com_${gfs}_prod_${gfs}.${ymdh}.anl.tar ./${gfs}.t${hh}z.pgrbanl

 hpsstar get $hpssdir1/com_${gfs}_prod_${gfs}.${ymdh}.pgrb.tar `cat pgrbf$cyc`
#hpsstar get $hpssdir/com_${gfs}_prod_${gfs}.${ymdh}.pgrb2.tar `cat pgrbf$cyc`
#ihr=$nstart
#until [ $ihr -gt $nend ]; do
#if [ $ihr -lt 10 ]; then ihr=0$ihr; fi
#if [ $ihr -le 180 ]; then mv ${gfs}.t${hh}z.pgrbf${ihr}.2p5deg ${gfs}.t${hh}z.pgrbf${ihr}; fi  
#ihr=` expr $ihr + $nint `
#done


#hpsstar get $hpssdir2/com_${gfs}_prod_${gfs}.${ymdh}.sigma.tar `cat sfname$cyc`
#hpsstar get $hpssdir2/com_${gfs}_prod_${gfs}.${ymdh}.surface.tar `cat bfname$cyc`
#hpsstar get $hpssdir2/com_${gfs}_prod_${gfs}.${ymdh}.sfluxgrb.tar `cat fxname$cyc`

cd $curdir
exit


