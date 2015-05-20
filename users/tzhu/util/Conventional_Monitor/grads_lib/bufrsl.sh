#!/bin/ksh
set -xeua


##  iall=0 gives only obs value and quality mark
##  iall=1 gives only obs,anl,ges and quality mark
export iall=1

export sdate=2000080200
export edate=2000080200
export incr=24

export idbug=1
export idbuga=0

###########  US
export flonw=180.0
export flone=320.0
export flats=5.0
export flatn=75.0

###########  Rapid City
export flonw=250.0
export flone=280.0
export flats=35.0
export flatn=50.0

###########  India
export flonw=55.0
export flone=100.0
export flats=5.0
export flatn=45.0

###########  Africa
#export flonw=-30.0
#export flone=60.0
#export flats=-40.0
#export flatn=40.0

###########  GL
export flonw=0.0
export flone=360.0
export flats=-90.0
export flatn=90.0

#for mdl in fnl avn eta ruc
for mdl in fnl
do

date=$sdate
until [ $date -gt $edate ] ; do

yyyy=`echo $date | cut -c1-4`
yy=`echo $date | cut -c3-4`
mm=`echo $date | cut -c5-6`
mon=`/nfsuser/g01/wx23ss/amip/scripts/cmon.sh $mm`
dd=`echo $date | cut -c7-8`
hh=`echo $date | cut -c9-10`
odate=$yy$mm$dd$hh

if [ $mdl = "fnl" ] ; then
infile=/com/fnl/prod/fnl.$yyyy$mm$dd/gdas1.t${hh}z.bufprepda
#infile=/gpfsuser/g01/wx20rt/prx/run1/prepqa.$date
fi
if [ $mdl = "avn" ] ; then
infile=/com/avn/prod/avn.$yyyy$mm$dd/gblav.t${hh}z.bufprepda
fi
if [ $mdl = "eta" ] ; then
#infile=/com/eta/prod/eta.$yyyy$mm$dd/eta.t${hh}z.prepda.tm00
infile=/gpfstmp/wx20er/tempsat/edas/edas.t12z.prepda.tm12
fi
if [ $mdl = "prx" ] ; then
#infile=/gpfsuser/g01/wx20rt/prx/run1/prepqa.$date
#infile=/gloptmp/datprx/prepqf.$date
infile=/gloptmp/datprx/prepqa.$date
fi
if [ $mdl = "pro" ] ; then
infile=/gpfsuser/g01/wx23ss/pro/run1/prepqa.$date
fi

outdir=/ptmp/wx23ss/bufr/$mdl
if [ ! -d $outdir ] ; then
  mkdir -p $outdir
fi

cd $outdir

#for name in ADPSFC SFCSHP ADPUPA PROFLR SATWND AIRCFT AIRCAR SATEMP SYNDAT VADWND SPSSMI
for name in  SATWND
do

export catname=$name
export oname=` echo $catname| tr "[A-Z]" "[a-z]" `

#####################################################################
#                  ADPSFC
#####################################################################
if [ $catname = "ADPSFC" ] ; then
export nst=20000

export imas=1
export iwnd=0
export sfc=1
#for hr in -3 -2 -1 0 1 2
for hr in 0
do
export fchr=$hr
export fdhr=0.5
export dtg=`/nwprod/util/exec/ndate $hr $date`
/nfsuser/g01/wx23ss/amip/scripts/surusbufr_y2k.sh $oname $nst $iall $imas $iwnd $fchr $fdhr $dtg $sfc 
done
fi

#####################################################################
#                  SFCSHP
#####################################################################
if [ $catname = "SFCSHP" ] ; then
export nst=10000
export imas=1
export iwnd=1
export sfc=1
#for hr in -3 -2 -1 0 1 2
for hr in 0
do
export fchr=$hr
export fdhr=0.5
export dtg=`/nwprod/util/exec/ndate $hr $date`
/nfsuser/g01/wx23ss/amip/scripts/surusbufr_y2k.sh $oname $nst $iall $imas $iwnd $fchr $fdhr $dtg $sfc 
done
fi

#####################################################################
#                  ADPUPA
#####################################################################
if [ $catname = "ADPUPA" ] ; then
export imas=1
export iwnd=1
export sfc=2
##   mandatory level data...
export nst=1000
export iprs=1
export ilev=21
export itol=0
##   close to model sigma level data...
#export nst=1000
#export iprs=0
#export ilev=28
#export itol=2
##   all data...
#export nst=800
#export iprs=2
#export ilev=75
#export itol=0

for hr in 0
do
export fchr=$hr
export fdhr=4.5
export dtg=`/nwprod/util/exec/ndate $hr $date`
/nfsuser/g01/wx23ss/amip/scripts/surusbufr_y2k.sh $oname $nst $iall $imas $iwnd $fchr $fdhr $dtg $sfc 
done
fi

#####################################################################
#                  PROFLR
#####################################################################
if [ $catname = "PROFLR" ] ; then
export nst=151
export imas=0
export iwnd=1
export sfc=2
##   mandatory level data...
#export iprs=1
#export ilev=21
#export itol=2
##   close to model sigma level data...
#export iprs=0
#export ilev=28
#export itol=2
##   all data...
export iprs=2
export ilev=100
export itol=0
#for hr in -3 -2 -1 0 1 2 3
for hr in 0
do
export fchr=$hr
export fdhr=0.5
dtg=`/nwprod/util/exec/ndate $hr $date`
/nfsuser/g01/wx23ss/amip/scripts/surusbufr_y2k.sh $oname $nst $iall $imas $iwnd $fchr $fdhr $dtg $sfc 
done
fi

#####################################################################
if [ $catname = "VADWND" ] ; then
export nst=150
export imas=0
export iwnd=1
export sfc=2
##   all data...
export iprs=2
export ilev=100
export itol=0
#for hr in 0
for hr in -3 -2 -1 0 1 2 3
do
export fchr=$hr
export fdhr=0.5
#export fdhr=4.5
export dtg=`/nwprod/util/exec/ndate $hr $date`
/nfsuser/g01/wx23ss/amip/scripts/surusbufr_y2k.sh $oname $nst $iall $imas $iwnd $fchr $fdhr $dtg $sfc 
done
fi

#####################################################################
#####################################################################
#                  SATEMP
#####################################################################
if [ $catname = "SATEMP" ] ; then
export nst=10000
export imas=0
export iwnd=0
export sfc=4
export ilev=29
#for hr in -3 -2 -1 0 1 2 3
for hr in 0
do
export fchr=$hr
export fdhr=0.5
export fdhr=4.0
export dtg=`/nwprod/util/exec/ndate $hr $date`
/nfsuser/g01/wx23ss/amip/scripts/surusbufr_y2k.sh $oname $nst $iall $imas $iwnd $fchr $fdhr $dtg $sfc 
done
fi

#####################################################################
#                  SATWND
#####################################################################
if [ $catname = "SATWND" ] ; then
export nst=50000
export imas=0
export iwnd=1
export sfc=3
#for hr in -3 -2 -1 0 1 2
for hr in 0
do
export fchr=$hr
#export fdhr=0.5
export fdhr=4.
export dtg=`/nwprod/util/exec/ndate $hr $date`
/nfsuser/g01/wx23ss/amip/scripts/surusbufr_y2k.sh $oname $nst $iall $imas $iwnd $fchr $fdhr $dtg $sfc 
done
fi

#####################################################################
#                  SPSSMI
#####################################################################
if [ $catname = "SPSSMI" ] ; then
export nst=20000
export imas=1
export iwnd=0
export sfc=5
#for hr in -3 -2 -1 0 1 2
for hr in 0
do
export fchr=$hr
#export fdhr=0.5
export fdhr=4.
export dtg=`/nwprod/util/exec/ndate $hr $date`
/nfsuser/g01/wx23ss/amip/scripts/surusbufr_y2k.sh $oname $nst $iall $imas $iwnd $fchr $fdhr $dtg $sfc 
done
fi

#####################################################################
#                  AIRCFT
#####################################################################
if [ $catname = "AIRCFT" ] ; then
export nst=20000
export imas=1
export iwnd=1
export sfc=3
#for hr in -3 -2 -1 0 1 2
for hr in 0
do
export fchr=$hr
#export fdhr=0.5
export fdhr=4.0
export dtg=`/nwprod/util/exec/ndate $hr $date`
/nfsuser/g01/wx23ss/amip/scripts/surusbufr_y2k.sh $oname $nst $iall $imas $iwnd $fchr $fdhr $dtg $sfc 
done
fi

#####################################################################
#                  AIRCAR
#####################################################################
if [ $catname = "AIRCAR" ] ; then
export nst=20000
export imas=1
export iwnd=1
export sfc=3
#for hr in -3 -2 -1 0 1 2
for hr in 0
do
export fchr=$hr
#export fdhr=0.5
export fdhr=4.0
export dtg=`/nwprod/util/exec/ndate $hr $date`
/nfsuser/g01/wx23ss/amip/scripts/surusbufr_y2k.sh $oname $nst $iall $imas $iwnd $fchr $fdhr $dtg $sfc 
done
fi

#####################################################################
#                  SYNDAT
#####################################################################
if [ $catname = "SYNDAT" ] ; then
export imas=1
export iwnd=1
export sfc=2
##   mandatory level data...
#export nst=1000
#export iprs=1
#export ilev=21
#export itol=0
##   close to model sigma level data...
#export nst=1000
#export iprs=0
#export ilev=28
#export itol=2
##   all data...
export nst=150
export iprs=2
export ilev=75
export itol=0
for hr in 0
do
export fchr=$hr
export fdhr=4.0
export dtg=`/nwprod/util/exec/ndate $hr $date`
/nfsuser/g01/wx23ss/amip/scripts/surusbufr_y2k.sh $oname $nst $iall $imas $iwnd $fchr $fdhr $dtg $sfc 
done
fi

#####################################################################
##  close catname-loop
done

##  close date-loop
date=`/nwprod/util/exec/ndate $incr $date`
done

##  close model-loop
done
