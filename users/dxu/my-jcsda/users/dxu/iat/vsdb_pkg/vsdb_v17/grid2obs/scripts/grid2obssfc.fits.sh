#!/bin/ksh
#set -x

#---------------------------------------------------------------------
#--Processing fits for surface variables using nam or ndas analysis over
#  subregions of the NAM area for all forecast cycles and all verifying hours.
#  Fanglin Yang, November 2011
#---------------------------------------------------------------------

export PLLN=$1      ;#experiment name
export vdate=$2     ;#verification time yyyymmddhh
export vlength=$3   ;#verification length in hours
export cyc=$4       ;#forecast cycle to verify 
export HOLDOUT=$5   ;#temporary running directory        
export DDIR=$6      ;#experiment data directory
 
export grid2obshome=${grid2obshome:-/global/save/wx24fy/VRFY/vsdb/grid2obs}
export PARMDIR=${PARMDIR:-$grid2obshome/parm}
export SCRIPTDIR=${SCRIPTDIR:-$grid2obshome/scripts}
export NWPROD=${NWPROD:-/nwprod}
export ndate=${ndate:-$NWPROD/util/exec/ndate}
export HPSSTAR=${HPSSTAR:-/u/Fanglin.Yang/bin/hpsstar}
export grbindex=${grbindex:-$NWPROD/util/exec/grbindex}
export gdtype=${gdtype:-3}      ;#fcst data resolution, 2-2.5deg, 3-1deg., 4-0.5deg

#--------------------------------------------------
export PLL3=`echo $PLLN |tr "[a-z]" "[A-Z]" `
export pll3=$PLLN

export RUNDIR=$HOLDOUT/prepfitsfc_${PLLN}.${vdate}
mkdir -p $RUNDIR; cd $RUNDIR ||exit 8
rm $RUNDIR/*

#-----------------------------------------------------------------------
# --verification hour 
vhour=$(echo $vdate |cut -c 9-10 )
>prepfits.in${vhour}

fhour1=$((vhour-cyc))                    ;#first forecast hour
fdate1=$(echo $vdate |cut -c 1-8)$cyc    ;#first forecast cycle
if [ $fhour1 -lt 0 ]; then 
  fdate1=$($ndate -24 $fdate1 )
  fhour1=$((24-cyc+vhour))               ;#first forecast hour
fi
if [ $fhour1 -lt 10 ]; then fhour1=0$fhour1; fi

fhour=$fhour1; fdate=$fdate1
while [ $fhour -le $vlength ]; do
 cp $DDIR/pgbf${fhour}.${PLLN}.${fdate}  AWIPD0${fhour}.tm00 
 $grbindex AWIPD0${fhour}.tm00 AWIPD0${fhour}i.tm00

 if [ -s AWIPD0${fhour}.tm00 ]; then
  echo "${pll3}"              >>prepfits.in${vhour}
  echo "AWIPD0${fhour}.tm00"  >>prepfits.in${vhour}
  echo "AWIPD0${fhour}i.tm00" >>prepfits.in${vhour}
 fi

 fhour=`expr $fhour + 24 `
 fdate=$($ndate -24 $fdate ) 
done


#  -------------------------------
#    obtain prepbufr files
#  -------------------------------
echo " verified againt ndas or nam prepbufr.$vdate "
$grid2obshome/scripts/get_ndasprepbufr.sh $vdate 
if [ ! -s prepda.$vdate ]; then
  $SUBJOB -e RUNDIR,vdate,NWPROD,ndate,HPSSTAR -a $ACCOUNT -q $CUE2FTP -p 1/1/S -r 1024/1 -t 2:00:00 -j getbufr \
    -o get_ndasprepbufr.out $grid2obshome/scripts/get_ndasprepbufr.sh
  nsleep=0; tsleep=30; msleep=40     
  while test ! -s prepda.$vdate -a $nsleep -lt $msleep;do
    sleep $tsleep ; nsleep=`expr $nsleep + 1`
  done
fi
if [ ! -s prepda.$vdate ]; then
 echo " prepda.$vdate not found, exit $0 "
 exit
fi

#  ----------------------------------------
#  define a prepbufr file to filter and fit
#  ----------------------------------------
 
bufr=prepda.$vdate    
chmod 700 prepda*

#  -------------------------------------------------------------
#  run editbufr and prepfits on the combined set of observations
#  -------------------------------------------------------------

rm datatmp prepfits.${PLL3}.$vdate       

rm fort.*
ln -sf $bufr         fort.20
ln -sf datatmp       fort.50
sed -e "s/gdtype/$gdtype/g"  $PARMDIR/gridtobs.keeplist.global >gridtobs.keeplist        
$NWPROD/exec/verf_gridtobs_editbufr < gridtobs.keeplist
if [ $? -ne 0 ]; then 
 echo " failed verf_gridtobs_editbufr in $0 , exit" 
 exit
fi

chmod 700 data*

rm fort.*
ln -sf $PARMDIR/gridtobs.levcat.global      fort.11
ln -sf datatmp                              fort.20
ln -sf $PARMDIR/verf_gridtobs.prepfits.tab  fort.22
ln -sf prepfits.${PLL3}.${vdate}            fort.50
$NWPROD/exec/verf_gridtobs_prepfits < prepfits.in${vhour} > prepfit${vhour}.out.${PLLN}
if [ $? -ne 0 ]; then 
 echo " failed verf_gridtobs_prepfits in $0 , exit" 
 exit
fi

chmod 700 prepfits*

rm fort.*
ln -sf prepfits.${PLL3}.${vdate}           fort.10
ln -sf $PARMDIR/verf_gridtobs.grid104      fort.20
ln -sf $PARMDIR/verf_gridtobs.regions      fort.21
ln -sf ${PLLN}_${vdate}.vdb                fort.50

#-- create grid2obs control file
$SCRIPTDIR/grid2obssfc.ctl.sh $fhour1 $vlength

#-- create grid2obs vsdb file 
$NWPROD/exec/verf_gridtobs_gridtobs <grid2obssfc.ctl > gto.${PLLN}${vhour}.out

cp ${PLLN}_${vdate}.vdb ${HOLDOUT}/${PLLN}_sfc_${vdate}.vdb
cd $HOLDOUT
rm -rf $RUNDIR

exit


