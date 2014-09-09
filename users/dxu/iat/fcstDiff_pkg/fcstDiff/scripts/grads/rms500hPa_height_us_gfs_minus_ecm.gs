#!/bin/ksh
# Author: V. Krishna Kumar, Systems Integration Branch, April 2008
# Run this script as follows (the last "b" is for batch mode)
# Minimum seven arguments - the eighth argument "b" is for batch mode
# sh rms500hPa_height_us_gfs_minus_ecm.gs1 ICdate(yyyymmdd) forecast_hour cycle Ver_anal_date Ver_fhr Ver_cyc lev b
# where lev should be in h Pa for example 500
#                                          icdate   fcst cyc verdate verfcst vercyc lev batch 
# sh rms500hPa_height_us_gfs_minus_ecm.gs 20071021 24  12 20071022 00 12 500 b
#
set -x
gfsdir=/global/shared/stat/prs

export tyyyymmdd=${1}
echo " Top panel initial conditions date (yyyymmdd)=$tyyyymmdd "
export tyyyy=`echo $tyyyymmdd | cut -c1-4`
export tmm=`echo $tyyyymmdd | cut -c5-6` 
export tdd=`echo $tyyyymmdd | cut -c7-8`
#
export fhr=${2}
echo " forecast hour (00,24,48,...)=$fhr "
#
export cyc=${3}
echo " cyc (00,06,12,...)=$cyc "
#
export vatyyyymmdd=${4}
echo " ver anal date top (yyyymmdd)=$vatyyyymmdd "
export vatyyyy=`echo $vatyyyymmdd | cut -c1-4`
export vatmm=`echo $vatyyyymmdd | cut -c5-6` 
export vatdd=`echo $vatyyyymmdd | cut -c7-8`
#
export vafhr=${5}
echo " verifying analysis hour (00)=$vafhr "
#
export vacyc=${6}
echo " veri anal cyc (00,06,12,...)=$vacyc "
#
export lev_select=${7:-"1000"}
echo " lev_select=$lev_select "

#DATA DIRECTORIES
export gfsdir=/global/shared/stat/prs
export ecmdir=/global/noscrub/wx23dc/ecm$tmm$tdd$cyc

cat << EOF > mapset.gs
*
*RMS height error differences between GFS and ECMWF for day-1,2,3,4,5
*forecast - verifying against its own analysis   
*
* Top panel for first date
*
'open pgbt.gfs.fcst.ctl'
'open pgbt.gfs.anal.ctl'
'open pgbt.ecm.fcst.ctl'
'open pgbt.ecm.anal.ctl'
*
'set vpage 0 8.5   0   11'
'set parea 0.8 8.3 6.5 10.5'
*'set map 15 1 6'
'set map 1 1 6'
'set mproj scaled'
*'set mproj nps'
'set lon -180 -50'
'set lat 15 70'
'set grads off'
'set lev $lev_select'
*'set time ${cyc}Z${tdd}oct${tyyyy}'
'set gxout shaded'
'define fmag = hgtprs.1-hgtprs.2'
'define sfmag = pow(fmag,2)'
'define rmseg = sqrt(sfmag)'
*
'define fmae = hgtprs.3-hgtprs.4'
'define sfmae = pow(fmae,2)'
'define rmsee = sqrt(sfmae)'
*
*RMS error differences between gfs and ecm day1 fcst
*
'define rmsed = rmseg-rmsee'
'run rgbset_redblue.gs'
'set clevs -50 -45 -40 -35 -30 -25 -20 -15 -10 -5 0 5 10 15 20 25 30 35 40 45 50'
'set ccols 49 48 47 46 45 44 43 42 41 40 0 0 20 21 22 23 24 25 26 27 28 29'
'd rmsed'
'set gxout vector'
'set cthick 4'
'set arrscl 0.20 5'
'set grid off'
*'Taking the difference between fcst & analy gfs and ecm'
*gfs
'define ufmag=ugrdprs.1-ugrdprs.2'
'define vfmag=vgrdprs.1-vgrdprs.2'
*ecm
'define ufmae=ugrdprs.3-ugrdprs.4'
'define vfmae=vgrdprs.3-vgrdprs.4'
*Vector error differences between gfs and ecm day1 fcst
'define ugmed=ufmag-ufmae' 
'define vgmed=vfmag-vfmae' 
'd skip(ugmed,4,2);vgmed'
'draw xlab LONGITUDE'
'draw ylab LATITUDE'
'set strsiz 0.12'
'set string 1 l 6'
'draw string 1.5 10.7 VDIFF(GFS-ECM)(m/s) & RMSHGT DIFF ${lev_select} mb $tyyyymmdd ${cyc}Z (GFS-ECM) f$fhr'
********************************************
'run cbarnew.gs'
'set strsiz 0.1'
'set string 1 l 6'
'draw string 3.5 0.15 WIND IMPACTS ON ANALYSIS - GFS vs ECM'
*******************************************
*'print'
*'disable print'
'printim gfs-ecm_RMSVDIFF_t${cyc}z_${tyyyy}$tmm${tdd}_${lev_select}hPa_f${fhr}.gif gif x640 y824 white'
'quit'
*
*
*
EOF

#
# begin grads setup
#
'open pgbt.gfs.fcst.ctl'
'open pgbt.gfs.anal.ctl'
'open pgbt.ecm.fcst.ctl'
'open pgbt.ecm.anal.ctl'

# GFS Forecast File Top Panel
/u/wx20mi/bin/grib2ctl.pl -verf $gfsdir/pgbf${fhr}.${tyyyymmdd}${cyc} > pgbt.gfs.fcst.ctl
/usrx/local/grads/bin/gribmap -i pgbt.gfs.fcst.ctl
# GFS Verifying Analysis File Top Panel
/u/wx20mi/bin/grib2ctl.pl -verf $gfsdir/pgbf${vafhr}.${vatyyyymmdd}${vacyc} > pgbt.gfs.anal.ctl
/usrx/local/grads/bin/gribmap -i pgbt.gfs.anal.ctl
# ECMWF Forecast File Top Panel
/u/wx20mi/bin/grib2ctl.pl -verf $ecmdir/pgbf${fhr}.gfs.${tyyyymmdd}${cyc} > pgbt.ecm.fcst.ctl
/usrx/local/grads/bin/gribmap -i pgbt.ecm.fcst.ctl
# ECMWF Verifying Analysis File Top Panel
/u/wx20mi/bin/grib2ctl.pl -verf $ecmdir/pgbf${vafhr}.gfs.${vatyyyymmdd}${vacyc} > pgbt.ecm.anal.ctl
/usrx/local/grads/bin/gribmap -i pgbt.ecm.anal.ctl

  if [ $8 = "b" ] ; then
/usrx/local/grads/bin/grads -pbc "run mapset.gs"
  else
/usrx/local/grads/bin/grads -pc "run mapset.gs"
  fi

