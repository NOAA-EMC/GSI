#!/bin/ksh
set -x

export latlon="20 40 280 310"
export fhlist="f00"
export ndays=5

#### ---------------------------------------------------------- 
###  Make area mean time series maps.                     
###  Input must be grib files on regular lat-lon grid and on 
###  isobaric layers in the vertical.
###  Fanglin Yang, EMC/NCEP/NOAA,  June 2012
###  fanglin.yang@noaa.gov; 301-7638000 x7296
#### ---------------------------------------------------------- 

export expnlist=${expnlist:-"pra "}               ;#experiments, up to 8; gfs will point to ops data
export expdlist=${expdlist:-"/global/shared/stat /global/hires/glopara/archive"}   ;#data archive
export dumplist=${dumplist:-". .gfs."}            ;#file format pgb${asub}${fhr}${dump}${yyyymmdd}${cyc}
export complist=${complist:-"cirrus cirrus"}      ;#computers 

export fhlist=${fhlist:-"anl f00 f12 f24"}              
export cyc=${cycle:-00}                       ;#forecast cycle to verify
export cdate=${DATEST:-20120501}              ;#starting verifying date
export ndays=${ndays:-31}                     ;#number of days (cases)

export nlev=${nlev:-26}                       ;#pgb file vertical layers
export grid=${grid:-G2}                       ;#pgb file resolution, G2->2.5deg; G3->1deg; G4->0.5deg
export pbtm=${pbtm:-1000}                     ;#bottom pressure for zonal mean maps
export ptop=${ptop:-1}                        ;#top pressure for zonal mean maps


export obdata=${obdata:-/climate/save/wx24fy/obdata}
export webhost=${webhost:-emcrzdm.ncep.noaa.gov}
export webhostid=${webhostid:-$LOGNAME}
export ftpdir=${ftpdir:-/home/people/emc/www/htdocs/gmb/$webhostid/vsdb_glopara/test}
export doftp=${doftp:-NO}

export rundir=${rundir:-/ptmp/$LOGNAME/2dmaps/tline}
mkdir -p $rundir; cd $rundir || exit 8
rm -rf *
export mapdir=${mapdir:-$rundir/web}        ;#place where maps are saved locally
mkdir -p $mapdir 

export APRUN=${APRUN:-""}   ;#for running jobs on Gaea
export NWPROD=${NWPROD:-/nwprod}
export ndate=${ndate:-$NWPROD/util/exec/ndate}
export cpygb=${cpygb:-"$APRUN $NWPROD/util/exec/copygb"}
#------------------------------------------------------------------
#------------------------------------------------------------------
srcdir=${vsdbhome:-/global/save/wx24fy/VRFY/vsdb}/plot2d
gradsutil=${vsdbhome:-/global/save/wx24fy/VRFY/vsdb}/map_util/grads
export GRADSBIN=${GRADSBIN:-/usrx/local/grads/bin}


odir=0
#=============================
for fh in $fhlist ; do
odir=`expr $odir + 1 `
tmpdir=$rundir/d${odir}
mkdir $tmpdir ; cd $tmpdir || exit 8


#--operational GFS only saves data on 26 layers up to 10hPa
for exp in $expnlist ; do
if [ $exp = gfs ]; then
 export nlev=26
 export ptop=10
fi
done

#==================================================================
#-- create GrADS control files
export ctldir=${tmpdir}/ctl                                   
$srcdir/makectl.sh


#==================================================================
#-- make maps

mapair_layer=${mapair_layer:-yes}      ;#compare forecasts of upper air quantities (T, U, V, Z, Q, RH, O3, CLW etc), single layer
mapsfc=${mapsfc:-yes}                  ;#compare forecasts of all surface variables that are of interest

export fcsthr="$fh Fcst";
#--------------------------------------------------------------------------

set -A mlist none Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
year=`echo $cdate |cut -c 1-4`
mon=`echo $cdate |cut -c 5-6`
day=`echo $cdate |cut -c 7-8`
monc=${mlist[$mon]}
sdate=${day}${monc}${year}

nhours=`expr $(expr $ndays \* 24) - 24 `
cdate2=`echo $($ndate +$nhours ${cdate}00) |cut -c 1-8 `
year2=`echo $cdate2 |cut -c 1-4`
mon2=`echo $cdate2 |cut -c 5-6`
day2=`echo $cdate2 |cut -c 7-8`
monc2=${mlist[$mon2]}
edate=${day2}${monc2}${year2}
set -A sname $expnlist

#--define map range
area=${area:-gb}
latlon=${latlon:-"-90 90 0 360"}        ;#map area lat1, lat2, lon1 and lon2
set -A latlonc none $latlon
lat1=${latlonc[1]}; lat2=${latlonc[2]}
lon1=${latlonc[3]}; lon2=${latlonc[4]}

export nexp=`echo $expnlist |wc -w`                                  ;# number of experiments
 n=1; nn=0
 while [ $n -le $nexp ]; do
  export ctl${n}=${ctldir}/${sname[$nn]}/${sname[$nn]}_${fh}.ctl
  n=` expr $n + 1 `
  nn=` expr $nn + 1 `
 done


#=====================================================================
if [ $mapsfc = "yes" ]; then
#=====================================================================
#### ------------------------------------------------------------------------ 
###   compare surface variables among different runs
#### ----------------------------------------------------------------------

#--  instantaneous and some time averaged variables
for var in  \
 GFLUXsfc  LHTFLsfc  SHTFLsfc  \
 PRATEsfc CPRATsfc  UGWDsfc VGWDsfc \
 TCDCclm TCDChcl TCDCmcl TCDClcl TCDCbcl  \
 USWRFtoa USWRFsfc DSWRFsfc ULWRFsfc DLWRFsfc ULWRFtoa \
 PEVPRsfc PWATclm  CWATclm  \
 SOILW0_10cm  SOILW10_40cm  SOILW40_100cm  SOILW100_200cm  \
 TMP0_10cm  TMP10_40cm  TMP40_100cm  TMP100_200cm  \
 TMAX2m TMIN2m TMPsfc  TMP2m  RH2m  SPFH2m \
 WATRsfc WEASDsfc  SNODsfc TOZNEclm  \
 PRESsfc  PRMSLmsl HGTsfc  HPBLsfc HGTtrp   \
 UGRD10m VGRD10m APCPsfc ICECsfc ICETKsfc CINsfc CAPEsfc
do

if [ $var = "USWRFtoa" ]; then varname="TOA Up SW"        scal=1 ; fi
if [ $var = "USWRFsfc" ]; then varname="Sfc Up SW"        scal=1 ; fi
if [ $var = "DSWRFsfc" ]; then varname="Sfc Down SW"      scal=1 ; fi
if [ $var = "ULWRFsfc" ]; then varname="Sfc Up LW"        scal=1 ; fi
if [ $var = "DLWRFsfc" ]; then varname="Sfc Down LW"      scal=1 ; fi
if [ $var = "ULWRFtoa" ]; then varname="TOA Up LW"        scal=1 ; fi
if [ $var = "TCDCclm" ];  then varname="Total Cloud"      scal=1 ; fi
if [ $var = "TCDChcl" ];  then varname="High Cloud"       scal=1 ; fi
if [ $var = "TCDCmcl" ];  then varname="Middle Cloud"     scal=1 ; fi
if [ $var = "TCDClcl" ];  then varname="Low Cloud"        scal=1 ; fi
if [ $var = "TCDCbcl" ];  then varname="Boundary-Layer Cloud" scal=1 ; fi
if [ $var = "APCPsfc" ];  then varname="Total Precip"     scal=4 ; fi
if [ $var = "UGRD10m" ];  then varname="U10m [m/s]"          scal=1 ; fi
if [ $var = "VGRD10m" ];  then varname="V10m [m/s]"          scal=1 ; fi
if [ $var = "UGWDsfc" ];  then varname="sfc U_GWD [N/m2]"          scal=1 ; fi
if [ $var = "VGWDsfc" ];  then varname="sfc V_GWD [N/m2]"          scal=1 ; fi
if [ $var = "WATRsfc" ];  then varname="Water runoff [g/m2]"          scal=1000 ; fi
if [ $var = "WEASDsfc" ]; then varname="Accum. snow [Kg/m2]"         scal=1 ; fi
if [ $var = "SNODsfc" ];  then varname="Snow depth [m]"                scal=1 ; fi
if [ $var = "TOZNEclm" ]; then varname="Column O3 [Dobson]"            scal=1 ; fi
if [ $var = "TMAX2m" ];   then varname="Max T2m [K]"                   scal=1 ; fi
if [ $var = "TMIN2m" ];   then varname="MIN T2m [K]"                   scal=1 ; fi
if [ $var = "TMP2m" ];    then varname="T2m [K]"                       scal=1 ; fi
if [ $var = "TMPsfc" ];   then varname="Skin Temp [K]"                 scal=1 ; fi
if [ $var = "RH2m" ];     then varname="RH2m "                         scal=1 ; fi
if [ $var = "SPFH2m" ];   then varname="Q2m [g/kg]"                    scal=1000 ; fi
if [ $var = "GFLUXsfc" ]; then varname="Gnd Heat Flux [W/m2]"          scal=1 ; fi
if [ $var = "LHTFLsfc" ];  then varname="Latent Heat Flux [W/m2]"       scal=1 ; fi
if [ $var = "SHTFLsfc" ];  then varname="Sensible Heat Flux [W/m2]"     scal=1 ; fi
if [ $var = "PEVPRsfc" ];  then varname="Poten Evap [W/m2]"             scal=1 ; fi
if [ $var = "PWATclm" ];   then varname="Column Precip Water [kg/m2]"   scal=1 ; fi
if [ $var = "CWATclm" ];   then varname="Column Cloud Water [g/m2]"     scal=1000 ; fi
if [ $var = "PRATEsfc" ];  then varname="Precip Rate [mm/day]"          scal=24*3600 ; fi
if [ $var = "CRATsfc" ];   then varname="Conv. Precip Rate [mm/day]"    scal=24*3600 ; fi
if [ $var = "SOILW0_10cm" ];    then varname="Soil Moist 0-10cm"        scal=100 ; fi
if [ $var = "SOILW10_40cm" ];   then varname="Soil Moist 10-40cm"       scal=100 ; fi
if [ $var = "SOILW40_100cm" ];  then varname="Soil Moist 40-100cm"      scal=100 ; fi
if [ $var = "SOILW100_200cm" ]; then varname="Soil Moist 100-200cm"     scal=100 ; fi
if [ $var = "TMP0_10cm" ];      then varname="Soil Temp 0-10cm [K]"     scal=1 ; fi
if [ $var = "TMP10_40cm" ];     then varname="Soil Temp 10-40cm [K]"    scal=1 ; fi
if [ $var = "TMP40_100cm" ];    then varname="Soil Temp 40-100cm [K]"   scal=1 ; fi
if [ $var = "TMP100_200cm" ];   then varname="Soil Temp 100-200cm [K]"  scal=1 ; fi
if [ $var = "CNWATsfc" ];       then varname="Canopy Sfc Water [kg/m2]" scal=1 ; fi
if [ $var = "PRESsfc" ];        then varname="Sfc Pres [hPa]"           scal=0.01 ; fi
if [ $var = "PRMSLmsl" ];       then varname="Sea-Level Pres [hPa]"     scal=0.01 ; fi
if [ $var = "HGTsfc" ];         then varname="Sfc HGT [m]"              scal=1 ; fi
if [ $var = "HPBLsfc" ];        then varname="PBL HGT [m]"              scal=1 ; fi
if [ $var = "HGTtrp" ];         then varname="Tropopause HGT [m]"       scal=1 ; fi
if [ $var = "albedo" ];         then varname="Surface Albedo"           scal=1 ; fi
if [ $var = "ICECsfc" ];        then varname="Ice Concentration"        scal=1 ; fi
if [ $var = "ICETKsfc" ];       then varname="Ice Thickness [m]"        scal=1 ; fi
if [ $var = "CINsfc" ];         then varname="Sfc CIN  [J/kg]"          scal=1 ; fi
if [ $var = "CAPEsfc" ];        then varname="Sfc CAPE [J/kg]"          scal=1 ; fi

SOILTAG=NONE
SOILTAG=`echo $var |cut -c 1-4`
CLDTAG=NONE
CLDTAG=`echo $var |cut -c 1-4`
TMPTAG=NO
TMPTAG=`echo $var |cut -c 1-2`
RADTAG=NON
RADTAG=`echo $var |cut -c 3-5`
if [ $var = GFLUXsfc -o $var = LHTFLsfc -o $var = SHTFLsfc ]; then RADTAG=WRF;  fi

#.........................
cat >${var}.gs <<EOF1 
'reinit'; 'set font 1'
'set display color  white'
  'open $ctl1'
   mdc.1=${sname[0]}
if  ($nexp >1)
  'open $ctl2'
  mdc.2=${sname[1]}
endif     
if  ($nexp >2)
  'open $ctl3'
  mdc.3=${sname[2]}
endif     
if  ($nexp >3)
  'open $ctl4'
  mdc.4=${sname[3]}
endif     
if  ($nexp >4)
  'open $ctl5'
  mdc.5=${sname[4]}
endif     
if  ($nexp >5)
  'open $ctl6'
  mdc.6=${sname[5]}
endif     
if  ($nexp >6)
  'open $ctl7'
  mdc.7=${sname[6]}
endif     
if  ($nexp >7)
  'open $ctl8'
  mdc.8=${sname[7]}
endif     

*-----
* 'set lat $lat1 $lat2'
* 'set lon $lon1 $lon2 '
  'set lat $lat1 '
  'set lon $lon1 '
  'set t 1 $ndays  '


n=1
while ( n <= ${nexp} )
  'define sn'%n'=${scal}*aave((${var}.'%n', lon=$lon1,lon=$lon2,lat=$lat1,lat=$lat2)'
  n=n+1
endwhile


*-- define line styles and model names
  cst.1=1; cst.2=5; cst.3=3; cst.4=5; cst.5=5; cst.6=3; cst.7=5; cst.8=5; cst.9=5; cst.10=5
  cth.1=9; cth.2=9; cth.3=4; cth.4=1; cth.5=4; cth.6=1; cth.7=1; cth.8=1; cth.9=1; cth.10=1
  cma.1=0; cma.2=8; cma.3=6; cma.4=1; cma.5=2; cma.6=0; cma.7=3; cma.8=7; cma.9=4; cma.10=5
  cco.1=1; cco.2=2; cco.3=3; cco.4=4; cco.5=8; cco.6=9; cco.7=5; cco.8=6; cco.9=7; cco.10=15

*------------------------
*--find maximum and minmum values 
   cmax=-10000000.0; cmin=10000000.0
   i=1
   while (i <= $nexp)
    'set gxout stat'
    'd sn'%i
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    ln=sublin(result,7); wd=subwrd(ln,8); b=substr(wd,1,3)
    if( b>0 )
      if(zmax > cmax); cmax=zmax; endif
      if(zmin < cmin); cmin=zmin; endif
    endif
    i=i+1
   endwhile
   dist=cmax-cmin; cmin=cmin-1.7*dist; cmax=1.2*cmax
   cmin=substr(cmin,1,10); cmax=substr(cmax,1,10); cint=10*substr((cmax-cmin)/100,1,4)
   if (cint = 0); cint=substr((cmax-cmin)/10,1,4); endif
   if (cint = 0); cint=0.1*substr((cmax-cmin),1,4); endif
   if (cint = 0); cint=0.01*substr((cmax-cmin)*10,1,4); endif
   if (cint = 0); cint=0.001*substr((cmax-cmin)*100,1,4); endif
   if (cint = 0); cint=0.0001*substr((cmax-cmin)*1000,1,4); endif
   if (cint = 0); cint=0.00001*substr((cmax-cmin)*10000,1,4); endif
   say 'cmin cmax cint 'cmin' 'cmax' 'cint

*------------------------
  xwd=7.0; ywd=4.0; yy=ywd/17
  xmin=1.0; xmax=xmin+xwd; ymin=6.0; ymax=ymin+ywd
  xt=xmin+0.3; xt1=xt+0.5; xt2=xt1+0.1; yt=ymin+0.36*ywd ;*for legend
  zxt=xt2+3.0; zxt1=zxt+0.5; zxt2=zxt1+0.1; zyt=ymin+0.36*ywd
  titlx=xmin+0.5*xwd;  titly=ymax+0.35                  ;*for tytle
  titlx2=xmin+0.5*xwd;  titly2=ymax+0.10                  ;*for tytle
  xlabx=xmin+0.5*xwd;  xlaby=ymin-0.60
  'set parea 'xmin' 'xmax' 'ymin' 'ymax
*------------------------

   i=1
   while (i <= $nexp)
    'set gxout stat'  ;* first compute means and count good data numbers
    'd sn'%i
    ln=sublin(result,11); wd=subwrd(ln,2); a=substr(wd,1,5)
    ln=sublin(result,7); wd=subwrd(ln,8); b=substr(wd,1,3)
    if ( b>0 )
      if( i <=5);
        'set strsiz 0.13 0.13'; yt=yt-yy
        'set line 'cco.i' 'cst.i' 11'; 'draw line 'xt' 'yt' 'xt1' 'yt
        'set string 'cco.i' bl 6';     'draw string 'xt2' 'yt' 'mdc.i'  ' a'  'b
      else
        'set strsiz 0.13 0.13'; zyt=zyt-yy
        'set line 'cco.i' 'cst.i' 11'; 'draw line 'zxt' 'zyt' 'zxt1' 'zyt
        'set string 'cco.i' bl 6';     'draw string 'zxt2' 'zyt' 'mdc.i'  ' a'  'b
      endif

      'set gxout line'
      'set display color white'; 'set missconn on';     'set grads off'; 'set grid on'
      'set xlopts 1 6 0.14';     'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
      'set vrange 'cmin' 'cmax; 'set ylint 'cint
*     'set xlint $xtick'
      'set cstyle 'cst.i; 'set cthick 'cth.i; 'set cmark 'cma.i; 'set ccolor 'cco.i
      'd sn'%i
     endif
   i=i+1
   endwhile

  'set string 1 bc 7'
  'set strsiz 0.14 0.14'
  'draw string 'titlx' 'titly' ${varname}, $fcsthr'
  'set strsiz 0.14 0.14'
  'draw string 'titlx2' 'titly2' area mean [${lat1}-${lat2}; ${lon1}-${lon2}]' 
* 'set strsiz 0.15 0.15'
* 'draw string 'xlabx' 'xlaby' Verification Date'

  'printim tl_${var}_${area}.gif gif x700 y700'
  'set vpage off'
'quit'
EOF1
$GRADSBIN/grads -bcp "run ${var}.gs" 

done
#.........................

if [ $doftp = YES ]; then
cat << EOF >ftpin
  binary
  prompt
  cd $ftpdir/2D/d$odir/
  mput *.gif
  quit
EOF
sftp  ${webhostid}@${webhost} <ftpin
 if [ $? -ne 0 ]; then
  scp -rp *.gif ${webhostid}@${webhost}:$ftpdir/2D/d$odir/.
 fi
fi
cp *.gif $mapdir/2D/d$odir/.
#=====================================================================
fi  ;# end of mapsfc 
#=====================================================================




#=====================================================================
if [ $mapair_layer = "yes" ]; then
#=====================================================================

#### ---------------------------------------------------------- 
###  generate 2D maps on specific layers of the air. No obs 
#### ---------------------------------------------------------- 

#for var in  TMPprs CLWMRprs HGTprs O3MRprs RHprs SPFHprs UGRDprs VGRDprs VVELprs ;do
for var in  TMPprs CLWMRprs HGTprs O3MRprs RHprs  UGRDprs VGRDprs VVELprs SPFHprs ;do
for lev in 1000 850 700 500 200 100 70 50 10 5 1; do

if [ $var = "TMPprs" ];   then varname="Temp (K)"           scal=1            ; fi
if [ $var = "ABSVprs" ];  then varname="Vorticity"          scal=1000         ; fi
if [ $var = "CLWMRprs" ]; then varname="Cloud Water (ppmg)" scal=1000000      ; fi
if [ $var = "HGTprs" ];   then varname="HGT (m)"            scal=1            ; fi
if [ $var = "O3MRprs" ];  then varname="O3 (ppmg)"          scal=1000000      ; fi
if [ $var = "RHprs" ];    then varname="RH "                scal=1            ; fi
if [ $var = "SPFHprs" ];  then varname="Q (1E-6 kg/kg)"     scal=1000000      ; fi
if [ $var = "UGRDprs" ];  then varname="U (m/s)"            scal=1            ; fi
if [ $var = "VGRDprs" ];  then varname="V (m/s)"            scal=1            ; fi
if [ $var = "VVELprs" ];  then varname="W (mb/hr)"          scal=24*36        ; fi

#.........................
cat >${var}${lev}.gs <<EOF1 
'reinit'; 'set font 1'
'set display color  white'
  'open $ctl1'
   mdc.1=${sname[0]}
if  ($nexp >1)
  'open $ctl2'
  mdc.2=${sname[1]}
endif
if  ($nexp >2)
  'open $ctl3'
  mdc.3=${sname[2]}
endif
if  ($nexp >3)
  'open $ctl4'
  mdc.4=${sname[3]}
endif
if  ($nexp >4)
  'open $ctl5'
  mdc.5=${sname[4]}
endif
if  ($nexp >5)
  'open $ctl6'
  mdc.6=${sname[5]}
endif
if  ($nexp >6)
  'open $ctl7'
  mdc.7=${sname[6]}
endif
if  ($nexp >7)
  'open $ctl8'
  mdc.8=${sname[7]}
endif

*-----
* 'set lat $lat1 $lat2'
* 'set lon $lon1 $lon2'
  'set lat $lat1 '
  'set lon $lon1 '
  'set lev $lev'
  'set t 1 $ndays  '


n=1
while ( n <= ${nexp} )
  'define sn'%n'=${scal}*aave((${var}.'%n', lon=$lon1,lon=$lon2,lat=$lat1,lat=$lat2)'
  n=n+1
endwhile


*-- define line styles and model names
  cst.1=1; cst.2=5; cst.3=3; cst.4=5; cst.5=5; cst.6=3; cst.7=5; cst.8=5; cst.9=5; cst.10=5
  cth.1=9; cth.2=9; cth.3=4; cth.4=1; cth.5=4; cth.6=1; cth.7=1; cth.8=1; cth.9=1; cth.10=1
  cma.1=0; cma.2=8; cma.3=6; cma.4=1; cma.5=2; cma.6=0; cma.7=3; cma.8=7; cma.9=4; cma.10=5
  cco.1=1; cco.2=2; cco.3=3; cco.4=4; cco.5=8; cco.6=9; cco.7=5; cco.8=6; cco.9=7; cco.10=15

*------------------------
*--find maximum and minmum values 
   cmax=-10000000.0; cmin=10000000.0
   i=1
   while (i <= $nexp)
    'set gxout stat'
    'd sn'%i
    range=sublin(result,9); zmin=subwrd(range,5); zmax=subwrd(range,6)
    ln=sublin(result,7); wd=subwrd(ln,8); b=substr(wd,1,3)
    if( b>0 )
      if(zmax > cmax); cmax=zmax; endif
      if(zmin < cmin); cmin=zmin; endif
    endif
   i=i+1
   endwhile
   dist=cmax-cmin; cmin=cmin-1.7*dist; cmax=1.2*cmax
   cmin=substr(cmin,1,10); cmax=substr(cmax,1,10); cint=10*substr((cmax-cmin)/100,1,4)
   if (cint = 0); cint=substr((cmax-cmin)/10,1,4); endif
   if (cint = 0); cint=0.1*substr((cmax-cmin),1,4); endif
   if (cint = 0); cint=0.01*substr((cmax-cmin)*10,1,4); endif
   if (cint = 0); cint=0.001*substr((cmax-cmin)*100,1,4); endif
   if (cint = 0); cint=0.0001*substr((cmax-cmin)*1000,1,4); endif
   if (cint = 0); cint=0.00001*substr((cmax-cmin)*10000,1,4); endif
   say 'cmin cmax cint 'cmin' 'cmax' 'cint

*------------------------
  xwd=7.0; ywd=4.0; yy=ywd/17
  xmin=1.0; xmax=xmin+xwd; ymin=6.0; ymax=ymin+ywd
  xt=xmin+0.3; xt1=xt+0.5; xt2=xt1+0.1; yt=ymin+0.36*ywd ;*for legend
  zxt=xt2+3.0; zxt1=zxt+0.5; zxt2=zxt1+0.1; zyt=ymin+0.36*ywd
  titlx=xmin+0.5*xwd;  titly=ymax+0.35                  ;*for tytle
  titlx2=xmin+0.5*xwd;  titly2=ymax+0.10                  ;*for tytle
  xlabx=xmin+0.5*xwd;  xlaby=ymin-0.60
  'set parea 'xmin' 'xmax' 'ymin' 'ymax
*------------------------

   i=1
   while (i <= $nexp)
    'set gxout stat'  ;* first compute means and count good data numbers
    'd sn'%i
    ln=sublin(result,11); wd=subwrd(ln,2); a=substr(wd,1,5)
    ln=sublin(result,7); wd=subwrd(ln,8); b=substr(wd,1,3)
    if ( b>0 )
      if( i <=5);
        'set strsiz 0.13 0.13'; yt=yt-yy
        'set line 'cco.i' 'cst.i' 11'; 'draw line 'xt' 'yt' 'xt1' 'yt
        'set string 'cco.i' bl 6';     'draw string 'xt2' 'yt' 'mdc.i'  ' a'  'b
      else
        'set strsiz 0.13 0.13'; zyt=zyt-yy
        'set line 'cco.i' 'cst.i' 11'; 'draw line 'zxt' 'zyt' 'zxt1' 'zyt
        'set string 'cco.i' bl 6';     'draw string 'zxt2' 'zyt' 'mdc.i'  ' a'  'b
      endif

      'set gxout line'
      'set display color white'; 'set missconn on';     'set grads off'; 'set grid on'
      'set xlopts 1 6 0.14';     'set ylopts 1 6 0.14'; 'set clopts 1 6 0.0'
      'set vrange 'cmin' 'cmax; 'set ylint 'cint
*     'set xlint $xtick'
      'set cstyle 'cst.i; 'set cthick 'cth.i; 'set cmark 'cma.i; 'set ccolor 'cco.i
      'd sn'%i
     endif
   i=i+1
   endwhile

  'set string 1 bc 7'
  'set strsiz 0.14 0.14'
  'draw string 'titlx' 'titly' ${lev}hPa ${varname}, $fcsthr '
  'set strsiz 0.14 0.14'
  'draw string 'titlx2' 'titly2' area mean [${lat1}-${lat2}; ${lon1}-${lon2}]' 
* 'set strsiz 0.15 0.15'
* 'draw string 'xlabx' 'xlaby' Verification Date'

  'printim tl_${var}${lev}_${area}.gif gif x700 y700'
  'set vpage off'
'quit'
EOF1
$GRADSBIN/grads -bcp "run ${var}${lev}.gs"  
sleep 2 

done
done
#.........................
sleep 30

#--wait for maps to be made
#nsleep=0
#tsleep=30      #seconds to sleep before checking file again
#msleep=40      #maximum number of times to sleep
#while test ! -s $tmpdir/UGRDprs100.gif -a $nsleep -lt $msleep;do
#  sleep $tsleep
#  nsleep=`expr $nsleep + 1`
#done


if [ $doftp = YES ]; then
cat << EOF >ftpin
  binary
  prompt
  cd $ftpdir/2D/d$odir
  mput *.gif
  quit
EOF
sftp  ${webhostid}@${webhost} <ftpin
 if [ $? -ne 0 ]; then
  scp -rp *.gif ${webhostid}@${webhost}:$ftpdir/2D/d$odir/.
 fi
fi
cp *.gif $mapdir/2D/d$odir/.
#=====================================================================
fi    ;# end of mapair_layer 
#=====================================================================


done  ;#end of fcst_day
#=======================
exit
