#!/bin/ksh
set -x
curdir=`pwd`

export expnlist=${expnlist:-"gfs prsl19"}             ;#experiment names
export expdlist=${expdlist:-"/global/hires/glopara/archive /global/hires/glopara/archive"}  
export dumplist=${dumplist:-".gfs. .gfs."}         ;#file format pgb${asub}${fhr}${dump}${yyyymmdd}${cyc}
export complist=${complist:-"cirrus cirrus"}       ;#computers where experiments are run

export fcst_day=${fcst_day:-5}                ;#forecast day to verify
export cyc=${cyc:-00}                         ;#forecast cycle to verify
export cdate=${cdate:-20100701}               ;#starting verifying date
export ndays=${ndays:-10}                     ;#number of days (cases)
export nlev=${nlev:-47}                       ;#pgb file vertical layers
export grid=${grid:-G2}                       ;#pgb file resolution, G2->2.5deg; G3->1deg; G4->0.5deg

export vsdbhome=${vsdbhome:-/global/save/wx24fy/VRFY/vsdb}
export gstat=${gstat:-/global/shared/stat}     
export APRUN=${APRUN:-""}   ;#for running jobs on Gaea
export machine=${machine:-WCOSS}     
export NWPROD=${NWPROD:-/nwprod}     
export ndate=${ndate:-$NWPROD/util/exec/ndate}
export cpygb=${cpygb:-"$APRUN $NWPROD/util/exec/copygb"}
#---------------------------------------------------------------------------------
if [ "$fhlist" = '' ] ; then
fhr4=`expr $fcst_day \* 24 `
fhr3=`expr $fhr4 - 6  `
fhr2=`expr $fhr4 - 12  `
fhr1=`expr $fhr4 - 18  `
if [ $fhr1 -lt 10 ]; then fhr1=0$fhr1; fi
export fhlist="f$fhr1 f$fhr2 f$fhr3 f$fhr4"    ;#fcst hours to be analyzed
fi


if [ $nlev = 26 ]; then
 levlist="1000 975 950 925 900 850 800 750 700 650 600 550 500 450 400 350 300 250 200 150 100 70 50 30 20 10"
elif [ $nlev = 31 ]; then
 levlist="1000 975 950 925 900 850 800 750 700 650 600 550 500 450 400 350 300 250 200 150 100 70 50 30 20 10 7 5 3 2 1"
elif [ $nlev = 37 ]; then
 levlist="1000 975 950 925 900 875 850 825 800 775 750 700 650 600 550 500 450 400 350 300 250 225 200 175 150 125 100 70 50 30 20 10 7 5 3 2 1"
elif [ $nlev = 47 ]; then
 levlist="1000 975 950 925 900 875 850 825 800 775 750 725 700 675 650 625 600 575 550 525 500 475 450 425 400 375 350 325 300 275 250 225 200 175 150 125 100 70 50 30 20 10 7 5 3 2 1"
else
 nlev=26
 levlist="1000 975 950 925 900 850 800 750 700 650 600 550 500 450 400 350 300 250 200 150 100 70 50 30 20 10"
# echo " pgb file vertical layers $nlev not supported, exit"
# exit 
fi

##--special case for ESRL FIM and ECMWF 
for exp in $expnlist; do
 if [ $exp = fim ] ; then 
  nlev=24
  levlist="1000 975 950 925 900 850 800 750 700 650 600 550 500 450 400 350 300 250 200 150 100 50 20 10" 
 fi 
done
for exp in $expnlist; do
 if [ $exp = ecm ] ; then 
  nlev=14
  levlist="1000 925 850 700 500 400 300 250 200 150 100 50 20 10"
 fi 
done

export rundir=${rundir:-/ptmp/$LOGNAME/2dmaps} 
export ctldir0=${ctldir:-$rundir/ctl}
mkdir -p $rundir $ctldir0

set -A expdname $expdlist
set -A compname $complist
set -A dumpname $dumplist
cd ${rundir} || exit 8

#------------------------------
n=0
for exp in $expnlist; do
#------------------------------

 if [ $grid = G2 ]; then
  nptx=144; npty=73; dxy=2.5; gribtype=2
 elif [ $grid = G3 ]; then
  nptx=360; npty=181; dxy=1.0; gribtype=3
 elif [ $grid = G4 ]; then
  nptx=720; npty=361; dxy=0.5; gribtype=4
 else
  echo " pgb file grid $grid not supported, exit"
  exit 
 fi
# if [ $exp = ecm ]; then gribtype=255 ;fi

export dump=${dumpname[n]}
expdir=${expdname[n]}
CLIENT=${compname[n]}
myhost=`echo $(hostname) |cut -c 1-1 `
myclient=`echo $CLIENT |cut -c 1-1 `

set -A mlist none Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
year=`echo $cdate |cut -c 1-4`
mon=`echo $cdate |cut -c 5-6`
day=`echo $cdate |cut -c 7-8`
monc=${mlist[$mon]}

export datadir=$rundir/$exp/d${fcst_day}                  
export ctldir=$ctldir0/$exp
#if [ -s $datadir ]; then rm -rf $datadir ;fi
#if [ -s $ctldir ]; then rm -rf $ctldir ;fi
mkdir -p $ctldir $datadir

#-- pgb files contain both instantaneous and time-mean radiative 
#-- fluxes and precip rate. grib2ctl is not able to distinguish 
#-- these two types of fields.  Fields with attributes TR=3 
#-- are then extracted from the original pgb files and written in 
#-- a temporary directory.
#
TR=3
GG=`echo $grid |cut -c 2-2 `
nhours=`expr $ndays \* 24 `
sdate=${cdate}${cyc}
edate=`$ndate +$nhours $sdate`
while [ $sdate -le $edate ]; do
for hr in $fhlist; do
 rm -f xtmp
 if [ $myhost = $myclient -o $machine != WCOSS ]; then
   if [ $exp = "gfs" ]; then
    if [ -s $gstat/gfs/pgb${hr}.$sdate ]; then
     ln -fs $gstat/gfs/pgb${hr}.$sdate ${datadir}/xtmp
    else
     ln -fs $gstat/gfs/pgb${hr}.$exp.$sdate ${datadir}/xtmp
    fi
   else
    ln -fs ${expdir}/${exp}/pgb${hr}$dump$sdate  ${datadir}/xtmp
   fi
 elif [ $machine = WCOSS ]; then
   if [ $exp = "gfs" ]; then
    scp -pB ${LOGNAME}@${CLIENT}:$gstat/gfs/pgb${hr}.$sdate ${datadir}/xtmp
   else
    scp -pB ${LOGNAME}@${CLIENT}:${expdir}/${exp}/pgb${hr}$dump$sdate ${datadir}/xtmp
   fi
 fi
  ${cpygb} -g$GG -x  ${datadir}/xtmp ${datadir}/pgb${hr}$dump$sdate
  $vsdbhome/bin/grbtr $TR ${datadir}/pgb${hr}$dump$sdate ${datadir}/pgb${hr}m$dump$sdate           
done
 sdate=`$ndate +24 $sdate`
done


#--------------------
for hr in $fhlist; do
#--------------------

rm $ctldir/${exp}_${hr}.ctl $ctldir/${exp}.t${cyc}z.pgrb${hr}.idx
#-----------------------------------
cat >$ctldir/${exp}_$hr.ctl <<EOF
dset ${datadir}/pgb${hr}${dump}%y4%m2%d2${cyc}
index $ctldir/${exp}.t${cyc}z.pgrb${hr}.idx
undef 9.999E+20
title ${exp}.t${cyc}z.pgrb${hr}
*  produced by grib2ctl v0.9.12.5p39a
dtype grib $gribtype
format template
options yrev
xdef $nptx linear 0.000000 $dxy     
ydef $npty linear -90.000000 $dxy 
tdef $ndays linear ${cyc}Z${day}${monc}${year} 1dy
zdef $nlev levels ${levlist}
vars 250
no4LFTXsfc  0 132,1,0  ** surface Best (4-layer) lifted index [K]
no5WAVA500mb  0 230,100,500 ** 500 mb 5-wave geopot. height anomaly [gpm]
no5WAVH500mb  0 222,100,500 ** 500 mb 5-wave geopotential height [gpm]
ABSVprs $nlev 41,100,0 ** (profile) Absolute vorticity [/s]
ACPCPsfc  0 63,1,0  ** surface Convective precipitation [kg/m^2]
ALBDOsfc  0 84,1,0  ** surface Albedo [%]
APCPsfc  0 61,1,0  ** surface Total precipitation [kg/m^2]
BRTMPtoa  0 118,8,0 ** top of atmos Brightness temperature [K]
CAPEsfc  0 157,1,0  ** surface Convective Avail. Pot. Energy [J/kg]
CAPE180_0mb  0 157,116,46080 ** 180-0 mb above gnd Convective Avail. Pot. Energy [J/kg]
CDUVBsfc  0 201,1,0  ** surface Clear Sky UV-B Downward Solar Flux [W/m^2]
CFRZRsfc  0 141,1,0  ** surface Categorical freezing rain [yes=1;no=0]
CICEPsfc  0 142,1,0  ** surface Categorical ice pellets [yes=1;no=0]
CINsfc  0 156,1,0  ** surface Convective inhibition [J/kg]
CIN180_0mb  0 156,116,46080 ** 180-0 mb above gnd Convective inhibition [J/kg]
CLWMRprs $nlev 153,100,0 ** (profile) Cloud water [kg/kg]
CNWATsfc  0 223,1,0  ** surface Plant canopy surface water [kg/m^2]
CPRATsfc  0 214,1,0  ** surface Convective precip. rate [kg/m^2/s]
CRAINsfc  0 140,1,0  ** surface Categorical rain [yes=1;no=0]
CSNOWsfc  0 143,1,0  ** surface Categorical snow [yes=1;no=0]
CWATclm  0 76,200,0 ** atmos column Cloud water [kg/m^2]
CWORKclm  0 146,200,0 ** atmos column Cloud work function [J/kg]
DLWRFsfc  0 205,1,0  ** surface Downward long wave flux [W/m^2]
DPT2m  0 17,105,2 ** 2 m above ground Dew point temp. [K]
DPT30_0mb  0 17,116,7680 ** 30-0 mb above gnd Dew point temp. [K]
DSWRFsfc  0 204,1,0  ** surface Downward short wave flux [W/m^2]
DUVBsfc  0 200,1,0  ** surface UV-B Downward Solar Flux [W/m^2]
FLDCPsfc  0 220,1,0  ** surface Field Capacity [fraction]
FRICVsfc  0 253,1,0  ** surface Friction velocity [m/s]
GFLUXsfc  0 155,1,0  ** surface Ground heat flux [W/m^2]
GPAprs 2 27,100,0 ** (profile) Geopotential height anomaly [gpm]
GUSTsfc  0 180,1,0  ** surface Surface wind gust [m/s]
HGTsfc  0 7,1,0  ** surface Geopotential height [gpm]
HGTprs $nlev 7,100,0 ** (profile) Geopotential height [gpm]
HGTpv500  0 7,117,500 ** pot vorticity = 500 units level Geopotential height [gpm]
HGTpv1000  0 7,117,1000 ** pot vorticity = 1000 units level Geopotential height [gpm]
HGTpv1500  0 7,117,1500 ** pot vorticity = 1500 units level Geopotential height [gpm]
HGTpv2000  0 7,117,2000 ** pot vorticity = 2000 units level Geopotential height [gpm]
HGTpvneg500  0 7,117,33268 ** pot vorticity = -500 units level Geopotential height [gpm]
HGTpvneg1000  0 7,117,33768 ** pot vorticity = -1000 units level Geopotential height [gpm]
HGTpvneg1500  0 7,117,34268 ** pot vorticity = -1500 units level Geopotential height [gpm]
HGTpvneg2000  0 7,117,34768 ** pot vorticity = -2000 units level Geopotential height [gpm]
HGThtfl  0 7,204,0 ** highest trop freezing level Geopotential height [gpm]
HGT0deg  0 7,4,0 ** 0C isotherm level Geopotential height [gpm]
HGTmwl  0 7,6,0 ** max wind level Geopotential height [gpm]
HGTtrp  0 7,7,0 ** tropopause Geopotential height [gpm]
HLCY0_1000m   0 190,106,2560 ** 0-1000 m above ground Storm relative helicity [m^2/s^2]
HLCY0_3000m   0 190,106,7680 ** 0-3000 m above ground Storm relative helicity [m^2/s^2]
HPBLsfc  0 221,1,0  ** surface Planetary boundary layer height [m]
ICAHTmwl  0 5,6,0 ** max wind level ICAO Standard Atmosphere Reference Height [M]
ICAHTtrp  0 5,7,0 ** tropopause ICAO Standard Atmosphere Reference Height [M]
ICECsfc  0 91,1,0  ** surface Ice concentration (ice=1;no ice=0) [fraction]
ICETKsfc  0 92,1,0  ** surface Ice thickness [m]
LANDsfc  0 81,1,0  ** surface Land cover (land=1;sea=0) [fraction]
LFTXsfc  0 131,1,0  ** surface Surface lifted index [K]
LHTFLsfc  0 121,1,0  ** surface Latent heat flux [W/m^2]
MNTSF320K  0 37,113,320 ** 320K level Montgomery stream function [m^2/s^2]
NCPCPsfc  0 62,1,0  ** surface Large scale precipitation [kg/m^2]
O3MRprs $nlev 154,100,0 ** (profile) Ozone mixing ratio [kg/kg]
PEVPRsfc  0 145,1,0  ** surface Potential evaporation rate [W/m^2]
PLI30_0mb  0 24,116,7680 ** 30-0 mb above gnd Parcel lifted index (to 500 hPa) [K]
POTsig995   0 13,107,9950 ** sigma=.995  Potential temp. [K]
PRATEsfc  0 59,1,0  ** surface Precipitation rate [kg/m^2/s]
PRESsfc  0 1,1,0  ** surface Pressure [Pa]
PRESmsl  0 1,102,0 ** mean-sea level Pressure [Pa]
PRESpv500  0 1,117,500 ** pot vorticity = 500 units level Pressure [Pa]
PRESpv1000  0 1,117,1000 ** pot vorticity = 1000 units level Pressure [Pa]
PRESpv1500  0 1,117,1500 ** pot vorticity = 1500 units level Pressure [Pa]
PRESpv2000  0 1,117,2000 ** pot vorticity = 2000 units level Pressure [Pa]
PRESpvneg500  0 1,117,33268 ** pot vorticity = -500 units level Pressure [Pa]
PRESpvneg1000  0 1,117,33768 ** pot vorticity = -1000 units level Pressure [Pa]
PRESpvneg1500  0 1,117,34268 ** pot vorticity = -1500 units level Pressure [Pa]
PRESpvneg2000  0 1,117,34768 ** pot vorticity = -2000 units level Pressure [Pa]
PRESlcb  0 1,212,0 ** low cloud base Pressure [Pa]
PRESlct  0 1,213,0 ** low cloud top Pressure [Pa]
PRESmcb  0 1,222,0 ** mid-cloud base Pressure [Pa]
PRESmct  0 1,223,0 ** mid-cloud top Pressure [Pa]
PREShcb  0 1,232,0 ** high cloud base Pressure [Pa]
PREShct  0 1,233,0 ** high cloud top Pressure [Pa]
PREScvb  0 1,242,0 ** convective cld base Pressure [Pa]
PREScvt  0 1,243,0 ** convective cld top Pressure [Pa]
PRESmwl  0 1,6,0 ** max wind level Pressure [Pa]
PREStrp  0 1,7,0 ** tropopause Pressure [Pa]
PRMSLmsl  0 2,102,0 ** mean-sea level Pressure reduced to MSL [Pa]
PVORT320K  0 4,113,320 ** 320K level Pot. vorticity [km^2/kg/s]
PWAT30_0mb  0 54,116,7680 ** 30-0 mb above gnd Precipitable water [kg/m^2]
PWATclm  0 54,200,0 ** atmos column Precipitable water [kg/m^2]
RHprs $nlev 52,100,0 ** (profile) Relative humidity [%]
RH2m  0 52,105,2 ** 2 m above ground Relative humidity [%]
RHsig995   0 52,107,9950 ** sigma=.995  Relative humidity [%]
RHsg33_100   0 52,108,8548 ** sigma=0.33-1 layer Relative humidity [%]
RHsg44_72   0 52,108,11336 ** sigma=0.44-0.72 layer Relative humidity [%]
RHsg44_100   0 52,108,11364 ** sigma=0.44-1 layer Relative humidity [%]
RHsg72_94   0 52,108,18526 ** sigma=0.72-0.94 layer Relative humidity [%]
RH30_0mb  0 52,116,7680 ** 30-0 mb above gnd Relative humidity [%]
RH60_30mb  0 52,116,15390 ** 60-30 mb above gnd Relative humidity [%]
RH90_60mb  0 52,116,23100 ** 90-60 mb above gnd Relative humidity [%]
RH120_90mb  0 52,116,30810 ** 120-90 mb above gnd Relative humidity [%]
RH150_120mb  0 52,116,38520 ** 150-120 mb above gnd Relative humidity [%]
RH180_150mb  0 52,116,46230 ** 180-150 mb above gnd Relative humidity [%]
RHclm  0 52,200,0 ** atmos column Relative humidity [%]
RHhtfl  0 52,204,0 ** highest trop freezing level Relative humidity [%]
RH0deg  0 52,4,0 ** 0C isotherm level Relative humidity [%]
SFCRsfc  0 83,1,0  ** surface Surface roughness [m]
SHTFLsfc  0 122,1,0  ** surface Sensible heat flux [W/m^2]
SNODsfc  0 66,1,0  ** surface Snow depth [m]
SOILL0_10cm  0 160,112,10 ** 0-10 cm underground Liquid volumetric soil moisture (non-frozen) [fraction]
SOILL10_40cm  0 160,112,2600 ** 10-40 cm underground Liquid volumetric soil moisture (non-frozen) [fraction]
SOILL40_100cm  0 160,112,10340 ** 40-100 cm underground Liquid volumetric soil moisture (non-frozen) [fraction]
SOILL100_200cm  0 160,112,25800 ** 100-200 cm underground Liquid volumetric soil moisture (non-frozen) [fraction]
SOILW0_10cm  0 144,112,10 ** 0-10 cm underground Volumetric soil moisture [fraction]
SOILW10_40cm  0 144,112,2600 ** 10-40 cm underground Volumetric soil moisture [fraction]
SOILW40_100cm  0 144,112,10340 ** 40-100 cm underground Volumetric soil moisture [fraction]
SOILW100_200cm  0 144,112,25800 ** 100-200 cm underground Volumetric soil moisture [fraction]
SPFHprs $nlev 51,100,0 ** (profile) Specific humidity [kg/kg]
SPFH2m  0 51,105,2 ** 2 m above ground Specific humidity [kg/kg]
SPFH30_0mb  0 51,116,7680 ** 30-0 mb above gnd Specific humidity [kg/kg]
SPFH60_30mb  0 51,116,15390 ** 60-30 mb above gnd Specific humidity [kg/kg]
SPFH90_60mb  0 51,116,23100 ** 90-60 mb above gnd Specific humidity [kg/kg]
SPFH120_90mb  0 51,116,30810 ** 120-90 mb above gnd Specific humidity [kg/kg]
SPFH150_120mb  0 51,116,38520 ** 150-120 mb above gnd Specific humidity [kg/kg]
SPFH180_150mb  0 51,116,46230 ** 180-150 mb above gnd Specific humidity [kg/kg]
TCDC475mb  0 71,100,475 ** 475 mb Total cloud cover [%]
TCDCclm  0 71,200,0 ** atmos column Total cloud cover [%]
TCDCbcl  0 71,211,0 ** boundary cld layer Total cloud cover [%]
TCDClcl  0 71,214,0 ** low cloud level Total cloud cover [%]
TCDCmcl  0 71,224,0 ** mid-cloud level Total cloud cover [%]
TCDChcl  0 71,234,0 ** high cloud level Total cloud cover [%]
TCDCcvl  0 71,244,0 ** convective cld layer Total cloud cover [%]
TMAX2m  0 15,105,2 ** 2 m above ground Max. temp. [K]
TMIN2m  0 16,105,2 ** 2 m above ground Min. temp. [K]
TMPsfc  0 11,1,0  ** surface Temp. [K]
TMPprs $nlev 11,100,0 ** (profile) Temp. [K]
TMP4572m  0 11,103,4572 ** 4572 m above msl Temp. [K]
TMP3658m  0 11,103,3658 ** 3658 m above msl Temp. [K]
TMP2743m  0 11,103,2743 ** 2743 m above msl Temp. [K]
TMP1829m  0 11,103,1829 ** 1829 m above msl Temp. [K]
TMP914m  0 11,103,914 ** 914 m above msl Temp. [K]
TMP610m  0 11,103,610 ** 610 m above msl Temp. [K]
TMP457m  0 11,103,457 ** 457 m above msl Temp. [K]
TMP305m  0 11,103,305 ** 305 m above msl Temp. [K]
TMP2m  0 11,105,2 ** 2 m above ground Temp. [K]
TMPsig995   0 11,107,9950 ** sigma=.995  Temp. [K]
TMP0_10cm  0 11,112,10 ** 0-10 cm underground Temp. [K]
TMP10_40cm  0 11,112,2600 ** 10-40 cm underground Temp. [K]
TMP40_100cm  0 11,112,10340 ** 40-100 cm underground Temp. [K]
TMP100_200cm  0 11,112,25800 ** 100-200 cm underground Temp. [K]
TMP320K  0 11,113,320 ** 320K level Temp. [K]
TMP30_0mb  0 11,116,7680 ** 30-0 mb above gnd Temp. [K]
TMP60_30mb  0 11,116,15390 ** 60-30 mb above gnd Temp. [K]
TMP90_60mb  0 11,116,23100 ** 90-60 mb above gnd Temp. [K]
TMP120_90mb  0 11,116,30810 ** 120-90 mb above gnd Temp. [K]
TMP150_120mb  0 11,116,38520 ** 150-120 mb above gnd Temp. [K]
TMP180_150mb  0 11,116,46230 ** 180-150 mb above gnd Temp. [K]
TMPpv500  0 11,117,500 ** pot vorticity = 500 units level Temp. [K]
TMPpv1000  0 11,117,1000 ** pot vorticity = 1000 units level Temp. [K]
TMPpv1500  0 11,117,1500 ** pot vorticity = 1500 units level Temp. [K]
TMPpv2000  0 11,117,2000 ** pot vorticity = 2000 units level Temp. [K]
TMPpvneg500  0 11,117,33268 ** pot vorticity = -500 units level Temp. [K]
TMPpvneg1000  0 11,117,33768 ** pot vorticity = -1000 units level Temp. [K]
TMPpvneg1500  0 11,117,34268 ** pot vorticity = -1500 units level Temp. [K]
TMPpvneg2000  0 11,117,34768 ** pot vorticity = -2000 units level Temp. [K]
TMPlct  0 11,213,0 ** low cloud top Temp. [K]
TMPmct  0 11,223,0 ** mid-cloud top Temp. [K]
TMPhct  0 11,233,0 ** high cloud top Temp. [K]
TMPmwl  0 11,6,0 ** max wind level Temp. [K]
TMPtrp  0 11,7,0 ** tropopause Temp. [K]
TOZNEclm  0 10,200,0 ** atmos column Total ozone [Dobson]
UGWDsfc  0 147,1,0  ** surface Zonal gravity wave stress [N/m^2]
UFLXsfc  0 124,1,0  ** surface Zonal momentum flux [N/m^2]
UGRDprs $nlev 33,100,0 ** (profile) u wind [m/s]
UGRD4572m  0 33,103,4572 ** 4572 m above msl u wind [m/s]
UGRD3658m  0 33,103,3658 ** 3658 m above msl u wind [m/s]
UGRD2743m  0 33,103,2743 ** 2743 m above msl u wind [m/s]
UGRD1829m  0 33,103,1829 ** 1829 m above msl u wind [m/s]
UGRD914m  0 33,103,914 ** 914 m above msl u wind [m/s]
UGRD610m  0 33,103,610 ** 610 m above msl u wind [m/s]
UGRD457m  0 33,103,457 ** 457 m above msl u wind [m/s]
UGRD305m  0 33,103,305 ** 305 m above msl u wind [m/s]
UGRD10m  0 33,105,10 ** 10 m above ground u wind [m/s]
UGRDsig995   0 33,107,9950 ** sigma=.995  u wind [m/s]
UGRD320K  0 33,113,320 ** 320K level u wind [m/s]
UGRD30_0mb  0 33,116,7680 ** 30-0 mb above gnd u wind [m/s]
UGRD60_30mb  0 33,116,15390 ** 60-30 mb above gnd u wind [m/s]
UGRD90_60mb  0 33,116,23100 ** 90-60 mb above gnd u wind [m/s]
UGRD120_90mb  0 33,116,30810 ** 120-90 mb above gnd u wind [m/s]
UGRD150_120mb  0 33,116,38520 ** 150-120 mb above gnd u wind [m/s]
UGRD180_150mb  0 33,116,46230 ** 180-150 mb above gnd u wind [m/s]
UGRDpv500  0 33,117,500 ** pot vorticity = 500 units level u wind [m/s]
UGRDpv1000  0 33,117,1000 ** pot vorticity = 1000 units level u wind [m/s]
UGRDpv1500  0 33,117,1500 ** pot vorticity = 1500 units level u wind [m/s]
UGRDpv2000  0 33,117,2000 ** pot vorticity = 2000 units level u wind [m/s]
UGRDpvneg500  0 33,117,33268 ** pot vorticity = -500 units level u wind [m/s]
UGRDpvneg1000  0 33,117,33768 ** pot vorticity = -1000 units level u wind [m/s]
UGRDpvneg1500  0 33,117,34268 ** pot vorticity = -1500 units level u wind [m/s]
UGRDpvneg2000  0 33,117,34768 ** pot vorticity = -2000 units level u wind [m/s]
UGRDmwl  0 33,6,0 ** max wind level u wind [m/s]
UGRDtrp  0 33,7,0 ** tropopause u wind [m/s]
ULWRFsfc  0 212,1,0  ** surface Upward long wave flux [W/m^2]
ULWRFtoa  0 212,8,0 ** top of atmos Upward long wave flux [W/m^2]
USTM0_6000m   0 196,106,15360 ** 0-6000 m above ground u-component of storm motion [m/s]
USWRFsfc  0 211,1,0  ** surface Upward short wave flux [W/m^2]
USWRFtoa  0 211,8,0 ** top of atmos Upward short wave flux [W/m^2]
VGWDsfc  0 148,1,0  ** surface Meridional gravity wave stress [N/m^2]
VFLXsfc  0 125,1,0  ** surface Meridional momentum flux [N/m^2]
VGRDprs $nlev 34,100,0 ** (profile) v wind [m/s]
VGRD4572m  0 34,103,4572 ** 4572 m above msl v wind [m/s]
VGRD3658m  0 34,103,3658 ** 3658 m above msl v wind [m/s]
VGRD2743m  0 34,103,2743 ** 2743 m above msl v wind [m/s]
VGRD1829m  0 34,103,1829 ** 1829 m above msl v wind [m/s]
VGRD914m  0 34,103,914 ** 914 m above msl v wind [m/s]
VGRD610m  0 34,103,610 ** 610 m above msl v wind [m/s]
VGRD457m  0 34,103,457 ** 457 m above msl v wind [m/s]
VGRD305m  0 34,103,305 ** 305 m above msl v wind [m/s]
VGRD10m  0 34,105,10 ** 10 m above ground v wind [m/s]
VGRDsig995   0 34,107,9950 ** sigma=.995  v wind [m/s]
VGRD320K  0 34,113,320 ** 320K level v wind [m/s]
VGRD30_0mb  0 34,116,7680 ** 30-0 mb above gnd v wind [m/s]
VGRD60_30mb  0 34,116,15390 ** 60-30 mb above gnd v wind [m/s]
VGRD90_60mb  0 34,116,23100 ** 90-60 mb above gnd v wind [m/s]
VGRD120_90mb  0 34,116,30810 ** 120-90 mb above gnd v wind [m/s]
VGRD150_120mb  0 34,116,38520 ** 150-120 mb above gnd v wind [m/s]
VGRD180_150mb  0 34,116,46230 ** 180-150 mb above gnd v wind [m/s]
VGRDpv500  0 34,117,500 ** pot vorticity = 500 units level v wind [m/s]
VGRDpv1000  0 34,117,1000 ** pot vorticity = 1000 units level v wind [m/s]
VGRDpv1500  0 34,117,1500 ** pot vorticity = 1500 units level v wind [m/s]
VGRDpv2000  0 34,117,2000 ** pot vorticity = 2000 units level v wind [m/s]
VGRDpvneg500  0 34,117,33268 ** pot vorticity = -500 units level v wind [m/s]
VGRDpvneg1000  0 34,117,33768 ** pot vorticity = -1000 units level v wind [m/s]
VGRDpvneg1500  0 34,117,34268 ** pot vorticity = -1500 units level v wind [m/s]
VGRDpvneg2000  0 34,117,34768 ** pot vorticity = -2000 units level v wind [m/s]
VGRDmwl  0 34,6,0 ** max wind level v wind [m/s]
VGRDtrp  0 34,7,0 ** tropopause v wind [m/s]
VISsfc  0 20,1,0  ** surface Visibility [m]
VSTM0_6000m   0 197,106,15360 ** 0-6000 m above ground v-component of storm motion [m/s]
VVELprs $nlev 39,100,0 ** (profile) Pressure vertical velocity [Pa/s]
VVELsig995   0 39,107,9950 ** sigma=.995  Pressure vertical velocity [Pa/s]
VWSHpv500  0 136,117,500 ** pot vorticity = 500 units level Vertical speed shear [1/s]
VWSHpv1000  0 136,117,1000 ** pot vorticity = 1000 units level Vertical speed shear [1/s]
VWSHpv1500  0 136,117,1500 ** pot vorticity = 1500 units level Vertical speed shear [1/s]
VWSHpv2000  0 136,117,2000 ** pot vorticity = 2000 units level Vertical speed shear [1/s]
VWSHpvneg500  0 136,117,33268 ** pot vorticity = -500 units level Vertical speed shear [1/s]
VWSHpvneg1000  0 136,117,33768 ** pot vorticity = -1000 units level Vertical speed shear [1/s]
VWSHpvneg1500  0 136,117,34268 ** pot vorticity = -1500 units level Vertical speed shear [1/s]
VWSHpvneg2000  0 136,117,34768 ** pot vorticity = -2000 units level Vertical speed shear [1/s]
VWSHtrp  0 136,7,0 ** tropopause Vertical speed shear [1/s]
WATRsfc  0 90,1,0  ** surface Water runoff [kg/m^2]
WEASDsfc  0 65,1,0  ** surface Accum. snow [kg/m^2]
WILTsfc  0 219,1,0  ** surface Wilting point [fraction]
var191sfc  0 191,1,0  ** surface undefined
ENDVARS
EOF
#-----------------------------------------------
gribmap -0 -i $ctldir/${exp}_${hr}.ctl


rm $ctldir/${exp}_${hr}m.ctl $ctldir/${exp}.t${cyc}z.pgrb${hr}m.idx
#-----------------------------------
cat >$ctldir/${exp}_${hr}m.ctl <<EOF
dset ${datadir}/pgb${hr}m${dump}%y4%m2%d2${cyc}
index $ctldir/${exp}.t${cyc}z.pgrb${hr}m.idx
undef 9.999E+20
title ${exp}.t${cyc}z.pgrb${hr}
*  produced by grib2ctl v0.9.12.5p39a
dtype grib $gribtype
format template
options yrev
xdef $nptx linear 0.000000 $dxy          
ydef $npty linear -90.000000 $dxy  
tdef $ndays linear ${cyc}Z${day}${monc}${year} 1dy
zdef 1 linear 1 1
vars 37
ALBDOsfc  0 84,1,0  ** surface Albedo [%]
CDUVBsfc  0 201,1,0  ** surface Clear Sky UV-B Downward Solar Flux [W/m^2]
CFRZRsfc  0 141,1,0  ** surface Categorical freezing rain [yes=1;no=0]
CICEPsfc  0 142,1,0  ** surface Categorical ice pellets [yes=1;no=0]
CPRATsfc  0 214,1,0  ** surface Convective precip. rate [kg/m^2/s]
CRAINsfc  0 140,1,0  ** surface Categorical rain [yes=1;no=0]
CSNOWsfc  0 143,1,0  ** surface Categorical snow [yes=1;no=0]
CWORKclm  0 146,200,0 ** atmos column Cloud work function [J/kg]
DLWRFsfc  0 205,1,0  ** surface Downward long wave flux [W/m^2]
DSWRFsfc  0 204,1,0  ** surface Downward short wave flux [W/m^2]
DUVBsfc  0 200,1,0  ** surface UV-B Downward Solar Flux [W/m^2]
GFLUXsfc  0 155,1,0  ** surface Ground heat flux [W/m^2]
LHTFLsfc  0 121,1,0  ** surface Latent heat flux [W/m^2]
PRATEsfc  0 59,1,0  ** surface Precipitation rate [kg/m^2/s]
PRESlcb  0 1,212,0 ** low cloud base Pressure [Pa]
PRESlct  0 1,213,0 ** low cloud top Pressure [Pa]
PRESmcb  0 1,222,0 ** mid-cloud base Pressure [Pa]
PRESmct  0 1,223,0 ** mid-cloud top Pressure [Pa]
PREShcb  0 1,232,0 ** high cloud base Pressure [Pa]
PREShct  0 1,233,0 ** high cloud top Pressure [Pa]
SHTFLsfc  0 122,1,0  ** surface Sensible heat flux [W/m^2]
TCDCclm  0 71,200,0 ** atmos column Total cloud cover [%]
TCDCbcl  0 71,211,0 ** boundary cld layer Total cloud cover [%]
TCDClcl  0 71,214,0 ** low cloud level Total cloud cover [%]
TCDCmcl  0 71,224,0 ** mid-cloud level Total cloud cover [%]
TCDChcl  0 71,234,0 ** high cloud level Total cloud cover [%]
TMPlct  0 11,213,0 ** low cloud top Temp. [K]
TMPmct  0 11,223,0 ** mid-cloud top Temp. [K]
TMPhct  0 11,233,0 ** high cloud top Temp. [K]
UGWDsfc  0 147,1,0  ** surface Zonal gravity wave stress [N/m^2]
UFLXsfc  0 124,1,0  ** surface Zonal momentum flux [N/m^2]
ULWRFsfc  0 212,1,0  ** surface Upward long wave flux [W/m^2]
ULWRFtoa  0 212,8,0 ** top of atmos Upward long wave flux [W/m^2]
USWRFsfc  0 211,1,0  ** surface Upward short wave flux [W/m^2]
USWRFtoa  0 211,8,0 ** top of atmos Upward short wave flux [W/m^2]
VGWDsfc  0 148,1,0  ** surface Meridional gravity wave stress [N/m^2]
VFLXsfc  0 125,1,0  ** surface Meridional momentum flux [N/m^2]
ENDVARS
EOF
#-----------------------------------------------
gribmap -0 -i $ctldir/${exp}_${hr}m.ctl


done
#-----------------------------------
n=`expr $n + 1 `               
done

exit
