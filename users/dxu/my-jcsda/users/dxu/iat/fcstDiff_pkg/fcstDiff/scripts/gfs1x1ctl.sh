#!/bin/sh
set -x

export dir0=${1:-/global/noscrub/$LOGNAME}
export rundir=${2:-/stmp/$LOGNAME/diffmap}
export exp=${3:-gfs}
export cyc=${4:-00}
export tag=${5:-pgbanl}
export sdate=${6:-01aug2007}
export CDATE=${7:-20070831}
export ndays=${8:-60}
export inter=${9:-12hr}

years=`echo $sdate |cut -c 6-9 `
mons=`echo $sdate |cut -c 3-5 `
days=`echo $sdate |cut -c 1-2 `

file=${exp}.t${cyc}z.${tag}
rm ${rundir}/${file}.ctl  ${rundir}/${file}.idx
export tag1=${tag}
if [ $tag1 = "pgbanl" ]; then tag1="pgbf00"; fi

cat >${rundir}/${file}.ctl <<EOF
dset ${dir0}/pgbf%f2.$CDATE${cyc}
options template
index ${rundir}/${file}.idx
undef 9.999E+20
dtype grib 3
options yrev
ydef 181 linear -90.000000 1
xdef 360 linear 0.000000 1.000000
*  z has 47 levels, for prs
tdef ${ndays} linear ${cyc}Z${days}${mons}${years} $inter
zdef 47 levels
1000 975 950 925 900 875 850 825 800 775 750 725 700 675 650 625 600 575 550 525 500 475 450 425 400 375 350 325 300 275 250 225 200 175 150 125 100 70 50 30 20 10 7 5 3 2 1
vars 143
no4LFTXsfc  0 132,1,0  ** surface Best (4-layer) lifted index [K]
no5WAVA500mb  0 230,100,500 ** 500 mb 5-wave geopot. height anomaly [gpm]
no5WAVH500mb  0 222,100,500 ** 500 mb 5-wave geopotential height [gpm]
ABSVprs 47 41,100,0 ** (profile) Absolute vorticity [/s]
CAPEsfc  0 157,1,0  ** surface Convective Avail. Pot. Energy [J/kg]
CAPE180_0mb  0 157,116,46080 ** 180-0 mb above gnd Convective Avail. Pot. Energy [J/kg]
CINsfc  0 156,1,0  ** surface Convective inhibition [J/kg]
CIN180_0mb  0 156,116,46080 ** 180-0 mb above gnd Convective inhibition [J/kg]
CLWMRprs 37 153,100,0 ** (profile) Cloud water [kg/kg]
CNWATsfc  0 223,1,0  ** surface Plant canopy surface water [kg/m^2]
CWATclm  0 76,200,0 ** atmos column Cloud water [kg/m^2]
GPAprs 2 27,100,0 ** (profile) Geopotential height anomaly [gpm]
HGTsfc  0 7,1,0  ** surface Geopotential height [gpm]
HGTprs 47 7,100,0 ** (profile) Geopotential height [gpm]
HGTpv2  0 7,117,2000 ** pot vorticity = 2000 units level Geopotential height [gpm]
HGTpvneg2  0 7,117,34768 ** pot vorticity = -2000 units level Geopotential height [gpm]
HGThtfl  0 7,204,0 ** highest trop freezing level Geopotential height [gpm]
HGT0deg  0 7,4,0 ** 0C isotherm level Geopotential height [gpm]
HGTmwl  0 7,6,0 ** max wind level Geopotential height [gpm]
HGTtrp  0 7,7,0 ** tropopause Geopotential height [gpm]
HPBLsfc  0 221,1,0  ** surface Planetary boundary layer height [m]
ICECsfc  0 91,1,0  ** surface Ice concentration (ice=1;no ice=0) [fraction]
ICETKsfc  0 92,1,0  ** surface Ice thickness [m]
LANDsfc  0 81,1,0  ** surface Land cover (land=1;sea=0) [fraction]
LFTXsfc  0 131,1,0  ** surface Surface lifted index [K]
O3MRprs 11 154,100,0 ** (profile) Ozone mixing ratio [kg/kg]
POTsig995   0 13,107,9950 ** sigma=.995  Potential temp. [K]
PRESsfc  0 1,1,0  ** surface Pressure [Pa]
PRESpv2  0 1,117,2000 ** pot vorticity = 2000 units level Pressure [Pa]
PRESpvneg2  0 1,117,34768 ** pot vorticity = -2000 units level Pressure [Pa]
PRESmwl  0 1,6,0 ** max wind level Pressure [Pa]
PREStrp  0 1,7,0 ** tropopause Pressure [Pa]
PRMSLmsl  0 2,102,0  ** unknown level Pressure reduced to MSL [Pa]
PWATclm  0 54,200,0 ** atmos column Precipitable water [kg/m^2]
RHprs 37 52,100,0 ** (profile) Relative humidity [%]
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
SNODsfc  0 66,1,0  ** surface Snow depth [m]
SOILL0_10cm  0 160,112,10 ** 0-10 cm underground Liquid volumetric soil moisture (non-frozen) [fraction]
SOILL10_40cm  0 160,112,2600 ** 10-40 cm underground Liquid volumetric soil moisture (non-frozen) [fraction]
SOILL40_100cm  0 160,112,10340 ** 40-100 cm underground Liquid volumetric soil moisture (non-frozen) [fraction]
SOILL100_200cm  0 160,112,25800 ** 100-200 cm underground Liquid volumetric soil moisture (non-frozen) [fraction]
SOILW0_10cm  0 144,112,10 ** 0-10 cm underground Volumetric soil moisture [fraction]
SOILW10_40cm  0 144,112,2600 ** 10-40 cm underground Volumetric soil moisture [fraction]
SOILW40_100cm  0 144,112,10340 ** 40-100 cm underground Volumetric soil moisture [fraction]
SOILW100_200cm  0 144,112,25800 ** 100-200 cm underground Volumetric soil moisture [fraction]
SPFHprs 37 51,100,0 ** (profile) Specific humidity [kg/kg]
SPFH2m  0 51,105,2 ** 2 m above ground Specific humidity [kg/kg]
SPFH30_0mb  0 51,116,7680 ** 30-0 mb above gnd Specific humidity [kg/kg]
SPFH60_30mb  0 51,116,15390 ** 60-30 mb above gnd Specific humidity [kg/kg]
SPFH90_60mb  0 51,116,23100 ** 90-60 mb above gnd Specific humidity [kg/kg]
SPFH120_90mb  0 51,116,30810 ** 120-90 mb above gnd Specific humidity [kg/kg]
SPFH150_120mb  0 51,116,38520 ** 150-120 mb above gnd Specific humidity [kg/kg]
SPFH180_150mb  0 51,116,46230 ** 180-150 mb above gnd Specific humidity [kg/kg]
TCDCcvl  0 71,244,0 ** convective cld layer Total cloud cover [%]
TMPsfc  0 11,1,0  ** surface Temp. [K]
TMPprs 47 11,100,0 ** (profile) Temp. [K]
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
TMP30_0mb  0 11,116,7680 ** 30-0 mb above gnd Temp. [K]
TMP60_30mb  0 11,116,15390 ** 60-30 mb above gnd Temp. [K]
TMP90_60mb  0 11,116,23100 ** 90-60 mb above gnd Temp. [K]
TMP120_90mb  0 11,116,30810 ** 120-90 mb above gnd Temp. [K]
TMP150_120mb  0 11,116,38520 ** 150-120 mb above gnd Temp. [K]
TMP180_150mb  0 11,116,46230 ** 180-150 mb above gnd Temp. [K]
TMPpv2  0 11,117,2000 ** pot vorticity = 2000 units level Temp. [K]
TMPpvneg2  0 11,117,34768 ** pot vorticity = -2000 units level Temp. [K]
TMPmwl  0 11,6,0 ** max wind level Temp. [K]
TMPtrp  0 11,7,0 ** tropopause Temp. [K]
TOZNEclm  0 10,200,0 ** atmos column Total ozone [Dobson]
UGRDprs 47 33,100,0 ** (profile) u wind [m/s]
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
UGRD30_0mb  0 33,116,7680 ** 30-0 mb above gnd u wind [m/s]
UGRD60_30mb  0 33,116,15390 ** 60-30 mb above gnd u wind [m/s]
UGRD90_60mb  0 33,116,23100 ** 90-60 mb above gnd u wind [m/s]
UGRD120_90mb  0 33,116,30810 ** 120-90 mb above gnd u wind [m/s]
UGRD150_120mb  0 33,116,38520 ** 150-120 mb above gnd u wind [m/s]
UGRD180_150mb  0 33,116,46230 ** 180-150 mb above gnd u wind [m/s]
UGRDpv2  0 33,117,2000 ** pot vorticity = 2000 units level u wind [m/s]
UGRDpvneg2  0 33,117,34768 ** pot vorticity = -2000 units level u wind [m/s]
UGRDmwl  0 33,6,0 ** max wind level u wind [m/s]
UGRDtrp  0 33,7,0 ** tropopause u wind [m/s]
VGRDprs 47 34,100,0 ** (profile) v wind [m/s]
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
VGRD30_0mb  0 34,116,7680 ** 30-0 mb above gnd v wind [m/s]
VGRD60_30mb  0 34,116,15390 ** 60-30 mb above gnd v wind [m/s]
VGRD90_60mb  0 34,116,23100 ** 90-60 mb above gnd v wind [m/s]
VGRD120_90mb  0 34,116,30810 ** 120-90 mb above gnd v wind [m/s]
VGRD150_120mb  0 34,116,38520 ** 150-120 mb above gnd v wind [m/s]
VGRD180_150mb  0 34,116,46230 ** 180-150 mb above gnd v wind [m/s]
VGRDpv2  0 34,117,2000 ** pot vorticity = 2000 units level v wind [m/s]
VGRDpvneg2  0 34,117,34768 ** pot vorticity = -2000 units level v wind [m/s]
VGRDmwl  0 34,6,0 ** max wind level v wind [m/s]
VGRDtrp  0 34,7,0 ** tropopause v wind [m/s]
VVELprs 37 39,100,0 ** (profile) Pressure vertical velocity [Pa/s]
VVELsig995   0 39,107,9950 ** sigma=.995  Pressure vertical velocity [Pa/s]
VWSHpv2  0 136,117,2000 ** pot vorticity = 2000 units level Vertical speed shear [1/s]
VWSHpvneg2  0 136,117,34768 ** pot vorticity = -2000 units level Vertical speed shear [1/s]
VWSHtrp  0 136,7,0 ** tropopause Vertical speed shear [1/s]
WEASDsfc  0 65,1,0  ** surface Accum. snow [kg/m^2]
ENDVARS
EOF

gribmap -e -i ${rundir}/${file}.ctl
exit
