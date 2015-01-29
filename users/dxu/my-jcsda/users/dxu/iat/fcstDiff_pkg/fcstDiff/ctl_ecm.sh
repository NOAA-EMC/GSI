#!/bin/sh
set -x
# convert ECM 1x1-deg data to 2.5x2.5-deg 

export input_dir=${1:-/data/$LOGNAME/shared/pgbfiles}      
export run_dir=${2:-/scratch5/$LOGNAME/fcstdiff_maps}
export exp=${3:-ecm}      
export cyc=${4:-00}      
export tag=${5:-pgbanl}      
export sdate=${6:-01aug2007}      
export CDATE=${7:-20070831}      
export ndays=${8:-60}      
export modelType=${9:-ecm}

years=`echo $sdate |cut -c 6-9 `
mons=`echo $sdate |cut -c 3-5 `
days=`echo $sdate |cut -c 1-2 `
mkdir -p ${run_dir}/${exp}  ||exit 8

n=1
while [ $n -le $ndays ]; do
   hr=`expr $n \* 24 `
   dd=`${ndate_dir}/ndate -$hr ${CDATE}${cyc}`
   inFile=$input_dir/${tag}.${modelType}.$CDATE$cyc
   outFile=${run_dir}/${exp}/${tag}.${exp}.$CDATE$cyc
   if [ -s ${inFile} ]
   then
      ${copygb_dir}/copygb -g2 -x ${inFile} ${outFile}
   else
      tag=pgbf00
      ${copygb_dir}/copygb -g2 -x ${inFile} ${outFile}
   fi
   n=`expr $n + 1 `
done


#-----------------------------
file=${exp}.t${cyc}z.${tag}
rm ${run_dir}/${file}.ctl  ${run_dir}/${file}.idx
export tag1=${tag}
#if [ $tag1 = "pgbanl" ]; then tag1="pgbf00"; fi

cat >${run_dir}/${file}.ctl <<EOF
dset ${run_dir}/${exp}/${tag}.${exp}.%y4%m2%d2${cyc}
format template
index ${run_dir}/${file}.idx
undef 9.999E+20
dtype grib 2
options yrev
ydef 73 linear -90.000000 2.5
xdef 144 linear 0.000000 2.500000
tdef ${ndays} linear ${cyc}Z${days}${mons}${years} 1dy 
zdef 14 levels 1000 925 850 700 500 400 300 250 200 150 100 50 20 10
vars 15
APCPsfc  0 61,1,0  ** surface Total precipitation [kg/m^2]
DPT2m  0 17,105,2 ** 2 m above ground Dew point temp. [K]
HGTprs 14 7,100,0 ** (profile) Geopotential height [gpm]
PRESsfc  0 1,1,0  ** surface Pressure [Pa]
PRMSLmsl  0 2,102,0  ** unknown level Pressure reduced to MSL [Pa]
RHprs 14 52,100,0 ** (profile) Relative humidity [%]
TCDCcvl  0 71,244,0 ** convective cld layer Total cloud cover [%]
TMAX2m  0 15,105,2 ** 2 m above ground Max. temp. [K]
TMIN2m  0 16,105,2 ** 2 m above ground Min. temp. [K]
TMPprs 14 11,100,0 ** (profile) Temp. [K]
TMP2m  0 11,105,2 ** 2 m above ground Temp. [K]
UGRDprs 14 33,100,0 ** (profile) u wind [m/s]
UGRD10m  0 33,105,10 ** 10 m above ground u wind [m/s]
VGRDprs 14 34,100,0 ** (profile) v wind [m/s]
VGRD10m  0 34,105,10 ** 10 m above ground v wind [m/s]
ENDVARS
EOF

${gribmap_dir}/gribmap -0 -i ${run_dir}/${file}.ctl

exit
