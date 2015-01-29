#!/bin/sh
#
# make the control files for the 6 hour gdas quick look
# v1.1 2/99
set -xa

bdate=$1
edate=$2
disk=$3
cycle=$4
type=$5

hour=`echo $edate | cut -c9-10`

date0=$bdate
date1=$edate

#date0=`find $disk -name "${cycle}_${type}_stas.????????${hour}" -print | sort \
#   | head -n 1 | sed "s/^.*${cycle}_${type}_stas\.//"`
#date1=`find $disk -name "${cycle}_${type}_stas.????????${hour}" -print | sort \
#   | tail -n 1 | sed "s/^.*${cycle}_${type}_stas\.//"`

[ "$date0" = '' ] && continue

yr=`echo $date0 | cut -c1-4`
mo=`echo $date0 | cut -c5-6`
da=`echo $date0 | cut -c7-8`

yr1=`echo $date1 | cut -c1-4`

date0=`echo $date0 | cut -c1-8`
date1=`echo $date1 | cut -c1-8`

jday0=`./julian.sh $date0`
jday1=`./julian.sh $date1`

if [ "$yr" = "$yr1" ];then

t=`expr $jday1 - $jday0`
else

leap=`expr $yr % 4`
nbase=${yr1}000
if [ $leap -eq 0 ]; then
ndays1=${yr}366
else
ndays1=${yr}365
fi
t=`expr $jday1 - $nbase`
t=`expr $t + $ndays1`
t=`expr $t - $jday0`
fi

ndays=`expr $t + 1`

case $mo in
   01) month=jan;;
   02) month=feb;;
   03) month=mar;;
   04) month=apr;;
   05) month=may;;
   06) month=jun;;
   07) month=jul;;
   08) month=aug;;
   09) month=sep;;
   10) month=oct;;
   11) month=nov;;
   12) month=dec;;
    *) echo "month error $mo"
       exit 1;;
esac

gdate="${hour}Z${da}${month}$yr"

if  [ "${type}"  = 'u' -o "${type}"  = 'v' ]; then
cp  uv_stas.ctl  ${type}_stas.ctl 
fi 
# make pgb.ctl and flx.ctl
sed <${type}_stas.ctl -e "s/(date0)/$date0 ${hour}Z/" \
   -e "s/(date1)/$date1 ${hour}Z/" \
  -e "s/tdef.*\$/tdef $ndays linear $gdate 24hr/" \
  > tmp.ctl

setdir="dset ${disk}/${cycle}_${type}_stas.%y4%m2%d2%h2"
sed -e "s=^dset.*=${setdir}=" tmp.ctl >${type}_stas_${cycle}.ctl

if [ ! -s ${disk}/${type}_stas_${cycle}.ctl ];then
cp ${type}_stas_${cycle}.ctl ${disk}
fi

#exit 0
