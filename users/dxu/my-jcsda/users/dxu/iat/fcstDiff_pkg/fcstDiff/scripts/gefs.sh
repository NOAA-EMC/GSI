#!/bin/sh
CDATE=2012091500
EDATE=2012092000
ens=anl
ndate=${ndate_dir}/ndate
savedir=/ptmp/wx23dc/gefs
mkdir -p $savedir
cd $savedir
rm -f gefs*
while [ $CDATE -le $EDATE ] ; do
for ft in anl f00 f12 f24 f36 f48 f60 f72 f84 f96 f108 f120; do
for en in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20; do
#http://nomads.ncep.noaa.gov/cgi-bin/filter_gens.pl?file=gec00.t06z.pgrb2anl&lev_2_m_above_ground=on&lev_500_mb=on&lev_mean_sea_level=on&lev_surface=on&var_ACPCP=on&var_APCP=on&var_HGT=on&var_TMP=on&leftlon=0&rightlon=360&toplat=90&bottomlat=-90&dir=%2Fgefs.20120914%2F06%2Fpgrb2

wget -O gefs.$CDATE.pgrb2${ft}.$en "http://nomads.ncep.noaa.gov/cgi-bin/filter_gens.pl?file=gep${en}.t${CDATE#????????}z.pgrb2${ft}&lev_2_m_above_ground=on&lev_500_mb=on&lev_mean_sea_level=on&lev_surface=on&var_ACPCP=on&var_APCP=on&var_HGT=on&var_TMP=on&leftlon=0&rightlon=360&toplat=90&bottomlat=-90&dir=%2Fgefs.${CDATE%??}%2F${CDATE#????????}%2Fpgrb2"
sleep 1
done
done

for ft in anl f00 f12 f24 f36 f48 f60 f72 f84 f96 f108 f120; do
for en in 00; do
wget -O gefs.$CDATE.pgrb2${ft}.$en "http://nomads.ncep.noaa.gov/cgi-bin/filter_gens.pl?file=gec${en}.t${CDATE#????????}z.pgrb2${ft}&lev_2_m_above_ground=on&lev_500_mb=on&lev_mean_sea_level=on&lev_surface=on&var_ACPCP=on&var_APCP=on&var_HGT=on&var_TMP=on&leftlon=0&rightlon=360&toplat=90&bottomlat=-90&dir=%2Fgefs.${CDATE%??}%2F${CDATE#????????}%2Fpgrb2"
sleep 1
done
done
CDATE=`ndate +06 $CDATE`
done

