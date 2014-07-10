#!/bin/ksh
set -x

# get operatinal GDAS prepbufr data
# in /com or from HPSS archive          

myhost=`echo $(hostname) |cut -c 1-1 `
if [ $myhost = t ]; then HOST=tide; CLIENT=gyre ; fi
if [ $myhost = g ]; then HOST=gyre; CLIENT=tide ; fi
rhost=`echo $CLIENT |cut -c 1-1 `


CDATE=${1:-$(date +%Y%m%d)}            
CDATEM1=`/nwprod/util/exec/ndate -24 ${CDATE}00 |cut -c 1-8`
CDATEM2=`/nwprod/util/exec/ndate -48 ${CDATE}00 |cut -c 1-8`
CDATEM3=`/nwprod/util/exec/ndate -240 ${CDATE}00 |cut -c 1-8`

IDAY=$CDATEM1
while [ $IDAY -le $CDATE ]; do

comout=/global/noscrub/Fanglin.Yang/prepbufr/gdas
remote=/global/noscrub/Fanglin.Yang/prepbufr/gdas
cd $comout

errgdas=0
for vcyc in 00 06 12 18; do
  filein=/com/gfs/prod/gdas.$IDAY/gdas1.t${vcyc}z.prepbufr
  fileout=prepbufr.gdas.${IDAY}${vcyc}
  cp $filein $fileout
  if [ $? -ne 0 ]; then 
    yyyy=`echo $IDAY |cut -c 1-4 `
    mm=`echo $IDAY |cut -c 5-6 `
    dd=`echo $IDAY |cut -c 7-8 `
     hpssdir=/NCEPPROD/hpssprod/runhistory/rh${yyyy}/${yyyy}${mm}/${yyyy}${mm}${dd}
     hpssdir2=/hpssprod/runhistory/rh${yyyy}/${yyyy}${mm}/${yyyy}${mm}${dd}
     /nwprod/util/ush/hpsstar  get ${hpssdir}/com_gfs_prod_gdas.${yyyy}${mm}${dd}${vcyc}.tar   ./gdas1.t${vcyc}z.prepbufr
#    if [ $? -ne 0 ]; then 
#     /nwprod/util/ush/hpsstar  get ${hpssdir2}/com_gfs_prod_gdas.${yyyy}${mm}${dd}${vcyc}.tar   ./gdas1.t${vcyc}z.prepbufr
#    fi
     mv gdas1.t${vcyc}z.prepbufr $fileout
  fi
  chmod a+r $fileout
  scp -p $fileout ${LOGNAME}@${CLIENT}:${remote}/.

#--copy unrestricted prepbufr file to emc ftp site
  filein2=/com/gfs/prod/gdas.$IDAY/gdas1.t${vcyc}z.prepbufr.nr
  fileout2=prepbufr.gdas.${IDAY}${vcyc}
  scp -p $filein2 wx24fy@emcrzdm.ncep.noaa.gov:/home/people/emc/ftp/gc_wmb/wx24fy/GFS/bufr/${fileout2}
done
IDAY=`/nwprod/util/exec/ndate +24 ${IDAY}00 |cut -c 1-8`
done

ssh -q -l wx24fy emcrzdm.ncep.noaa.gov "chmod a+r /home/people/emc/ftp/gc_wmb/wx24fy/GFS/bufr/prepbufr.gdas.* "
ssh -q -l wx24fy emcrzdm.ncep.noaa.gov "rm /home/people/emc/ftp/gc_wmb/wx24fy/GFS/bufr/prepbufr.gdas.${CDATEM3}* "
exit
