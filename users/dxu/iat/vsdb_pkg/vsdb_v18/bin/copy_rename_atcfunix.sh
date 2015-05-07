#!/bin/sh
#set -x

# copy and rename atcfunix files

expold=prhs13
oldc=PRHS      

expnew=gfs2015
newc=GFS2       

dates=20120101
datee=20121231 

olddir=/global/noscrub/Fanglin.Yang/archive/gfsq1fy15_tmp
newdir=/global/noscrub/Fanglin.Yang/archive/$expnew  

mkdir -p $newdir
cd $newdir ||exit 8

yyyymmdd=$dates
while [ $yyyymmdd -le $datee ]; do 
for cyc in 00 06 12 18; do

infile=$olddir/atcfunix.gfs.${yyyymmdd}${cyc}
outfile=$newdir/atcfunix.gfs.${yyyymmdd}${cyc}
if [ -s $infile ]; then
 sed "s?$oldc?$newc?g" $infile > $outfile                     
fi

done
yyyymmdd=`/nwprod/util/exec/ndate +24 ${yyyymmdd}00 |cut -c 1-8`
#yyyymmdd=`/scratch2/portfolios/NCEPDEV/global/save/Fanglin.Yang/VRFY/vsdb/nwprod/util/exec/ndate +24 ${yyyymmdd}00 |cut -c 1-8`
done


exit
