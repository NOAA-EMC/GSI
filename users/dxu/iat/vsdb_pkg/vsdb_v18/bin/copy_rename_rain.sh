#!/bin/sh
#set -x

# copy and rename rain files

expold=prhs13
oldc="prhs13  "

expnew=gfs2015
newc="gfs2015 "

dates=20120501
datee=20121231 

#olddir=/global/noscrub/emc.glopara/archive/$expold
olddir=/global/noscrub/Russ.Treadon/archive/$expold
newdir=/global/noscrub/Fanglin.Yang/archive/$expnew
myhost=`echo $(hostname) |cut -c 1-1 `
if [ $myhost = "t" ]; then CLIENT="gyre" ;fi
if [ $myhost = "g" ]; then CLIENT="tide" ;fi

if [ ! -s $newdir ]; then mkdir -p $newdir; fi
cd $newdir ||exit 8

yyyymmdd=$dates
while [ $yyyymmdd -le $datee ]; do 

#--00Z stats
ofile=${expold}_rain_${yyyymmdd}
infile=$olddir/$ofile                        
outfile=${expnew}_rain_${yyyymmdd}
if [ -s $infile ]; then
 sed "s?$oldc?$newc?g" $infile > $outfile                     
else
 scp -p $LOGNAME@${CLIENT}:$infile $ofile
 if [ -s $ofile ]; then
  sed "s?$oldc?$newc?g" $ofile > $outfile                     
 fi
 rm $ofile
fi
 

#--12Z stats
ofile=${expold}_rain_${yyyymmdd}12
infile=$olddir/$ofile                        
outfile=${expnew}_rain_${yyyymmdd}12
if [ -s $infile ]; then
 sed "s?$oldc?$newc?g" $infile > $outfile                     
else
 scp -p $LOGNAME@${CLIENT}:$infile $ofile
 if [ -s $ofile ]; then
  sed "s?$oldc?$newc?g" $ofile > $outfile                     
 fi
 rm $ofile
fi

yyyymmdd=`/nwprod/util/exec/ndate +24 ${yyyymmdd}00 |cut -c 1-8`
#yyyymmdd=`/scratch2/portfolios/NCEPDEV/global/save/Fanglin.Yang/VRFY/vsdb/nwprod/util/exec/ndate +24 ${yyyymmdd}00 |cut -c 1-8`
done


exit
