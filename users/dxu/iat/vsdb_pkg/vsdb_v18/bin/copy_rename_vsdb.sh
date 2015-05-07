#!/bin/sh
#set -x

# copy and rename vsdb files


expold=prhs12
expnew=gfs2015
dates=20120501
datee=20121231

diro=/global/noscrub/emc.glopara/archive/vsdb_data
dirn=/global/noscrub/Fanglin.Yang/archive/vsdb_data

myhost=`echo $(hostname) |cut -c 1-1 `
if [ $myhost = "t" ]; then CLIENT="gyre" ;fi
if [ $myhost = "g" ]; then CLIENT="tide" ;fi


oldc=`echo $expold |tr "[a-z]" "[A-Z]" `
newc=`echo $expnew |tr "[a-z]" "[A-Z]" `

for type in anom pres sfc; do
for cyc in 00 06 12 18; do

olddir=$diro/$type/${cyc}Z/$expold 
newdir=$dirn/$type/${cyc}Z/$expnew 
if [ ! -s $newdir ]; then mkdir -p $newdir ;fi
cd $newdir ||exit 8

yyyymmdd=$dates
while [ $yyyymmdd -le $datee ]; do 

infile=$olddir/${expold}_${yyyymmdd}.vsdb  
outfile=$newdir/${expnew}_${yyyymmdd}.vsdb  
if [ -s $infile ]; then
 sed "s?$oldc?$newc?g" $infile > $outfile                     
else
 scp -p ${LOGNAME}@${CLIENT}:${infile} ${expold}_${yyyymmdd}.vsdb
 infile=${expold}_${yyyymmdd}.vsdb  
 if [ -s $infile ]; then
  sed "s?$oldc?$newc?g" $infile > $outfile                     
  rm $infile
 fi
fi

yyyymmdd=`/nwprod/util/exec/ndate +24 ${yyyymmdd}00 |cut -c 1-8`
##yyyymmdd=`/scratch2/portfolios/NCEPDEV/global/save/Fanglin.Yang/VRFY/vsdb/nwprod/util/exec/ndate +24 ${yyyymmdd}00 |cut -c 1-8`
done
done
done


exit
