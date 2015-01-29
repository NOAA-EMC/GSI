#!/bin/sh
#set -x

# copy and rename vsdb files

dir=/global/hires/$LOGNAME/archive/vsdb_data

expold=pre13abcdefg
expnew=pre13d
dates=20100505
datee=20100820

oldc=`echo $expold |tr "[a-z]" "[A-Z]" `
newc=`echo $expnew |tr "[a-z]" "[A-Z]" `

for type in anom pres sfc; do
for cyc in 00 06 12 18; do

olddir=$dir/$type/${cyc}Z/$expold 
newdir=$dir/$type/${cyc}Z/$expnew 
mkdir -p $newdir
cd $newdir ||exit 8

yyyymmdd=$dates
while [ $yyyymmdd -le $datee ]; do 

infile=$olddir/${expold}_${yyyymmdd}.vsdb  
outfile=$newdir/${expnew}_${yyyymmdd}.vsdb  
sed "s?$oldc?$newc?g" $infile > $outfile                     

yyyymmdd=`/nwprod/util/exec/ndate +24 ${yyyymmdd}00 |cut -c -1-8`
done
done
done


exit
