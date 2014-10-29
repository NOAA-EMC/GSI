#!/bin/sh
set -x

#--compare gsistat and find missed satellite data 


exp1=/climate/noscrub/glopara/archive/pru12d
exp2=/climate/noscrub/glopara/archive/prd09q1o
dump=gdas
rundir=/stmp/$LOGNAME/diffgsi
mkdir -p $rundir; cd $rundir; rm *

output=diff_${dump}_aqua.txt
rm $output


export DATEST=20080801     
export DATEND=20080830         
#------------------------
for cyc in 00 06 12 18; do
#------------------------

#for sat in amsua airs sndr iasi ssmi hires; do
for sat in  amsua airs ; do

CDATE=$DATEST
while [ $CDATE -le $DATEND ]; do

rm x1 x2
file=gsistat.${dump}.${CDATE}${cyc}

grep $sat ${exp1}/${file} |grep "o-g 01 rad  aqua"  >x1
grep $sat ${exp2}/${file} |grep "o-g 01 rad  aqua"  >x2

zero1=`eval grep $sat ${exp1}/${file} |grep  "o-g 01 rad  aqua" |cut -c -36-46`
zero2=`eval grep $sat ${exp2}/${file} |grep  "o-g 01 rad  aqua" |cut -c -36-46`

if [ $zero1 -gt 0 -a $zero2 -eq 0 ];then
 echo ${CDATE}${cyc} >>$output                 
 diff x1 x2 >> $output                 
fi
if [ $zero1 -eq 0 -a $zero2 -gt 0 ];then
 echo ${CDATE}${cyc} >>$output                 
 diff x1 x2 >> $output                 
fi





#------------------------
CDATE=` ${ndate_dir}/ndate +24 ${CDATE}${cyc} | cut -c1-8`
done 
#------------------------
done 
#------------------------
done 
#------------------------


exit
