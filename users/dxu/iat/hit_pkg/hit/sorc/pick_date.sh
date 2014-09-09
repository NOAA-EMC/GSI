#!/bin/sh
set -x

rm y 
>y

for cdate in 20110819 20110820 20110821 20110822 20110823 20110824 20110825 20110826; do
for cyc in 00 12; do

data=${cdate}$cyc

day1=2011082400
day2=2011082618

fhr=120
while [ $fhr -ge 00 ]; do

day=`/nwprod/util/exec/ndate +$fhr $data`
if [ $day -ge $day1 -a $day -le $day2 ]; then 

if [ $fhr -lt 100 ]; then fhr=" $fhr"; fi
for mdl in AVNO HWRF GFDL FIM2 FIMY NGPS OFCL; do
 grep $day aal092011.dat | grep "${mdl}, $fhr, " >> y
done

fi

fhr=`expr $fhr - 6 `
if [ $fhr -lt 10 ]; then fhr=0$fhr ; fi
done

done
done
