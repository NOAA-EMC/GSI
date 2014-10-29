#!/bin/sh
#set -x

dir=/global/hires/glopara/archive/pre13a
savedir=/ptmp/wx23dc/t574
cdate=2010042712
edate=2010050200
ndate=${ndate_dir}/ndate
mkdir -p $savedir; cd $savedir

while [ $cdate -le $edate ]; do
  cp $dir/pgb*gfs*$cdate $savedir
  tar -cvf gfsT574.$cdate.tar pgb*$cdate 
  compress -fv gfsT574.$cdate.tar
  cdate=`$ndate +12 $cdate`
done
ls -l *tar.Z
exit

