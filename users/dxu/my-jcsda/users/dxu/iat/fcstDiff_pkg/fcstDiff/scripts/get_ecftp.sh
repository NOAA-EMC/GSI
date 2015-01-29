#!/bin/sh
set -x
HOST='nanuk.eosdis.nasa.gov'
savedir=/global/noscrub/wx23dc/ecmhires
if [ ! -d $savedir ]; then mkdir $savedir; fi
cd $savedir
#mmdd=`date +%m%d`

sdate=2009082300
edate=2009082400
while [ $sdate -le $edate ] ; do
yy=`echo $sdate  | cut -c 1-4`
mm=`echo $sdate  | cut -c 5-6`
dd=`echo $sdate  | cut -c 7-8`
cyc=`echo $sdate | cut -c 9-10`
min='00'
file=UAD${mm}${dd}${cyc}${min}${mm}${dd}${cyc}${min}1.gz

cat << EOF >ftpin
  binary
  get $file
  quit
EOF
ftp -i -v $HOST < ftpin
gunzip $file
gfile=`echo $file | cut -c1-20`
ls -l $gfile
mv $gfile pgbanl.$sdate
sdate=`${ndate_dir}/ndate   +06 $sdate`
done
exit $rc
