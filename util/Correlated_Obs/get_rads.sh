#!/bin/sh
exp=prexoz
#where the files are located on hpss
pathf=/NCEPDEV/emc-da/5year/Kristen.Bathmann/ZEUS/${exp}
#where the radstat files will be save
noscr=/scratch1/portfolios/NCEPDEV/da/noscrub/Kristen.Bathmann/archive/${exp}
bdate=2013110906
edate=2013111018
cdate=$bdate
hpsstar=/home/Emily.Liu/bin/hpsstar
ndate=/scratch2/portfolios/NCEPDEV/global/save/Fanglin.Yang/VRFY/vsdb/nwprod/util/exec/ndate
while [[ $cdate -le $edate ]] ; do
  $hpsstar get ${pathf}/${cdate}gdas.tar radstat.gdas.$cdate
  mv radstat.gdas.$cdate $noscr
  cdate=`$ndate +06 $cdate`
done
