#!/bin/sh
exp=prexoz
#where the files are located on hpss
pathf=/NCEPDEV/emc-da/5year/${USER}/ZEUS/${exp}
#where the radstat files will be save
noscr=/scratch1/portfolios/NCEPDEV/da/noscrub/${USER}/archive/${exp}
bdate=2013110906
edate=2013111018
hpsstar=/home/Emily.Liu/bin/hpsstar
ndate=/scratch2/portfolios/NCEPDEV/global/save/Fanglin.Yang/VRFY/vsdb/nwprod/util/exec/ndate


#######

cdate=$bdate
while [[ $cdate -le $edate ]] ; do
  $hpsstar get ${pathf}/${cdate}gdas.tar radstat.gdas.$cdate
  mv radstat.gdas.$cdate $noscr
  cdate=`$ndate +06 $cdate`
done
