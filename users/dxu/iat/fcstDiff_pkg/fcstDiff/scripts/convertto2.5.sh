#!/bin/sh
set -x
adate=2012111300
edate=2012111300
rundir=/ptmp/wx23dc/lowres
datadir=/ptmp/wx23dc/prgm141con
#list6="anl f00 f06 f12 f18 f24 f30 f36 f42 f48 f54 f60 f66 f72 f78 f84 f90 f96 f102 f108 f114 f120 f126 f132 f138 f144 f150 f156 f162 f168 f174 f180 f186 f192 f204 f216 f228 f240" 
#252 264 276 288 300 312 324 336 348 360 372 384"
list3="anl f00 f03 f06 f09 f12 f15 f18 f21 f24 f27 f30 f33 f36 f39 f42 f45 f48 f51 f54 f57 f60 f63 f66 f69 f72 f75 f78 f81 f84 f87 f90 f93 f96 f99 f102 f105 f108 f111 f114 f117 f120 f123 f126 f129 f132 f135 f138 f141 f144 f147 f150 f153 f156 f159 f162 f165 f168 f171 f174 f177 f180 f183 f186 f189 f192" 
list1="anl f00 f24 f48 f72 f96 f120 f144 f168 f192 f216 f240 f264 f288 f312 f336 f360 f384"
mkdir $rundir
ndate=${ndate_dir}/ndate

while [ $adate -le $edate ]; do
for fhr in $list1; do
/nwprod/util/exec/copygb -x -g2 $datadir/pgb${fhr}.gfs.$adate $rundir/pgb${fhr}.gfs.$adate
done
adate=`$ndate +24 $adate`
done
exit

