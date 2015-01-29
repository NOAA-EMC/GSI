#!/bin/sh
set -x
export para=/global/shared/glopara/prd09q1o
export yyyy=`date +%Y`
export mm=`date +%m`
export dd=`date +%d`
export hh=00
export fhrlist="f00 f06 f12 f18 f24 f30 f36 f42 f48 f54 f60 f66 f72 f78 f84 f90 f96 f102 f108 f114 f120 f126 f132 f138 f144 f150 f156 f162 f168 f174 f180 f186 f192 f198 f204 f210"
export rundir=/global/noscrub/wx23dc/caplan
if [ -s $rundir/pgbf180.gfs.$yyyy$mm$dd$hh ]; then
exit 8
fi
mkdir -p $rundir; cd $rundir ; rm -f pgb*

for fhr in $fhrlist; do
cat >/global/save/wx23dc/scripts/stdin <<EOF
cd $para
mget pgb${fhr}.gfs.$yyyy$mm$dd$hh
quit
EOF

sftp -b /global/save/wx23dc/scripts/stdin mist 

done

cat >/global/save/wx23dc/scripts/stdin2 << EOF
cd $para
mget pgb*.gdas.$yyyy$mm$dd$hh
mget pgbanl.gfs.$yyyy$mm$dd$hh
quit
EOF

sftp -b /global/save/wx23dc/scripts/stdin2 mist
echo "HI DANA, I'M FINISHED COPYING THE DATA"
