#!/bin/sh
set -xa
#
#if  [ $# -ne 1 ] ; then
# echo "usage: $0 date"
# exit 8
#fi
#
# Set mydir.  Remove and make clean mydir.  cd mydir
#
exp=$1
dtype=$2
mtype=$3
subtype=$4
rdate=$5
fixdir=$6
nreal=$7
exec=$8
type=$9
cycle=${10}
datadir=${11}
sorcdir=${12}
#exp=copr
# dtype=uv220_00
# mtype=uv220
#subtype=00
# rdate=2007071100
# fixdir=/nwprod/fix
# nreal=21
# exec=read_uv.x
# type=uv
# cycle=ges
# datadir=/u/wx20es/nbns/stats/convweb/copr/horz_hist/ges
# sorcdir=/u/wx20es/home/convweb/exec
# tmpdir=/ptmpp1/wx20es/test

# set up the directory with excutable files
  ndatex=/nwprod/util/exec/ndate
#  fixfile=global_convinfo.txt 


fname=$datadir/${dtype}.scater.${rdate}


#mkdir -p $tmpdir
#cd $tmpdir
#-----------------------------------------------------------
#
# Create namelist input on the file.  (Could make this
# a file which you copy to $mydir)
#
rm -f input
cat << EOF > input
  &input 
  nreal=${nreal},mtype='${mtype}',fname='${fname}',fileo='out',rlev=0.1,insubtype=${subtype},
/
EOF

   
#

cp $sorcdir/$exec ./$exec
#cp ${fixdir}/${fixfile} ./convinfo
cp $CONVINFO_FILE ./convinfo


./$exec <input  > stdout  2>&1

rm -f $exec
#rm -f convinfo
rm -f input
rm -f fname.out
mv out out_${dtype}_${cycle}.${rdate}
mv stdout stdout_${dtype}_${cycle}.${rdate}
if [ "${type}" = 'uv' ]; then
mv out_u out_${dtype}_u_${cycle}.${rdate}
mv out_v out_${dtype}_v_${cycle}.${rdate}
mv stdout_u stdout_${dtype}_u_${cycle}.${rdate}
mv stdout_v stdout_${dtype}_v_${cycle}.${rdate}
fi

exit
