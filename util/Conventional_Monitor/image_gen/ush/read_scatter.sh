#!/bin/sh
set -xa
#-----------------------------------------------------------------------------------
#
#  read_scatter.sh
#
#-----------------------------------------------------------------------------------

echo "---> read_scatter.sh"
echo " CMON_SUFFIX = $CMON_SUFFIX"

##if  [ $# -ne 1 ] ; then
## echo "usage: $0 date"
## exit 8
##fi


## Set mydir.  Remove and make clean mydir.  cd mydir
##????

exp=$1
echo "exp set to $exp"
dtype=$2
echo "dtype set to $dtype"
mtype=$3
echo "mtype set to $mtype"
subtype=$4
echo "subtype set to $subtype"
rdate=$5
echo "rdate set to $rdate"
fixdir=$6
echo "fixdir set to $fixdir"
nreal=$7
echo "nreal set to $nreal"
exec=$8
echo "exec set to $exec"
type=$9
echo "type set to $type"
cycle=${10}
echo "cycle set to $cycle"
datadir=${11}
echo "datadir set to $datadir"
sorcdir=${12}
echo "sorcdir set to $sorcdir"

##exp=copr
## dtype=uv220_00
## mtype=uv220
##subtype=00
## rdate=2007071100
## fixdir=/nwprod/fix
## nreal=21
## exec=read_uv.x
## type=uv
## cycle=ges
## datadir=/u/wx20es/nbns/stats/convweb/copr/horz_hist/ges
## sorcdir=/u/wx20es/home/convweb/exec


## set up the directory with excutable files

fixfile=global_convinfo.txt 
cp ${fixdir}/${fixfile} ./convinfo

fname=$datadir/${dtype}.scater.${rdate}


#-----------------------------------------------------------
#
# Create namelist input file.  
#

rm -f input
cat << EOF > input
  &input 
  nreal=${nreal},mtype='${mtype}',fname='${fname}',fileo='out',rlev=0.1,insubtype=${subtype},
/
EOF

cp $sorcdir/$exec ./$exec
#cp $CONVINFO_FILE ./convinfo

./$exec <input  > stdout  2>&1

#rm -f $exec
##rm -f convinfo
#rm -f input
#rm -f fname.out
mv out out_${dtype}_${cycle}.${rdate}
mv stdout stdout_${dtype}_${cycle}.${rdate}

#if [ "${type}" = 'uv' ]; then
#mv out_u out_${dtype}_u_${cycle}.${rdate}
#mv out_v out_${dtype}_v_${cycle}.${rdate}
#mv stdout_u stdout_${dtype}_u_${cycle}.${rdate}
#mv stdout_v stdout_${dtype}_v_${cycle}.${rdate}
#fi


echo "<--- read_scatter.sh"
exit
