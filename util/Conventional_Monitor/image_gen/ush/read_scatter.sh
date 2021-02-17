#!/bin/sh
set -xa
#-----------------------------------------------------------------------------------
#
#  read_scatter.sh
#
#    Extract a subset of data from the scater file
#    and generate an out_${mtype) data file and 
#    control file for GrADS.
#-----------------------------------------------------------------------------------

echo "---> read_scatter.sh"
echo " CONMON_SUFFIX = $CONMON_SUFFIX"

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

## set up the directory with excutable files

fixfile=global_convinfo.txt 
if [[ ! -e ./convinfo ]]; then
   cp ${fixdir}/${fixfile} ./convinfo
fi

fname=$datadir/${dtype}.scater.${cycle}.${rdate}


#-----------------------------------------------------------
#
# Create namelist input file.  
#

rm -f input
cat << EOF > input
  &input 
  nreal=${nreal},
  mtype='${mtype}',
  fname='${fname}',
  fileo='out_${dtype}_${cycle}.${rdate}',
  rlev=0.1,
  insubtype=${subtype},
  grads_info_file='grads_info_${dtype}_${cycle}.${rdate}'
/
EOF

cp $sorcdir/$exec ./$exec

./$exec <input  > stdout_${dtype}_${cycle}.${rdate}  2>&1

#rm -f $exec
#rm -f input


if [ "${type}" = 'uv' ]; then
   mv out_u out_${dtype}_u_${cycle}.${rdate}
   mv out_v out_${dtype}_v_${cycle}.${rdate}

   mv stdout_u stdout_${dtype}_u_${cycle}.${rdate}
   mv stdout_v stdout_${dtype}_v_${cycle}.${rdate}

   mv grads_info_u grads_info_${dtype}_u_${cycle}.${rdate}
   mv grads_info_v grads_info_${dtype}_v_${cycle}.${rdate}
fi


echo "<--- read_scatter.sh"
exit
