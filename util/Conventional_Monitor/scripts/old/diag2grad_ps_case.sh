#!/bin/sh
set -xa
#
#if  [ $# -ne 1 ] ; then
# echo "usage: $0 date"
# exit 8
#fi
#
# Set mydir.  Remove and make clean mydir.  cd mydir
  exp=$1
  rdate=$2
  fixdir=$3
  sorcdir=$4
  cycle=$5
  nreal=$6
  savedir=$7
  type=$8
  hint=${9}
  tmpdir=${10}

#exp=con2
#cdump=gdas
#rdate=2006040100
#fixdir=/u/wx20xs/home/prycon/fix
#datadir=/ptmp/wx20xs/tmp
#nreal=17             ## the neal in the set up subtract 2

cd $tmpdir

# set up the directory with excutable files
  export ndatex=/nwprod/util/exec/ndate

  nreal_ps=$nreal        ### one less than the data items of diagnostic files
  nreal2_ps=`expr $nreal - 2`        ### the data items in the grads files 
  

# the  directory with data files


if [ "$type" = 'ps180' -o "$type" = 'ps181' -o  "$type" = 'ps183' -o  "$type" = 'ps187' ]; then
 rm -f diag2grads
  cp $sorcdir/grads_sfctime ./diag2grads
  ctype=`echo ${type} | cut -c3-5`
  rm -f input
cat <<EOF >input
      &input
       intype=' ps',stype='${type}',itype=$ctype,nreal=$nreal_ps,nreal2=$nreal2_ps,
       iscater=1,igrads=1,timecard='time11',
/
EOF
elif [ "$type" = 'ps120' ]; then
 rm -f diag2grads
 cp ${sorcdir}/grads_sfc ./diag2grads
 ctype=`echo ${type} | cut -c3-5`
 rm -f input
cat <<EOF >input
      &input
       intype=' ps',stype='${type}',itype=$ctype,nreal=$nreal_ps,nreal2=$nreal2_ps,
       iscater=1,igrads=1,
/
EOF
fi

./diag2grads <input>stdout 2>&1 
rm -f ${type}.tmp
mv stdout stdout_diag2grads_${type}_$cycle

for file in ps*grads 
do
mv ${file} ${savedir}/${file}.${rdate}
done
for file in ps*scater
do
mv ${file} ${savedir}/${file}.${rdate}
done

exit
