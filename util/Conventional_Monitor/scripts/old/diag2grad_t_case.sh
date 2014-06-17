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

  export ndatex=/nwprod/util/exec/ndate


nreal2=`expr $nreal - 2`
 
### data id represents the way to write in grads format##### 
#t130_card=alllev
#t131_card=alllev
#t132_card=alllev
#t133_card=alllev
card=alllev
#hint=10

cd $tmpdir

# set up the directory as working directory 

if [ "$type" = 't130' -o "$type" = 't131' -o "$type" = 't132' -o "$type" = 't133' ]; then
 rm -f diag2grads
  cp $sorcdir/grads_lev ./diag2grads
#  eval card=\${${type}_card}
  ctype=`echo ${type} | cut -c2-4`
cat <<EOF >input
      &input
       intype='  t',stype='${type}',itype=$ctype,nreal=$nreal,nreal2=$nreal2,
       iscater=1,igrads=1,levcard='$card',intv=$hint
/
EOF
elif [ "$type" = 't120' ]; then
 rm -f diag2grads
 cp $sorcdir/grads_mandlev ./diag2grads
 ctype=`echo ${type} | cut -c2-4`
cat <<EOF >input
      &input
       intype='  t',stype='${type}',itype=$ctype,nreal=$nreal,nreal2=$nreal2,
       iscater=1,igrads=1,
/
EOF
elif [ "$type" = 't180' -o "$type" = 't181' -o "$type" = 't182' -o "$type" = 't183'  -o "$type" = 't187' ]; then
 rm -f diag2grads
 cp $sorcdir/grads_sfc ./diag2grads
 ctype=`echo ${type} | cut -c2-4`
cat <<EOF >input
      &input
       intype='  t',stype='${type}',itype=$ctype,nreal=$nreal,nreal2=$nreal2,
       iscater=1,igrads=1,
/
EOF

fi

./diag2grads <input>stdout 2>&1 
rm -f *tmp
mv stdout stdout_diag2grads_${type}_${cycle}
for file in t*grads
do
mv ${file} ${savedir}/${file}.${rdate}
done
for file in t*scater
do
mv ${file} ${savedir}/${file}.${rdate}
done


exit
