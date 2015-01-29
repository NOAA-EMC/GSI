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
  rdate=$2
  fixdir=$3
  sorcdir=$4
  cycle=$5
  nreal=$6
  savedir=$7
  type=$8
  hint=${9}
  tmpdir=${10}

  nreal_q=$nreal        ### one less than the data items of diagnostic files
  nreal2_q=`expr $nreal - 2`        ### the data items in the grads files 


q130_card=alllev
q132_card=alllev
q133_card=alllev

cd $tmpdir
if [  "$type" = 'q132' -o "$type" = 'q133' ]; then
 rm -f diag2grads
  cp $sorcdir/grads_lev ./diag2grads
  eval card=\${${type}_card}
  ctype=`echo ${type} | cut -c2-4`
cat <<EOF >input
      &input
       intype='  q',stype='${type}',itype=$ctype,nreal=$nreal_q,nreal2=$nreal2_q,
       iscater=1,igrads=1,levcard='$card',intv=$hint
/
EOF
elif [ "$type" = 'q120' ]; then
 rm -f diag2grads
 cp $sorcdir/grads_mandlev ./diag2grads
 ctype=`echo ${type} | cut -c2-4`
cat <<EOF >input
      &input
       intype='  q',stype='${type}',itype=$ctype,nreal=$nreal_q,nreal2=$nreal2_q,
       iscater=1,igrads=1,
/
EOF
elif [ "$type" = 'q180' -o "$type" = 'q181' -o "$type" = 'q183'  -o "$type" = 'q187' ]; then
 rm -f diag2grads
 cp $sorcdir/grads_sfctime ./diag2grads
 ctype=`echo ${type} | cut -c2-4`
cat <<EOF >input
      &input
       intype='  q',stype='${type}',itype=$ctype,nreal=$nreal_q,nreal2=$nreal2_q,
       iscater=1,igrads=1,timecard='time11',
/
EOF

fi

./diag2grads <input>stdout 2>&1 
rm -f *tmp
mv stdout stdout_diag2grads_${type}_${cycle}
for file in q*grads
do
mv ${file} ${savedir}/${file}.${rdate}
done
for file in q*scater
do
mv ${file} ${savedir}/${file}.${rdate}
done

exit
