#!/bin/sh
set -xa

#----------------------------------------------------------
#
#  diag2grad_uv_case.sh
#
#----------------------------------------------------------


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
  subtype=$9
  hint=${10}
  tmpdir=${11}

export ndatex=/nwprod/util/exec/ndate


#uv230_card=alllev
#uv231_card=alllev
#uv232_card=alllev
#uv233_card=alllev
#uv242_card=alllev
#uv243_card=allev
#uv245_card=upair
#uv246_card=upair
#uv252_card=upair
#uv253_card=acft
###uv257_card=acft
#uv258_card=upair
#uv221_card=acft
card=alllev
#hint=10    

nreal_uv=$nreal        ### one less than the data items of diagnostic files
nreal2_uv=`expr $nreal - 2`

cd $tmpdir

if [ "$type" = 'uv221' -o "$type" = 'uv230' -o "$type" = 'uv231' -o "$type" = 'uv232' -o "$type" = 'uv233' -o "$type" = 'uv234' -o "$type" = 'uv235' -o "$type" = 'uv242' -o "$type" = 'uv243'  -o "$type" = 'uv245' -o "$type" = 'uv246' -o "$type" = 'uv247' -o "$type" = 'uv248' -o "$type" = 'uv249' -o "$type" = 'uv250' -o "$type" = 'uv251' -o "$type" = 'uv252' -o "$type" = 'uv253' -o "$type" = 'uv254' -o "$type" = 'uv255' -o "$type" = 'uv256' -o "$type" = 'uv257' -o "$type" = 'uv258' ]; then
 rm -f diag2grads
  cp $sorcdir/grads_lev.x ./diag2grads
#  eval card=\${${type}_card}
  ctype=`echo ${type} | cut -c3-5`
  rm -f input
cat <<EOF >input
      &input
       intype=' uv',stype='${type}',itype=$ctype,nreal=$nreal_uv,nreal2=$nreal2_uv,
       iscater=1,igrads=1,levcard='$card',intv=$hint,subtype='${subtype}',isubtype=${subtype},
/
EOF
elif  [ "$type" = 'uv223' -o "$type" = 'uv224' -o "$type" = 'uv228' ]; then
 rm -f diag2grads
 cp $sorcdir/grads_sig.x ./diag2grads
 ctype=`echo ${type} | cut -c3-5`
 rm -f input
cat <<EOF >input
      &input
       intype=' uv',stype='${type}',itype=$ctype,nreal=$nreal_uv,nreal2=$nreal2_uv,
       iscater=1,igrads=1,subtype='${subtype}',isubtype=${subtype},
/
EOF
elif [ "$type" = 'uv220' ]; then
 rm -f diag2grads
 cp $sorcdir/grads_mandlev.x ./diag2grads
 ctype=`echo ${type} | cut -c3-5`
 rm -f input
cat <<EOF >input
      &input
       intype=' uv',stype='${type}',itype=$ctype,nreal=$nreal_uv,nreal2=$nreal2_uv,
       iscater=1,igrads=1,subtype='${subtype}',isubtype=${subtype},
/
EOF

elif [ "$type" = 'uv280' -o "$type" = 'uv281' -o "$type" = 'uv282' -o "$type" = 'uv284'  -o "$type" = 'uv287' ]; then
 rm -f diag2grads
 cp $sorcdir/grads_sfctime.x ./diag2grads
 ctype=`echo ${type} | cut -c3-5`
 rm -f input
cat <<EOF >input
      &input
       intype=' uv',stype='${type}',itype=$ctype,nreal=$nreal_uv,nreal2=$nreal2_uv,
       iscater=1,igrads=1,timecard='time11',subtype='${subtype}',isubtype=${subtype},
/
EOF

elif [ "$type" = 'uv229' ]; then
 rm -f diag2grads
 cp $sorcdir/grads_sfctime.x ./diag2grads
 ctype=`echo ${type} | cut -c3-5`
 rm -f input
cat <<EOF >input
      &input
       intype=' uv',stype='${type}',itype=$ctype,nreal=$nreal_uv,nreal2=$nreal2_uv,
       iscater=1,igrads=1,timecard='time7',subtype='${subtype}',isubtype=${subtype},
/
EOF


fi

./diag2grads <input>stdout 2>&1 
rm -f *tmp
mv stdout stdout_diag2grads_${type}_${subtype}.${cycle} 
for file in uv*grads
do
mv ${file} ${savedir}/${file}.${rdate}
done
for file in uv*scater
do
mv ${file} ${savedir}/${file}.${rdate}
done

exit
