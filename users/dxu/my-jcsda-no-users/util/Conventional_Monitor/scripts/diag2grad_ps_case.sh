#!/bin/sh
set -xa

#-------------------------------------------------------
#
#  diag2grad_ps_case.sh
#
#-------------------------------------------------------

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

echo "DEBUG:  start diag2grad_ps_case.sh"
echo "DEBUG:  exp       = $exp"
echo "DEBUG:  rdate     = $rdate"
echo "DEBUG:  fixdir    = $fixdir"
echo "DEBUG:  sorcdir   = $sorcdir"
echo "DEBUG:  cycle     = $cycle"
echo "DEBUG:  nreal     = $nreal"
echo "DEBUG:  savedir   = $savedir"
echo "DEBUG:  type      = $type"
echo "DEBUG:  subtype   = $subtype"
echo "DEBUG:  hint      = $hint"
echo "DEBUG:  tmpdir    = $tmpdir"
#exp=con2
#cdump=gdas
#rdate=2006040100
#fixdir=/u/wx20es/home/prycon/fix
#datadir=/ptmpp1/wx20es/tmp
#nreal=17             ## the neal in the set up subtract 2

cd $tmpdir

nreal_ps=$nreal        ### one less than the data items of diagnostic files
nreal2_ps=`expr $nreal - 2`        ### the data items in the grads files 
  

# the  directory with data files

ctype=`echo ${type} | cut -c3-5`

if [ "$type" = 'ps180' -o "$type" = 'ps181' -o  "$type" = 'ps183' -o  "$type" = 'ps187' ]; then
   rm -f diag2grads
   cp $sorcdir/grads_sfctime.x ./diag2grads
   rm -f input
cat <<EOF >input
      &input
       intype=' ps',stype='${type}',itype=$ctype,nreal=$nreal_ps,nreal2=$nreal2_ps,
       iscater=1,igrads=1,timecard='time11',subtype='${subtype}',isubtype=${subtype},
/
EOF
elif [ "$type" = 'ps120' ]; then
   rm -f diag2grads
   cp ${sorcdir}/grads_sfc.x ./diag2grads
   rm -f input
cat <<EOF >input
      &input
       intype=' ps',stype='${type}',itype=$ctype,nreal=$nreal_ps,nreal2=$nreal2_ps,
       iscater=1,igrads=1,subtype='${subtype}',isubtype=${subtype},
/
EOF
fi

./diag2grads <input>stdout 2>&1 
rm -f ${type}_${subtype}.tmp
mv stdout stdout_diag2grads_${type}_${subtype}.$cycle

for file in ps*grads; do 
   mv ${file} ${savedir}/${file}.${rdate}
done
for file in ps*scater; do
   mv ${file} ${savedir}/${file}.${rdate}
done

exit
