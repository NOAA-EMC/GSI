#!/bin/sh
set -xa

#----------------------------------------------------------
#
#  diag2grad_uv_case.sh
#
#----------------------------------------------------------

echo "--> diag2grad_uv_case.sh"

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
#uv257_card=acft
#uv258_card=upair
#uv221_card=acft

card=alllev

nreal_uv=$nreal   
nreal2_uv=`expr $nreal - 2`
echo "nreal2_uv = ", ${nreal2_uv}
echo "mtype     = ", ${mtype}

ctype=`echo ${mtype} | cut -c3-5`

if [ "$mtype" = 'uv221' -o "$mtype" = 'uv230' -o "$mtype" = 'uv231' -o "$mtype" = 'uv232' -o "$mtype" = 'uv233' -o "$mtype" = 'uv234' -o "$mtype" = 'uv235' -o "$mtype" = 'uv242' -o "$mtype" = 'uv243'  -o "$mtype" = 'uv245' -o "$mtype" = 'uv246' -o "$mtype" = 'uv247' -o "$mtype" = 'uv248' -o "$mtype" = 'uv249' -o "$mtype" = 'uv250' -o "$mtype" = 'uv251' -o "$mtype" = 'uv252' -o "$mtype" = 'uv253' -o "$mtype" = 'uv254' -o "$mtype" = 'uv255' -o "$mtype" = 'uv256' -o "$mtype" = 'uv257' -o "$mtype" = 'uv258' ]; then

   echo "IN if condition 1"
   rm -f diag2grads
   cp ${EXECcmon}/grads_lev.x ./diag2grads

      echo "INPUTS to grads_lev.x = "
      echo "intype   =  uv"
      echo "stype    = ", ${mtype}
      echo "itype    = ", ${ctype}
      echo "nreal    = ", ${nreal_uv}
      echo "nreal2   = ", ${nreal2_uv}
      echo "iscater  = 1"
      echo "igrads   = 1"
      echo "levcard  = ", ${card}
      echo "intv     = ", ${hint}
      echo "subtype  = ", ${subtype}
      echo "isubtype = ", ${subtype} 

   rm -f input
   cat <<EOF >input
      &input
       intype=' uv',stype='${mtype}',itype=$ctype,nreal=$nreal_uv,
       iscater=1,igrads=1,levcard='$card',intv=$hint,subtype='${subtype}',isubtype=${subtype},
/
EOF

elif  [ "$mtype" = 'uv223' -o "$mtype" = 'uv224' -o "$mtype" = 'uv228' ]; then
   echo "IN if condition 2"

   rm -f diag2grads
   cp ${EXECcmon}/grads_sig.x ./diag2grads
   rm -f input

   echo "9 inputs to grads_sig.x = "
   echo "intype =  uv"
   echo "stype  = ", ${mtype}
   echo "itype  = ", ${ctype}
   echo "nreal  = ", ${nreal_uv}
   echo "nreal2 = ", ${nreal2_uv}
   echo "iscater = 1"
   echo "igrads  = 1"
   echo "subtype = ", ${subtype}
   echo "isubtype = ", ${subtype}

   cat <<EOF >input
      &input
       intype=' uv',stype='${mtype}',itype=$ctype,nreal=$nreal_uv,
       iscater=1,igrads=1,subtype='${subtype}',isubtype=${subtype},
/
EOF

elif [ "$mtype" = 'uv220' ]; then
   echo "IN if condition 3"

   rm -f diag2grads
   cp ${EXECcmon}/grads_mandlev.x ./diag2grads
   rm -f input
   cat <<EOF >input
      &input
       intype=' uv',stype='${mtype}',itype=$ctype,nreal=$nreal_uv,
       iscater=1,igrads=1,subtype='${subtype}',isubtype=${subtype},
/
EOF

elif [ "$mtype" = 'uv280' -o "$mtype" = 'uv281' -o "$mtype" = 'uv282' -o "$mtype" = 'uv284'  -o "$mtype" = 'uv287' ]; then
   echo "IN if condition 4"

   rm -f diag2grads
   cp ${EXECcmon}/grads_sfctime.x ./diag2grads
   rm -f input
   cat <<EOF >input
      &input
       intype=' uv',stype='${mtype}',itype=$ctype,nreal=$nreal_uv,
       iscater=1,igrads=1,timecard='time11',subtype='${subtype}',isubtype=${subtype},
/
EOF

elif [ "$mtype" = 'uv229' ]; then
   echo "IN if condition 5"
   rm -f diag2grads
   cp ${EXECcmon}/grads_sfctime.x ./diag2grads
   rm -f input
   cat <<EOF >input
      &input
       intype=' uv',stype='${mtype}',itype=$ctype,nreal=$nreal_uv,
       iscater=1,igrads=1,timecard='time7',subtype='${subtype}',isubtype=${subtype},
/
EOF


fi

./diag2grads <input>stdout 2>&1 

##############################################
#  Create the nt file, rename stdout, move nt,
#  grads, and scatter files to $TANDIR_cmon
##############################################
ntline=`tail -n1 stdout`
nt=`echo ${ntline} | sed 's/^ *//g' | sed 's/ *$//g'`
if [ ${#nt} = 1 ]; then
   ntfile="nt_${mtype}_${subtype}.${PDATE}"
   echo ${nt} > ${ntfile}
   cp ${ntfile} ${TANKDIR_cmon}/horz_hist/${cycle}/.
fi

rm -f *tmp
mv stdout stdout_diag2grads_${mtype}_${subtype}.${cycle} 

dest_dir="${TANKDIR_cmon}/horz_hist/${cycle}"

for file in uv*grads; do
   mv ${file} ${dest_dir}/${file}.${PDATE}
done

for file in uv*scater; do
   mv ${file} ${dest_dir}/${file}.${PDATE}
done


echo "<-- diag2grad_uv_case.sh"

exit
