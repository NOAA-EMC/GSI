#!/bin/sh
set -xa

#------------------------------------------------------------
#
#  diag2grad_q_case.sh
#
#------------------------------------------------------------

echo "--> diag2grad_q_case.sh"

   echo "CMON_SUFFIX   = $CMON_SUFFIX"
   echo "TANKDIR_cmon  = $TANKDIR_cmon"
   echo "type          = $type"
   echo "PDATE         = $PDATE"
   echo "EXECcmon      = $EXECcmon"
   echo "cycle         = $cycle"
   echo "nreal         = $nreal"
   echo "mtype         = $mtype (type = $type)"
   echo "subtype       = $subtype"
   echo "hint          = $hint"
   echo "workdir       = $workdir"

  nreal_q=$nreal                 ### one less than the data items of diagnostic files
  nreal2_q=`expr $nreal - 2`     ### the data items in the grads files 


  q130_card=alllev
  q132_card=alllev
  q133_card=alllev

  ctype=`echo ${mtype} | cut -c2-4`

  if [  "$mtype" = 'q132' -o "$mtype" = 'q133' -o "$mtype" = 'q134'  ]; then
     rm -f diag2grads
     cp ${EXECcmon}/grads_lev.x ./diag2grads
     eval card=\${${mtype}_card}
  cat <<EOF >input
     &input
     intype='  q',stype='${mtype}',itype=$ctype,nreal=$nreal_q,nreal2=$nreal2_q,
     iscater=1,igrads=1,levcard='$card',intv=$hint,subtype='${subtype}',isubtype=${subtype},
/
EOF
  elif [ "$mtype" = 'q120' ]; then
     rm -f diag2grads
     cp ${EXECcmon}/grads_mandlev.x ./diag2grads
  cat <<EOF >input
     &input
     intype='  q',stype='${mtype}',itype=$ctype,nreal=$nreal_q,nreal2=$nreal2_q,
     iscater=1,igrads=1,subtype='${subtype}',isubtype=${subtype},
/
EOF
  elif [ "$mtype" = 'q180' -o "$mtype" = 'q181' -o "$mtype" = 'q183'  -o "$mtype" = 'q187' ]; then
     rm -f diag2grads
     cp ${EXECcmon}/grads_sfctime.x ./diag2grads
  cat <<EOF >input
     &input
     intype='  q',stype='${mtype}',itype=$ctype,nreal=$nreal_q,nreal2=$nreal2_q,
     iscater=1,igrads=1,timecard='time11',subtype='${subtype}',isubtype=${subtype},
/
EOF

  fi

  ./diag2grads <input>stdout 2>&1 


  rm -f *tmp
  mv stdout stdout_diag2grads_${mtype}_${subtype}.${cycle}

  dest_dir="${TANKDIR_cmon}/horz_hist/${cycle}"

  for file in q*grads; do
     mv ${file} ${dest_dir}/${file}.${PDATE}
  done

  for file in q*scater; do
     mv ${file} ${dest_dir}/${file}.${PDATE}
  done


echo "<-- diag2grad_q_case.sh"

exit
