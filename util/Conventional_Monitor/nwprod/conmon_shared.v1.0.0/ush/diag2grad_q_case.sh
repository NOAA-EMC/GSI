#!/bin/sh
set -xa

#------------------------------------------------------------
#
#  diag2grad_q_case.sh
#
#------------------------------------------------------------

echo "--> diag2grad_q_case.sh"

   echo "CONMON_SUFFIX   = $CONMON_SUFFIX"
   echo "TANKDIR_conmon  = $TANKDIR_conmon"
   echo "type          = $type"
   echo "PDATE         = $PDATE"
   echo "EXECconmon      = $EXECconmon"
   echo "cycle         = $cycle"
   echo "run           = $run  "
   echo "nreal         = $nreal"
   echo "mtype         = $mtype (type = $type)"
   echo "subtype       = $subtype"
   echo "hint          = $hint"
   echo "workdir       = $workdir"
   echo "INPUT_FILE    = ${INPUT_FILE}"

  nreal_q=$nreal                 ### one less than the data items of diagnostic files
  nreal2_q=`expr $nreal - 2`     ### the data items in the grads files 

  echo "CONMON_NETCDF = ${CONMON_NETCDF}"

  netcdf=.false.
  if [[ ${CONMON_NETCDF} -eq 1 ]]; then
    netcdf=.true.
  end if

  ctype=`echo ${mtype} | cut -c2-4`

  if [ "$mtype" = 'q130' -o "$mtype" = 'q131' -o "$mtype" = 'q132' -o "$mtype" = 'q133' -o "$mtype" = 'q134' -o "$mtype" = 'q135' ]; then
     rm -f diag2grads
     cp ${EXECconmon}/conmon_grads_lev.x ./diag2grads
  cat <<EOF >input
     &input
     input_file=${INPUT_FILE},
     intype='  q',stype='${mtype}',itype=$ctype,nreal=$nreal_q,
     iscater=1,igrads=1,levcard='alllev',
     intv=$hint,subtype='${subtype}',isubtype=${subtype},
     netcdf=${netcdf},
/
EOF
  elif [ "$mtype" = 'q120' ]; then
     rm -f diag2grads
     cp ${EXECconmon}/conmon_grads_mandlev.x ./diag2grads
  cat <<EOF >input
     &input
     input_file=${INPUT_FILE},
     intype='  q',stype='${mtype}',itype=$ctype,nreal=$nreal_q,
     iscater=1,igrads=1,subtype='${subtype}',isubtype=${subtype},
     netcdf=${netcdf},
/
EOF
  elif [ "$mtype" = 'q180' -o "$mtype" = 'q181' -o "$mtype" = 'q182' -o "$mtype" = 'q183'  -o "$mtype" = 'q187' ]; then
     rm -f diag2grads
     cp ${EXECconmon}/conmon_grads_sfctime.x ./diag2grads
  cat <<EOF >input
     &input
     input_file=${INPUT_FILE},
     intype='  q',stype='${mtype}',itype=$ctype,nreal=$nreal_q,
     iscater=1,igrads=1,timecard='time11',subtype='${subtype}',isubtype=${subtype},
     netcdf=${netcdf},
/
EOF

  fi

  ./diag2grads <input>stdout 2>&1 


  rm -f *tmp
  mv stdout stdout_diag2grads_${mtype}_${subtype}.${run}

  dest_dir="${TANKDIR_conmon}/horz_hist/${run}"

  for file in q*grads; do
     cp ${file} ${dest_dir}/${file}.${PDATE}
  done

  for file in q*scater; do
     cp ${file} ${dest_dir}/${file}.${PDATE}
  done


echo "<-- diag2grad_q_case.sh"

exit
