#!/bin/sh
set -xa

#----------------------------------------------------------
#
#  diag2grad_uv_case.sh
#
#----------------------------------------------------------

echo "--> diag2grad_uv_case.sh"

   card=alllev

   nreal_uv=$nreal   
   nreal2_uv=`expr $nreal - 2`
   echo "nreal2_uv = ", ${nreal2_uv}
   echo "mtype     = ", ${mtype}

   run_exe=1
   ctype=`echo ${mtype} | cut -c3-5`

   echo "CONMON_NETCDF = ${CONMON_NETCDF}"
   netcdf=".false."
   run_exe=1

   if [ $CONMON_NETCDF -eq 1 ]; then
      netcdf=".true."
   fi
   echo "netcdf = $netcdf"


   if [ "$mtype" = 'uv221' -o "$mtype" = 'uv224' -o "$mtype" = 'uv229' -o "$mtype" = 'uv230' -o "$mtype" = 'uv231' -o "$mtype" = 'uv232' -o "$mtype" = 'uv233' -o "$mtype" = 'uv234' -o "$mtype" = 'uv235' -o "$mtype" = 'uv240' -o "$mtype" = 'uv241' -o "$mtype" = 'uv242' -o "$mtype" = 'uv243'  -o "$mtype" = 'uv245' -o "$mtype" = 'uv246' -o "$mtype" = 'uv247' -o "$mtype" = 'uv248' -o "$mtype" = 'uv249' -o "$mtype" = 'uv250' -o "$mtype" = 'uv251' -o "$mtype" = 'uv252' -o "$mtype" = 'uv253' -o "$mtype" = 'uv254' -o "$mtype" = 'uv255' -o "$mtype" = 'uv256' -o "$mtype" = 'uv257' -o "$mtype" = 'uv258' -o "$mtype" = 'uv259' -o "$mtype" = 'uv260' ]; then

      rm -f diag2grads
      cp ${EXECconmon}/conmon_grads_lev.x ./diag2grads

      rm -f input
      cat <<EOF >input
         &input
          input_file=${INPUT_FILE},
          intype=' uv',stype='${mtype}',itype=$ctype,nreal=$nreal_uv,
          iscater=1,igrads=1,levcard='$card',intv=$hint,subtype='${subtype}',isubtype=${subtype},
          netcdf=${netcdf}, run=${run},
/
EOF

   elif  [ "$mtype" = 'uv223' -o "$mtype" = 'uv224' -o "$mtype" = 'uv228' ]; then

      rm -f diag2grads
      cp ${EXECconmon}/conmon_grads_sig.x ./diag2grads
      rm -f input

      cat <<EOF >input
         &input
          input_file=${INPUT_FILE},
          intype=' uv',stype='${mtype}',itype=$ctype,nreal=$nreal_uv,
          iscater=1,igrads=1,subtype='${subtype}',isubtype=${subtype},
          netcdf=${netcdf}, run=${run},
/
EOF

   elif [ "$mtype" = 'uv220' ]; then

      rm -f diag2grads
      cp ${EXECconmon}/conmon_grads_mandlev.x ./diag2grads
      rm -f input
      cat <<EOF >input
         &input
          input_file=${INPUT_FILE},
          intype=' uv',stype='${mtype}',itype=$ctype,nreal=$nreal_uv,
          iscater=1,igrads=1,subtype='${subtype}',isubtype=${subtype},
          netcdf=${netcdf}, run=${run},
/
EOF

   elif [ "$mtype" = 'uv280' -o "$mtype" = 'uv281' -o "$mtype" = 'uv282' -o "$mtype" = 'uv284' -o "$mtype" = 'uv286' -o "$mtype" = 'uv287' -o "$mtype" = 'uv290' -o "$mtype" = 'uv291' -o "$mtype" = 'uv296' ]; then

      rm -f diag2grads
      cp ${EXECconmon}/conmon_grads_sfctime.x ./diag2grads
      rm -f input
      cat <<EOF >input
         &input
          input_file=${INPUT_FILE},
          intype=' uv',stype='${mtype}',itype=$ctype,nreal=$nreal_uv,
          iscater=1,igrads=1,timecard='time11',subtype='${subtype}',isubtype=${subtype},
          netcdf=${netcdf}, run=${run},
/
EOF

   elif [ "$mtype" = 'uv229' ]; then

      rm -f diag2grads
      cp ${EXECconmon}/conmon_grads_sfctime.x ./diag2grads
      rm -f input
      cat <<EOF >input
         &input
          input_file=${INPUT_FILE},
          intype=' uv',stype='${mtype}',itype=$ctype,nreal=$nreal_uv,
          iscater=1,igrads=1,timecard='time7',subtype='${subtype}',isubtype=${subtype},
          netcdf=${netcdf}, run=${run},
/
EOF

   else
      run_exe=0
   fi

   if [ $run_exe -eq 1 ]; then
      ./diag2grads <input>stdout 2>&1 

      rm -f *tmp

      mv stdout stdout_diag2grads_${mtype}_${subtype}.${run} 
      dest_dir="${TANKDIR_conmon}/horz_hist/${run}"

      grads_list=`ls uv*grads.${run}`
      for file in $grads_list; do
         ${COMPRESS} ${file}
         cp -f ${file}.${Z} ${dest_dir}/${file}.${PDATE}.${Z}
      done

      scater_list=`ls uv*scater.${run}`
      for file in $scater_list; do
         ${COMPRESS} ${file}
         cp -f ${file}.${Z} ${dest_dir}/${file}.${PDATE}.${Z}
      done

   else
      echo "aborting run, unmatched mtype ${mtype}"
   fi

echo "<-- diag2grad_uv_case.sh"

exit
