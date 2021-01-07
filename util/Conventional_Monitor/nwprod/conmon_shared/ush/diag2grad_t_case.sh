#!/bin/sh
set -xa

#-----------------------------------------------------
#  
#  diag2grad_t_case.sh
#
#-----------------------------------------------------

echo "--> diag2grad_t_case.sh"

   echo "CONMON_SUFFIX   = $CONMON_SUFFIX"
   echo "TANKDIR_conmon  = $TANKDIR_conmon"
   echo "type          = $type"
   echo "PDATE         = $PDATE"
   echo "EXECconmon      = $EXECconmon"
   echo "cycle         = $cycle"
   echo "nreal         = $nreal"
   echo "mtype         = $mtype (type = $type)"
   echo "subtype       = $subtype"
   echo "hint          = $hint"
   echo "workdir       = $workdir"
   echo "INPUT_FILE    = ${INPUT_FILE}"

   echo "CONMON_NETCDF = ${CONMON_NETCDF}"
   netcdf=".false."

   if [ $CONMON_NETCDF -eq 1 ]; then
      netcdf=".true."
   fi
   echo "netcdf = $netcdf"


   ctype=`echo ${mtype} | cut -c2-4`
   nreal2=`expr $nreal - 2`
   if [[ $VERBOSE = "YES" ]]; then
      echo ctype, nreal2 = $ctype, nreal2
   fi

   card=alllev
   run_exe=1

   if [[ -e ./diag2grads ]]; then 
      rm -f ./diag2grads
   fi

   if [ "$mtype" = 't130' -o "$mtype" = 't131' -o "$mtype" = 't132' -o "$mtype" = 't133' -o "$mtype" = 't134' -o "$mtype" = 't135' ]; then
      cp $EXECconmon/conmon_grads_lev.x ./diag2grads

      cat <<EOF >input
         &input
         input_file=${INPUT_FILE},
         intype='  t',stype='${mtype}',itype=$ctype,nreal=$nreal,
         iscater=1,igrads=1,levcard='$card',intv=$hint,subtype='${subtype}',isubtype=${subtype},
         netcdf=${netcdf}, run=${run},
         /
EOF
  
   elif [ "$mtype" = 't120' ]; then
      cp $EXECconmon/conmon_grads_mandlev.x ./diag2grads

      cat <<EOF >input
         &input
         input_file=${INPUT_FILE},
         intype='  t',stype='${mtype}',itype=$ctype,nreal=$nreal,
         iscater=1,igrads=1,subtype='${subtype}',isubtype=${subtype},
         netcdf=${netcdf}, run=${run},
         /
EOF

   elif [ "$mtype" = 't180' -o "$mtype" = 't181' -o "$mtype" = 't182' -o "$mtype" = 't183'  -o "$mtype" = 't187' ]; then
      cp $EXECconmon/conmon_grads_sfc.x ./diag2grads
      cat <<EOF >input
         &input
         input_file=${INPUT_FILE},
         intype='  t',stype='${mtype}',itype=$ctype,nreal=$nreal,
         iscater=1,igrads=1,subtype='${subtype}',isubtype=${subtype},
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

      grads_list=`ls t*grads.${run}`
      for file in $grads_list; do
         ${COMPRESS} ${file}
         cp -f  ${file}.${Z} ${dest_dir}/${file}.${PDATE}.${Z}
      done

      scater_list=`ls t*scater.${run}`
      for file in $scater_list; do
         ${COMPRESS} ${file}
         cp -f ${file}.${Z} ${dest_dir}/${file}.${PDATE}.${Z}
      done

   else
      echo "aborting run, unmatched mtype ${mtype}"
   fi


echo "<-- diag2grad_t_case.sh"
exit
