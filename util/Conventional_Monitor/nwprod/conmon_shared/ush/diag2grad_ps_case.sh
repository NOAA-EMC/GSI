#!/bin/sh
set -xa

#-------------------------------------------------------
#
#  diag2grad_ps_case.sh
#
#-------------------------------------------------------

   echo "-->  diag2grad_ps_case.sh"

   echo "CONMON_SUFFIX   = $CONMON_SUFFIX"
   echo "TANKDIR_conmon  = $TANKDIR_conmon"
   echo "type            = $type"
   echo "PDATE           = $PDATE"
   echo "EXECconmon      = $EXECconmon"
   echo "cycle           = $cycle"
   echo "nreal           = $nreal"
   echo "mtype           = $mtype (type = $type)"
   echo "subtype         = $subtype"
   echo "hint            = $hint"
   echo "workdir         = $workdir"
   echo "INPUT_FILE    = ${INPUT_FILE}"


   echo "CONMON_NETCDF = ${CONMON_NETCDF}"
   netcdf=".false."
   run_exe=1   

   if [ $CONMON_NETCDF -eq 1 ]; then
      netcdf=".true."
   fi
   echo "netcdf = $netcdf"


   ctype=`echo ${mtype} | cut -c3-5`
   nreal_ps=$nreal                 ### one less than the data items of diagnostic files
   nreal2_ps=`expr $nreal - 2`     ### the data items in the grads files 


   if [ "$mtype" = 'ps180' -o "$mtype" = 'ps181' -o  "$mtype" = 'ps183' -o  "$mtype" = 'ps187' ]; then
      rm -f diag2grads
      cp $EXECconmon/conmon_grads_sfctime.x ./diag2grads
      rm -f input

      cat <<EOF >input
         &input
         input_file=${INPUT_FILE},intype=' ps', stype='${mtype}',
         itype=$ctype, nreal=$nreal_ps, iscater=1, igrads=1,
         timecard='time11', subtype='${subtype}', isubtype=${subtype},
         netcdf=${netcdf}, run=${run},
/
EOF
   elif [ "$mtype" = 'ps120' ]; then
      rm -f diag2grads
      cp ${EXECconmon}/conmon_grads_sfc.x ./diag2grads
      rm -f input

      cat <<EOF >input
         &input
          input_file=${INPUT_FILE},intype=' ps',stype='${mtype}',itype=$ctype,nreal=$nreal_ps,
          iscater=1,igrads=1,subtype='${subtype}',isubtype=${subtype},netcdf=${netcdf},
          netcdf=${netcdf}, run=${run},
/
EOF
   else
      run_exe=0
   fi

   if [ $run_exe -eq 1 ]; then
      ./diag2grads <input>stdout 2>&1 

      rm -f ${mtype}_${subtype}.tmp


      mv stdout stdout_diag2grads_${mtype}_${subtype}.$run
      dest_dir="${TANKDIR_conmon}/horz_hist/${run}"

      grads_list=`ls ps*grads.${run}`
      for file in $grads_list; do 
         ${COMPRESS} ${file}
         cp -f ${file}.${Z} ${dest_dir}/${file}.${PDATE}.${Z}
      done

      scater_list=`ls ps*scater.${run}`
      for file in $scater_list; do
         ${COMPRESS} ${file}
         cp -f ${file}.${Z} ${dest_dir}/${file}.${PDATE}.${Z}
      done

   else
      echo "abort run, unmatched mtype ${mtype} " 
   fi

   echo "<--  diag2grad_ps_case.sh"

exit
