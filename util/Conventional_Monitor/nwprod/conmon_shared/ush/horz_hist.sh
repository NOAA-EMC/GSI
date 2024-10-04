#!/bin/sh

#------------------------------------------------------------------
#
#  horz_hist.sh
#
#------------------------------------------------------------------

   set -ax

   echo "--> horz_hist.sh"
   date
   rc=0

   echo "CONMON_SUFFIX = $CONMON_SUFFIX"
   export hint=10    ##(mb) the plot pressure interval press+-hint

   #----------------------------------------------------------
   # The list of data type, based on convinfo.txt file
   #----------------------------------------------------------
   ps_TYPE=`${USHconmon}/get_typelist.pl --file $convinfo --type ps --mon`
   q_TYPE=`${USHconmon}/get_typelist.pl --file $convinfo --type q --mon`
      
   t_TYPE=`${USHconmon}/get_typelist.pl --file $convinfo --type t --mon`
   uv_TYPE=`${USHconmon}/get_typelist.pl --file $convinfo --type uv --mon`


   echo TANKDIR_conmon = $TANKDIR_conmon

   mkdir -p ${TANKDIR_conmon}/horz_hist/ges
   mkdir -p ${TANKDIR_conmon}/horz_hist/anl

   export nreal_ps=${nreal_ps:-19}
   export nreal_q=${nreal_q:-20} 
   export nreal_t=${nreal_t:-24} 
   export nreal_uv=${nreal_uv:-23} 


   for type in ps q t uv; do

      eval stype=\${${type}_TYPE}
      eval nreal=\${nreal_${type}}
      exec=read_${type}

      #---------------------------------
      #  decoding the dignostic file
      #---------------------------------

      for dtype in ${stype}; do

         mtype=`echo ${dtype} | cut -f1 -d_ | xargs`
         subtype=`echo ${dtype} | cut -f2 -d_ | xargs`

         if [[ "$VERBOSE" = "YES" ]]; then
            echo "DEBUG:  dtype = $dtype"
            echo "mtype, subtype = $mtype, $subtype"
         fi

         for run in ges anl; do

            #-------------------------------------------------------------
            #  Because few things in life are convenient or consistent, 
            #  the cnvstat has different contents depending on file type.
            #
            #  Binary cnvstat files contain 2 diag files (ges and anl), 
            #  while NetCDF cnvstat files contain a diag file for each 
            #  data type (ps, q, t, etc) as well as run (ges, anl).
            #-------------------------------------------------------------
            if [[ $CONMON_NETCDF -eq 0 ]]; then 
               export INPUT_FILE="diag_conv_${run}.${PDATE}"
            else 
               export INPUT_FILE="diag_conv_${type}_${run}.${PDATE}"
            fi
            echo "INPUT_FILE =  ${INPUT_FILE}"


            if [[ "$VERBOSE" = "YES" ]]; then
               echo "run = $run"
            fi 

            ${USHconmon}/diag2grad_${type}_case.sh

         done    #### done with run
         
      done   ### done with dtype

   done   ### done with type


   #---------------------------------------
   #  tar, compress, and move stdout files
   #---------------------------------------
   for run in ges anl; do
      stdout_tar=stdout.${PDATE}.${run}.tar
      tar -cvf ${stdout_tar} stdout*.${run}
      ${COMPRESS} ${stdout_tar}
            
      dest_dir=${TANKDIR_conmon}/horz_hist/${run}
      mv -f ${stdout_tar}.${Z} ${dest_dir}/.

      cat *nobs.${run} > nobs.${run}.${PDATE}
      cp nobs.${run}.${PDATE} ${dest_dir}/.
  
      #--------------------------------- 
      #  run the mk_low_cnt.pl script 
      #--------------------------------- 
      ${USHconmon}/mk_low_cnt.pl --net ${CONMON_SUFFIX} \
             --run ${RUN}  --cyc ${PDATE} \
             --nobsf ${TANKDIR_conmon}/horz_hist/${run}/nobs.${run}.${PDATE} \
             --lcntf ${TANKDIR_conmon}/horz_hist/${run}/low_cnt.${run}.${PDATE} \
             --basef ${conmon_base}


      #--------------------------------
      #  run the mk_err_rpt.pl script
      #--------------------------------
      low_cnt_file=${TANKDIR_conmon}/horz_hist/${run}/low_cnt.${run}.${PDATE}
      if [[ -e ${low_cnt_file}.gz ]]; then
         $UNCOMPRESS ${low_cnt_file}.gz
      fi

      prev_low_cnt_file=${TANKDIR_prev_conmon}/horz_hist/${run}/low_cnt.${run}.${GDATE}
      if [[ -e ${prev_low_cnt_file}.gz ]]; then
         $UNCOMPRESS ${prev_low_cnt_file}.gz
      fi

      ${USHconmon}/mk_err_rpt.pl --net ${CONMON_SUFFIX} --run ${RUN} \
             --lcf ${low_cnt_file} --plcf ${prev_low_cnt_file} \
             --cyc0 ${PDATE} --cyc1 ${GDATE} \
             --errf ${TANKDIR_conmon}/horz_hist/${run}/err_rpt.${run}.${PDATE}
   done

echo "<-- horz_hist.sh"

exit ${rc}

