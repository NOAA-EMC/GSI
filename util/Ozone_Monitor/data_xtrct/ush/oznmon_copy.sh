#!/bin/bash

#--------------------------------------------------------------------
#  oznmon_copy.sh
#
#    This script is run as a submitted job by OznMon_CP.sh and
#    should not be run directly.
#
#    This script searches for new radmon output and copies those 
#    filess to the user's $TANKDIR directory under the specified 
#    suffix argument. 
#
#    The bad_penalty, low count, and missing diag reports are 
#    reevaluated using local copies of the base file and satype
#    files in the $TANKdir/$suffix/info directory. 
#    
#    Note that processing occurs within TANKdir, not in stmp space.
#
#    The unified error report is journaled to warning.${PDY}${CYC}.
#
#--------------------------------------------------------------------


echo ""
echo "--> oznmon_copy.sh"
echo ""
echo "OZNMON_SUFFIX     = $OZNMON_SUFFIX"
echo "PDATE             = $PDATE"
echo "RUN               = $RUN"
echo "DATA_LOCATION     = ${DATA_LOCATION}" 

exit_value=0
monitor=oznmon

set -ax

prev=`$NDATE -06 $PDATE`
prev_day=`echo $prev|cut -c1-8`
prev_cyc=`echo $prev|cut -c9-10`

echo prev_day, prev_cyc = $prev_day, $prev_cyc

dest_dir=${OZN_STATS_TANKDIR}/${RUN}.${PDY}/${CYC}/oznmon
echo "dest_dir = ${dest_dir}"
satype_file=${OZN_STATS_TANKDIR}/info/${RUN}_oznmon_satype.txt

if [[ ! -s ${satype_file} ]]; then
   satype_file=${FIXgdas_ozn}/gdas_oznmon_satype.txt
fi

if [[ ! -s ${satype_file} ]]; then
   echo "WARNING:  unable to locate ${satype_file}"
else
   satype_list=`cat ${satype_file}`
fi


#---------------------------------------------------
#  Check the number of files available to copy
#  and the number of files < 30 min old.  Abort the
#  copy if any files are < 30 min old.  This avoids
#  incomplete results which can result in false
#  'drop out' plots.
# 

subdir_list="horiz time"
for sub in ${subdir_list}; do
   nfile_src=`ls -l ${DATA_LOCATION}/${sub}/*${PDATE}*ieee_d* | egrep -c '^-'`
   echo "nfile_src = ${nfile_src}"

   nfile_thirty=`find ${DATA_LOCATION}/${sub}/*${PDATE}*ieee_d* -maxdepth 0 -mmin -30`
   echo "nfile_thirty = ${nfile_thirty}"

   if [[ ${nfile_src} -le 0 ]]; then
      exit_value=5
      exit 
   elif [[ ${nfile_thirty} != "" ]]; then
      exit_value=4
      exit
   fi
done

if [[ ${exit_value} -eq 0 ]]; then
   if [[ ! -d ${dest_dir} ]]; then
      mkdir -p ${dest_dir}
   fi
   cd ${dest_dir}


   for sub in ${subdir_list}; do 
     
      mkdir -p ${dest_dir}/${sub} 

      $NCP ${DATA_LOCATION}/${sub}/*${PDATE}.ieee_d* ${dest_dir}/${sub}/.
      $NCP ${DATA_LOCATION}/${sub}/*.ctl* ${dest_dir}/${sub}/.
      if [[ ${sub} = "time" ]]; then
         $NCP ${DATA_LOCATION}/${sub}/bad*${PDATE}* ${dest_dir}/${sub}/.
      fi
   done


   if [[ $DO_DATA_RPT -eq 1 && ${#satype_list} -gt 0 ]]; then

      #-------------------------------------------------------------------
      # re-run the bad_diag report 
      #    1.  Rm any existing bad_diag file.      
      #    2.  Get the contents of the SATYPE list in this order:
      #           $OZN_TANKdir/$SUFFIX/info/gdas_oznmon_satype.txt
      #           $GDAS_OZNMON/fix/gdas/oznmon_satype.txt
      #    3.  Get contents of oznstat file & strip to sat/instr.
      #    4.  Compare satype list to oznstat contents; report missing.
      #
      bad_diag=${dest_dir}/time/bad_diag.${PDATE}
      if [[ -e ${bad_diag} ]]; then
         rm ${bad_diag}
      fi

      oznstat_files=`tar -tf ${OZNSTAT}`
      #-------------------------------------------------------------------------
      # Diag files are in this format: 
      #     diag_gome_metop-a_ges.2021040506.nc4.gz
      #     diag_gome_metop-a_anl.2021040506.nc4.gz
      #
      # Select only the 'ges' files and reduce them to:
      #     gome_metop-a
      #
      for file in ${oznstat_files}; do
         if [[ "${file}" == *"ges"* ]]; then
            sat=`echo ${file} | cut -d. -f1`
            sat="$( cut -d '_' -f2- <<< "${sat}" )";
            sat=`echo ${sat} | rev | cut -d"_" -f2-  | rev`

            oznstat_list="${oznstat_list} ${sat}" 
         fi
      done     


      if [[ ${#oznstat_list} -gt 0 ]]; then

         diag_rpt="diag_rpt.txt"
         echo '' > ${diag_rpt}

         for sat in ${satype_list}; do
            test=`echo ${oznstat_list} | grep ${sat}`
            echo "test length = ${#test}"

            if [[  "${#test}" -eq 0 ]]; then
               echo " missing diag file -- diag_${sat}.${PDATE} not found" >> ${diag_rpt}
            fi
         done

         diag_test=`cat ${diag_rpt}`
         if [[ ${#diag_test} -gt 0 ]]; then
            mv ${diag_rpt} ${bad_diag}
         fi     

         if [[ -e ${diag_rpt} ]]; then
            rm ${diag_rpt}
         fi
      fi

   fi	  # DO_DATA_REPORT


fi   # exit value != 0

echo ""
echo "<-- oznmon_copy.sh"
echo ""
exit ${exit_value}

