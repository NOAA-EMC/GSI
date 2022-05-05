#!/bin/bash

#--------------------------------------------------------------------
#  radmon_copy.sh
#
#    This script is run as a submitted job by RadMon_CP_glbl.sh and
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
echo "--> radmon_copy.sh"
echo ""
echo "RADMON_SUFFIX     = $RADMON_SUFFIX"
echo "PDATE             = $PDATE"
echo "RUN               = $RUN"
echo "DATA_LOCATION     = ${DATA_LOCATION}" 

exit_value=0
monitor=radmon

set -ax

prev=`$NDATE -06 $PDATE`
prev_day=`echo $prev|cut -c1-8`
prev_cyc=`echo $prev|cut -c9-10`

echo prev_day, prev_cyc = $prev_day, $prev_cyc

dest_dir=${TANKverf}/${RUN}.${PDY}/${CYC}/radmon
echo "dest_dir = ${dest_dir}"
satype_file=${TANKverf}/info/${RUN}_radmon_satype.txt

if [[ ! -s ${satype_file} ]]; then
   satype_file=${FIXgdas}/gdas_radmon_satype.txt
fi

if [[ ! -s ${satype_file} ]]; then
   echo "WARNING:  unable to locate ${satype_file}"
fi


#---------------------------------------------------
#  Check the number of files available to copy
#  and the number of files < 30 min old.  Abort the
#  copy if any files are < 30 min old.  This avoids
#  incomplete results which can result in false
#  'drop out' plots.
# 
nfile_src=`ls -l ${DATA_LOCATION}/*${PDATE}*ieee_d* | egrep -c '^-'`
echo "nfile_src = ${nfile_src}"

nfile_thirty=`find ${DATA_LOCATION}/*${PDATE}*ieee_d* -maxdepth 0 -mmin -30`
echo "nfile_thirty = ${nfile_thirty}"

if [[ ${nfile_src} -le 0 ]]; then
   exit_value=5
elif [[ ${nfile_thirty} != "" ]]; then
   exit_value=4
fi

if [[ ${exit_value} -eq 0 ]]; then
   if [[ ! -d ${dest_dir} ]]; then
      mkdir -p ${dest_dir}
   fi
   cd ${dest_dir}

   type_list="angle bcoef bcor time"

   for type in ${type_list}; do 

      file_list=`ls ${DATA_LOCATION}/${type}.*${PDATE}*ieee_d* ${DATA_LOCATION}/${type}*tar* `

      for file in ${file_list}; do
         bfile=`basename ${file}`
         echo "testing ${file}"

         echo "target = ${dest_dir}/${bfile}"  
         if [[ ! -e ${dest_dir}/${bfile} ]]; then
            echo "copying file" 
            ${NCP} ${file} ${dest_dir}/${bfile}
         fi
      done
   done

   $NCP ${DATA_LOCATION}/*.ctl* ${dest_dir}/.


   if [[ $DO_DATA_RPT -eq 1 ]]; then


      #-------------------------------------------------
      #  run validate.sh
      #
      $NCP ${DE_EXEC}/radmon_validate_tm.x ${dest_dir}/.
      $NCP $DE_SCRIPTS/validate.sh    ${dest_dir}/.
      echo "firing validate.sh"

      ./validate.sh ${PDATE}

      valid_tar=stdout.validate.tar

      if [[ $CYC -eq "00" ]]; then
         tar -cvf ${valid_tar} stdout.validate.*.00 
      else
         tar -rvf ${valid_tar} stdout.validate.*.${CYC}
      fi

      rm -f stdout.validate.*.${CYC}
  
      ls -la ./${valid_tar} 
   fi


   if [[ ${CLEAN_TANKVERF} -eq 1 ]]; then
      days_to_keep=60
      ${HOMEradmon}/ush/clean_tankdir.sh ${RAD_AREA} ${days_to_keep}
   fi

fi

warn_msg="warning.${PDATE}"
warn_msg2="warning2.${PDATE}"

echo "exit_value now = ${exit_value}"
if [[ $exit_value == 0 ]]; then

   #--------------------------------------------------------------------
   #  Tar up the stdout.validation files 
   #--------------------------------------------------------------------
   if [[ $DO_DATA_RPT -eq 1 ]]; then
      echo "begin DO_DATA_RPT"

      #--------------------------------------------------------------------
      #  Remove extra spaces in new bad_pen and low_count files
      #--------------------------------------------------------------------
      bad_pen=bad_pen.${PDATE}
      gawk '{$1=$1}1' $bad_pen > tmp.bad_pen
      mv -f tmp.bad_pen $bad_pen

      low_count=low_count.${PDATE}
      gawk '{$1=$1}1' $low_count > tmp.low_count
      mv -f tmp.low_count $low_count
         
 
      #--------------------------------------------------------------------
      #  Diag report processing
      #
      #  New algorithm:
      #
      #     1.  locate satype and radstat files, specify output file
      #     2.  run new radmon_diag_ck.sh script
      #     3.  build diag report from output file
      #     4.  move output file to target tankdir
      #--------------------------------------------------------------------
      radstat=${radstat:-${RADSTAT_LOCATION}/${RUN}.t${CYC}z.radstat}
      diag_out="bad_diag.${PDATE}"

      if [[ ! -e ${satype_file} ]]; then
         echo "MISSING satype_file: ${satype_file}"
      else
         echo "satype_file is good to go:  ${satype_file}"
      fi

      if [[ ! -e ${radstat} ]]; then
         echo "MISSING radstat_file: ${radstat}"
      else
         echo "radstat is good to go: ${radstat}"
      fi


      if [[ -e ${satype_file} && -e ${radstat} ]]; then
         echo "satype  = $satype_file"
         echo "radstat = $radstat"

	 ${DE_SCRIPTS}/radmon_diag_ck.sh --rad ${radstat} --sat ${satype_file} --out ${diag_out}
         if [[ -e ${diag_out} ]]; then
            $NCP ./${diag_out} ${TANKverf}/${RUN}.${day}/${cyc}/radmon/.
         fi
      fi


      #--------------------------------------------------------------------
      #  Create a new penalty error report using the new bad_pen file
      #--------------------------------------------------------------------
      $NCP $DE_SCRIPTS/radmon_err_rpt.sh      ${dest_dir}/.

      prev_bad_pen=${TANKverf}/${RUN}.${prev_day}/${prev_cyc}/${monitor}/bad_pen.${prev}
      prev_low_count=${TANKverf}/${RUN}.${prev_day}/${prev_cyc}/${monitor}/low_count.${prev}

      bad_pen=bad_pen.${PDATE}
      diag_rpt="diag.txt"
      bad_pen_rpt="pen.${PDATE}.txt"
      err_rpt="err.${PDATE}.txt"
      low_obs_rpt="obs.${PDATE}.txt"

      ./radmon_err_rpt.sh $prev_bad_pen $bad_pen pen ${prev} ${PDATE} $diag_out $bad_pen_rpt

      ./radmon_err_rpt.sh $prev_low_count $low_count cnt ${prev} ${PDATE} $diag_out $low_obs_rpt



      #--------------------------
      #  Build the $warn_msg file
      #
      if [[ -s $bad_pen_rpt || -s $low_obs_rpt || -s ${diag_out} ]]; then

         if [[ -s $bad_pen_rpt ]]; then
            args="${args} --pen ${bad_pen_rpt}"
         fi

         if [[ -s $low_obs_rpt ]]; then
            args="${args} --obs ${low_obs_rpt}"
         fi

         if [[ -s $diag_out ]]; then
            args="${args} --diag ${diag_out}"
         fi

         ${DE_SCRIPTS}/radmon_mk_warning.sh ${args} --out ${warn_msg}

      fi

      $COMPRESS *.ctl

   fi

   #--------------------------------------------------------------------
   # Remove processing scripts/executables and exit.
   #--------------------------------------------------------------------
   rm -f radmon_validate_tm.x
   rm -f validate.sh 
   rm -f radmon_err_rpt.sh  

   nfile_dest=`ls -l ${dest_dir}/*${PDATE}*ieee_d* | egrep -c '^-'`

   if [[ exit_value -eq 0 && $nfile_src -ne $nfile_dest ]]; then
      exit_value=6 
   fi

fi

echo ""
echo "<-- radmon_copy.sh"
echo ""
exit ${exit_value}

