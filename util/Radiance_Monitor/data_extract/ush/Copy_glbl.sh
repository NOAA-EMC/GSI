#!/bin/sh

#--------------------------------------------------------------------
#  Copy_glbl.sh
#
#    This script searches for new radmon output from the global GDAS
#    and copies those filess to the user's $TANKDIR directory under the
#    specified suffix argument. 
#    The bad_penalty files are regenerated using the local copy of the 
#    base file.
#    
#--------------------------------------------------------------------

function usage {
  echo "Usage:  Copy_glbl.sh suffix date"
  echo "            File name for Copy_glbl.sh may be full or relative path"
  echo "            Suffix is the indentifier for this data source, and should"
  echo "             correspond to an entry in the ../../parm/data_map file."
  echo "            DATE is 10 digit yyyymmddhh string."
}

set -ax
echo start Copy_glbl.sh
exit_value=0

nargs=$#
if [[ $nargs -ne 2 ]]; then
   usage
   exit 1
fi

this_file=`basename $0`
this_dir=`dirname $0`
compress="/usrx/local/bin/pigz -f"
no_diag_rpt=0
no_error_rpt=0

export SUFFIX=$1
export DATE=$2

export RAD_AREA=glb

echo SUFFIX = $SUFFIX
echo DATE   = $DATE


#--------------------------------------------------------------------
# Set environment variables
#--------------------------------------------------------------------

top_parm=${this_dir}/../../parm
export RADMON_VERSION=${RADMON_VERSION:-${top_parm}/radmon.ver}
if [[ -s ${RADMON_VERSION} ]]; then
   . ${RADMON_VERSION}
else
   echo "Unable to source ${RADMON_VERSION} (radmon version) file"
   exit 2
fi

export RADMON_CONFIG=${RADMON_CONFIG:-${top_parm}/RadMon_config}
if [[ -s ${RADMON_CONFIG} ]]; then
   . ${RADMON_CONFIG}
else
   echo "Unable to source ${RADMON_CONFIG} (radmon config) file"
   exit 2
fi

if [[ -s ${RADMON_USER_SETTINGS} ]]; then
   . ${RADMON_USER_SETTINGS}
else
   echo "Unable to source ${RADMON_USER_SETTINGS} (radmon user settings) file"
   exit 3
fi

. ${DE_PARM}/data_extract_config
export USHradmon=${USHradmon:-$HOMEradmon/ush}


#--------------------------------------------------------------------
#  Check setting of RUN_ONLY_ON_DEV and possible abort if on prod and
#  not permitted to run there.
#--------------------------------------------------------------------

if [[ RUN_ONLY_ON_DEV -eq 1 ]]; then
   is_prod=`${DE_SCRIPTS}/onprod.sh`
   if [[ $is_prod = 1 ]]; then
      exit 10
   fi
fi


#---------------------------------------------------------------
# Create any missing directories.
#---------------------------------------------------------------
mkdir -p $TANKverf
mkdir -p $LOGdir

day=`echo $DATE|cut -c1-8`
cycle=`echo $DATE|cut -c9-10`
echo day  = $day

PDATE=${DATE}
echo PDATE = $PDATE

prev=`$NDATE -06 $PDATE`
prev_day=`echo $prev|cut -c1-8`
prev_cyc=`echo $prev|cut -c9-10`
next=`$NDATE +06 $PDATE`
next_day=`echo $next|cut -c1-8`
next_cyc=`echo $next|cut -c9-10`

echo prev_day, prev_cyc = $prev_day, $prev_cyc
echo next_day, next_cyc = $next_day, $next_cyc


DATDIR=${DATDIR:-/com/verf/prod/radmon.${day}}

test_dir=${TANKverf}/radmon.${day}

if [[ ! -d ${test_dir} ]]; then
   mkdir -p ${test_dir}
fi
cd ${test_dir}

if [[ ! -s gdas_radmon_satype.txt  ]]; then
   if [[ -s ${TANKverf}/radmon.${prev_day}/gdas_radmon_satype.txt ]]; then
      $NCP ${TANKverf}/radmon.${prev_day}/gdas_radmon_satype.txt .
   else
      echo WARNING:  unable to locate gdas_radmon_satype.txt in ${TANKverf}/radmon.${prev_day}
   fi 
fi


nfile_src=`ls -l ${DATDIR}/*${PDATE}*ieee_d* | egrep -c '^-'`

if [[ $nfile_src -gt 0 ]]; then
   type_list="angle bcoef bcor time"

   for type in ${type_list}; do 

      file_list=`ls ${DATDIR}/${type}.*${PDATE}*ieee_d* `


      for file in ${file_list}; do
         bfile=`basename ${file}`
         echo "testing ${file}"
 
         if [[ ! -e ${test_dir}/${bfile} ]]; then
            echo "copying file" 
            ${NCP} ${file} ${test_dir}/${bfile}
         fi
      done
   done

   $NCP ${DATDIR}/*.ctl* ${test_dir}/.


#  run validate.sh
#    1.  copy validate.sh and validate_data.x locally
#    2.  run validate.sh 
#    3.  clean up
#           rm validate.sh, validate_data.x
#           rm stdout files?
#           make sure *.base and *.tar are removed

   $NCP ${DE_EXEC}/validate_time.x ${test_dir}/.
   $NCP $DE_SCRIPTS/validate.sh      ${test_dir}/.
   ./validate.sh ${PDATE}

else
   exit_value=5
fi


if [[ $exit_value == 0 ]]; then
   #--------------------------------------------------------------------
   #  Tar up the stdout.validation files 
   #--------------------------------------------------------------------
   valid_tar=stdout.validate.tar

   if [[ $cycle -eq "00" ]]; then
      tar -cvf ${valid_tar} stdout.validate.*.00 
   else
      tar -rvf ${valid_tar} stdout.validate.*.${cycle}
   fi

   #--------------------------------------------------------------------
   #  Remove extra spaces in new bad_pen file
   #--------------------------------------------------------------------
   bad_pen=bad_pen.${PDATE}
   gawk '{$1=$1}1' $bad_pen > tmp.bad_pen
   mv -f tmp.bad_pen $bad_pen
    
   #--------------------------------------------------------------------
   #  Create a new penalty error report using the new bad_pen file
   #--------------------------------------------------------------------
   $NCP $DE_SCRIPTS/radmon_err_rpt.sh      ${test_dir}/.
   $NCP $HOMEradmon/ush/radmon_getchgrp.pl ${test_dir}/.

   prev_bad_pen=${TANKverf}/radmon.${prev_day}/bad_pen.${prev}
   bad_pen=bad_pen.${PDATE}
   diag_rpt="diag.txt"
   outfile="pen.${PDATE}.txt"

   ./radmon_err_rpt.sh $prev_bad_pen $bad_pen pen ${prev} ${PDATE} $diag_rpt $outfile

 
   #--------------------------------------------------------------------
   #  Copy over the /com/output/prod/YYYYMMDD/gdas_verfrad_HH.o* log 
   #   Note that the 18z cycle log file is found in the next day's 
   #   directory.
   #    1.  Confirm that any entries in the Diagnostic file report 
   #        are in the satype table.  Remove any entries that are not
   #        valid and the entire report if none are valid.
   #    2.  Remove the existing bad penalty report
   #    3.  Add the output from the new penalty error report (if present)
   #        otherwise remove the entier penalty report.
   #    4.  put log in the /ptmp/logs/radopr/data_extract logs dir
   #--------------------------------------------------------------------
   opr_log=opr_${PDATE}.log
   tmp_log=tmp_${PDATE}.log
   new_log=new_opr_${PDATE}.log
   if [[ $cycle = 18 ]]; then 
      $NCP /com/output/prod/${next_day}/gdas_verfrad_${cycle}.o* ${opr_log}
   else
     $NCP /com/output/prod/${day}/gdas_verfrad_${cycle}.o* ${opr_log}
   fi

   #--------------------------------------------------------------------
   #  Diag report processing
   #--------------------------------------------------------------------
   tmp_diag="diag.tmp"
   new_diag="diag.new"
   opr_log_start=1

   start=`grep -n 'cat diag_report.txt' ${opr_log}`
   diag_start=`echo $start | sed 's/:/ /g' | gawk '{print $1}'`
   echo diag_start = $diag_start

   if [[ $diag_start -gt 1 ]]; then
      end=`grep -nx 'End Problem Reading Diagnostic File' ${opr_log}`
      diag_end=`echo $end | sed 's/:/ /g' | gawk '{print $1}'` 
      echo diag_end = $diag_end

      gawk "NR>=$diag_start && NR<=$diag_end" ${opr_log} >> $tmp_diag

      while read line; do
         new_sat=`echo $line | grep PROBLEM`
         len=`expr length "$new_sat"`
         if [[ $len -gt 0 ]]; then
            sat=`echo $new_sat | gawk '{print $1}'`
         
            test_satype=`grep $sat gdas_radmon_satype.txt`
            len_test=`expr length "$test_satype"`
            if [[ $len_test -gt 0 ]]; then
               echo $line >> $new_diag
            fi
         else
            echo $line >> $new_diag
         fi 
      done <${tmp_diag} 

      #--------------------------------------------------------------------
      # if $new_diag still contains errors with reading diag files
      # then return $new_diag to the $opr_log
      #--------------------------------------------------------------------
      test_new_diag=`cat $new_diag | grep "PROBLEM"` 
      len_test_new_diag=`expr length "$test_new_diag"`

      l_end=`wc -l $opr_log`
      log_end=`echo $l_end | sed 's/:/ /g' | gawk '{print $1}'` 

      diag_start=`expr $diag_start - 1`
      diag_end=`expr $diag_end + 1`
      gawk "NR>=1 && NR<=$diag_start" ${opr_log} >> $tmp_log

      if [[ $len_test_new_diag -gt 0 ]]; then
         cat $new_diag >> $tmp_log
      fi

      gawk "NR>=$diag_end && NR<=$log_end" ${opr_log} >> $tmp_log
      mv -f $opr_log opr_log.bu 
      $NCP $tmp_log $opr_log 

   else
      no_diag_rpt=1 
   fi


   #--------------------------------------------------------------------
   #  Penalty report processing
   #--------------------------------------------------------------------
   end=`grep -n '============ ======= ======      Cycle                 Penalty          Bound' ${opr_log} | tail -1`
   opr_log_end=`echo $end | sed 's/:/ /g' | gawk '{print $1}'`

   if [[ $opr_log_end -gt 1 ]]; then
      opr_log_start=1


      #------------------------------------------------------------------------
      #  If $outfile exists, replace existing penalty report with $outfile 
      #  contents or remove the penalty report altogether if there is no
      #  $outfile
      #------------------------------------------------------------------------
      if [[ -s $outfile ]]; then
         echo "OUTFILE -s $outfile is TRUE"
         opr_log_end=`expr $opr_log_end + 1`
         gawk "NR>=$opr_log_start && NR<=$opr_log_end" ${opr_log} >> $new_log
         cat $outfile >> $new_log
         echo "End Cycle Data Integrity Report" >> $new_log
      else
         echo "OUTFILE -s $outfile is FALSE"
         opr_log_end=`expr $opr_log_end - 15`
         gawk "NR>=$opr_log_start && NR<=$opr_log_end" ${opr_log} >> $new_log
#         echo "NO ERROR REPORT" >> $new_log
         no_error_rpt=1 
      fi

   else
      
      if [[ -s $outfile ]]; then
         rm -f report.txt
         cp $opr_log $new_log

         echo "Begin Cycle Data Integrity Report" > report.txt  

         echo " "        >> report.txt
         echo "Cycle Data Integrity Report" >> report.txt
         echo "${PDATE}" >> report.txt
         echo " "        >> report.txt
         echo "Region Definitions:" >> report.txt
         echo " "        >> report.txt
    
         echo "  1  Global              (90S-90N, 0-360E)" >> report.txt
         echo " "        >> report.txt
         echo " "        >> report.txt
         echo "Penalty values outside of the established normal range were found" >> report.txt
         echo "for these sensor/channel/regions in the past two cycles: " >> report.txt
         echo " "        >> report.txt
         echo "Questionable Penalty Values " >> report.txt
         echo "============ ======= ======      Cycle                 Penalty          Bound" >> report.txt
         echo "                                 -----                 -------          ----- " >> report.txt
         echo " "        >> report.txt
         cat $outfile >> report.txt
         echo " "        >> report.txt
         echo " "        >> report.txt
         echo " "        >> report.txt
         echo "End Cycle Data Integrity Report" >> report.txt  

         cat report.txt >> $new_log

      else
         mv $opr_log $new_log
      fi
   fi

   if [[ $no_diag_rpt -eq 1 ]]; then
      echo "NO DIAG REPORT" >> $new_log
   fi
   if [[ $no_error_rpt -eq 1 ]]; then
      echo "NO ERROR REPORT" >> $new_log
   fi

   $NCP ./$new_log ${LOGdir}/data_extract.${day}.${cycle}.log

   #rm -f $new_log
   rm -f $opr_log 
   #rm -f $new_diag $tmp_diag
   rm -f $tmp_log

   $compress *.ctl


   #--------------------------------------------------------------------
   # Remove processing scripts/executables and exit.
   #--------------------------------------------------------------------
   rm -f validate_time.x
   rm -f validate.sh 
   rm -f radmon_err_rpt.sh  
   rm -f radmon_getchgrp.pl  
   rm -f opr_log.bu

   nfile_dest=`ls -l ${test_dir}/*${PDATE}*ieee_d* | egrep -c '^-'`

   if [[ exit_value -eq 0 && $nfile_src -ne $nfile_dest ]]; then
      exit_value=6 
   fi

fi


echo end Copy_glbl.sh
exit ${exit_value}

