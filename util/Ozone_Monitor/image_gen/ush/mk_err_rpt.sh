#! /bin/bash

#-----------------------------------------------------------------
#
#  mk_err_rpt.sh
#
#     Put together the 3 possible elements of an error report for
#     a given cycle and mail to the recipient list.
#
#-----------------------------------------------------------------


function usage {
  echo " "
  echo "Usage:  mk_err_rpt.sh OZNMON_SUFFIX -p|--pdate yyyymmddcc -r|--run gdas|gfs"
  echo "            OZNMON_SUFFIX is data source identifier that matches data in "
  echo "              the $TANKverf/stats directory."
  echo "            -p | --pdate yyyymmddcc to specify the cycle to be plotted"
  echo "              if unspecified the last available date will be plotted"
  echo "            -r | --run   the gdas|gfs run to be plotted"
  echo " "
}

set -ax

echo start mk_err_rpt.sh
err=0

#-------------------------------------------------------
#  set standalone=1 to run from the command line instead
#  of from OznMon_Plot.sh
#
standalone=0


if [[ "$standalone" -eq 1 ]]; then
   nargs=$#
   echo "args = $nargs"

   if [[ $nargs -ne 5 ]]; then
      usage
      exit 1
   fi


   while [[ $# -ge 1 ]]; do
      key="$1"
      echo $key

      case $key in
         -p|--pdate)
            PDATE="$2"
            shift # past argument
         ;;
         -r|--run)
            export RUN="$2"
            shift # past argument
         ;;
         *)
            #any unspecified key is OZNMON_SUFFIX
            export OZNMON_SUFFIX=$key
         ;;
      esac

      shift
   done
fi

echo "OZNMON_SUFFIX = $OZNMON_SUFFIX"
echo "PDATE         = $PDATE"
echo "RUN           = $RUN"
PDY=`echo $PDATE | cut -c1-8`
cyc=`echo $PDATE | cut -c9-10`

hyperlink_base="http://www.emc.ncep.noaa.gov/gmb/gdas/es_ozn/index.html?"



if [[ "$standalone" -eq 1 ]]; then
   #--------------------------------------------------
   # source verison, config, and user_settings files
   #--------------------------------------------------
   this_dir=`dirname $0`
   top_parm=${this_dir}/../../parm


   oznmon_version_file=${oznmon_version:-${top_parm}/OznMon.ver}
   if [[ -s ${oznmon_version_file} ]]; then
      . ${oznmon_version_file}
      echo "able to source ${oznmon_version_file}"
   else
      echo "Unable to source ${oznmon_version_file} file"
      exit 2
   fi

   oznmon_user_settings=${oznmon_user_settings:-${top_parm}/OznMon_user_settings}
   if [[ -s ${oznmon_user_settings} ]]; then
      . ${oznmon_user_settings}
      echo "able to source ${oznmon_user_settings}"
   else
      echo "Unable to source ${oznmon_user_settings} file"
      exit 4
   fi

   oznmon_config=${oznmon_config:-${top_parm}/OznMon_config}
   if [[ -s ${oznmon_config} ]]; then
      . ${oznmon_config}
      echo "able to source ${oznmon_config}"
   else
      echo "Unable to source ${oznmon_config} file"
      exit 3
   fi

fi


#--------------------------------------------------------------------
#  Specify TANKDIR for this suffix
#--------------------------------------------------------------------
if [[ $GLB_AREA -eq 1 ]]; then
   TANKDIR=${OZN_TANKDIR}/stats/${OZNMON_SUFFIX}
else
   TANKDIR=${OZN_TANKDIR}/stats/regional/${OZNMON_SUFFIX}
fi


OZN_TANKDIR_TIME=${TANKDIR}/${RUN}.${PDY}/${cyc}/oznmon/time
echo "OZN_TANKDIR_TIME = $OZN_TANKDIR_TIME"


bad_cnt=`ls $OZN_TANKDIR_TIME/bad_cnt.${PDATE}`
bad_diag=`ls $OZN_TANKDIR_TIME/bad_diag.${PDATE}`
bad_pen=`ls $OZN_TANKDIR_TIME/bad_pen.${PDATE}`

echo "bad_cnt  = $bad_cnt"
echo "bad_diag = $bad_diag"
echo "bad_pen  = $bad_pen"

prev_cycle=`$NDATE -6 $PDATE`
prev_pdy=`echo $prev_cycle | cut -c1-8`
prev_cyc=`echo $prev_cycle | cut -c9-10`

OZN_TANKDIR_PREV=${TANKDIR}/${RUN}.${prev_pdy}/${prev_cyc}/oznmon/time
echo "OZN_TANKDIR_PREV = $OZN_TANKDIR_PREV"

prev_bad_cnt=`ls $OZN_TANKDIR_PREV/bad_cnt.${prev_cycle}`
prev_bad_pen=`ls $OZN_TANKDIR_PREV/bad_pen.${prev_cycle}`

echo "prev_bad_cnt = $prev_bad_cnt"
echo "prev_bad_pen = $prev_bad_pen"

have_err_rpt=0

if [[ -s $bad_cnt || -s $bad_diag || -s $bad_pen ]]; then

   echo "Making error report"
   err_rpt="err_rpt.txt"

   echo "Net, Run = $OZNMON_SUFFIX, $RUN" > $err_rpt
   echo "Cycle Data Integrity Report $PDATE" >> $err_rpt
   echo " " >> $err_rpt 
   echo " " >> $err_rpt
   echo " Region Definitions:" >> $err_rpt
   echo " " >> $err_rpt
   echo "    1, Global              (90S-90N, 0-360E)" >> $err_rpt
   echo " " >> $err_rpt

   if [[ -s ${bad_diag} ]]; then
      have_err_rpt=1
      diag_files=`cat ${bad_diag}`
      echo " Missing diagnostic files:" >> $err_rpt

      for word in ${diag_files}; do
         echo "   word = ${word}"
         if [[ $word =~ .*${PDATE}*. ]]; then
            echo "   ${word} " >> $err_rpt         
         fi
      done

      echo " " >> $err_rpt
   fi


   #-----------------------------------------------------
   #  Process the bad_cnt file contents.
   #
   #  Report any bad_cnt results that are in both this
   #   cycle's results and the previous cycle's results.
   #
   #
   if [[ -s ${bad_cnt} && -s ${prev_bad_cnt} ]]; then
      added_hdr=0
      hdr=" Sat/Instrument levels with low observational counts:"

      #-----------------------------------------------
      #  1. read the bad_cnt file line by line,
      #  2. cut the line on '=' and keep the first 3
      #      fields (sat, level, region). 
      #
      #      NOTE:  setting IFS (Internal Field Separator) to 
      #      something other than ' ' preserves white space, 
      #      and allows the grep to work correctly.
      #
      #  3. grep on that substring in the previous 
      #      cycle's bad_cnt file.  
      #  4. if grep matches then report a bad cnt for
      #      for the 2 cycle periods 
      #  5. add a hyperlink for report
      #
      cat ${bad_cnt} | while read LINE; do
         IFS='%'
         substr=$(echo $LINE | cut -d'=' -f 1-3)
         echo "substr = $substr"

         test=`grep "$substr" $prev_bad_cnt`
         echo "test,len   = $test, ${#test}"

         remains=`echo $test | gawk '{printf "%s %s %s %s", $5, $6, $7, $8}'`
         echo "remains = $remains"

         #------------------------------------------------
	 # Pull sat and lev for hyperlink construction.
	 # Only concerned (for now) with region 1 (global)
         # 
         sat=`echo $test | gawk '{print $1}' | xargs`
         lev=`echo $test | gawk '{print $3}'`
         reg=1
         stat="obs"

         link="${hyperlink_base}sat=${sat}"
         link="${link}&level=${lev}"
         link="${link}&region=${reg}"
         link="${link}&stat=${stat}"
         link="${link}&src=${OZNMON_SUFFIX}/${RUN}"

#         echo "link = $link"

         if [[ ${#test} -gt 0 ]]; then   

            if [[ "$added_hdr" -eq 0 ]]; then
               echo "$hdr" >> $err_rpt
               added_hdr=1
            fi

            echo "   $LINE" >> $err_rpt
            echo "                previous cycle:         $remains" >> $err_rpt
            echo "          $link" >> $err_rpt

         fi
      done

      #----------------------------------------------------------------------
      #  learned the hard way, assiging $have_err_rpt inside the while loop
      #  fails because bash implements while loops as subshells and the 
      #  assignment is out of scope upon return.
      #
      added_cnt=`cat $err_rpt | grep "$hdr"`
      if [[ ${#added_cnt} -gt 0 ]]; then
         have_err_rpt=1
         echo " " >> $err_rpt
      fi

   fi 


   #-----------------------------------------------------
   #  Process the bad_pen file contents.
   #
   #  Report any bad_pen results that are in both this
   #   cycle's results and the previous cycle's results.
   #
   if [[ -s ${bad_pen} && -s ${prev_bad_pen} ]]; then
      added_hdr=0
      hdr=" Sat/Instrument levels with high penalty values:"

      #---------------------------------------------------------
      #  Algorithm is the same as for the bad_cnt above.
      #
      #      NOTE:  setting IFS (Internal Field Separator) to 
      #      something other than ' ' preserves white space and
      #      allows the grep to work correctly.
      #
      cat ${bad_pen} | while read LINE; do
         IFS='%'
         substr=$(echo $LINE | cut -d'=' -f 1-3)
         echo "substr = $substr"

         test=`grep "$substr" $prev_bad_pen`
         echo "test,len   = $test, ${#test}"

         remains=`echo $test | gawk '{printf "%s %s %s %s", $5, $6, $7, $8}'`
         echo "remains = $remains"

         #------------------------------------------------
	 # Pull sat and lev for hyperlink construction.
	 # Only concerned (for now) with region 1 (global)
         # 
         sat=`echo $test | gawk '{print $1}' | xargs`
         lev=`echo $test | gawk '{print $3}'`
         reg=1
         stat="pen"

         link="${hyperlink_base}sat=${sat}"
         link="${link}&level=${lev}"
         link="${link}&region=${reg}"
         link="${link}&stat=${stat}"
         link="${link}&src=${OZNMON_SUFFIX}/${RUN}"

         if [[ ${#test} -gt 0 ]]; then   

            if [[ "$added_hdr" -eq 0 ]]; then
               echo "$hdr" >> $err_rpt
               added_hdr=1
            fi

            echo "   $LINE" >> $err_rpt
            echo "                previous cycle:       $remains" >> $err_rpt
            echo "      $link" >> $err_rpt
         fi
      done

      #----------------------------------------------------------------------
      #  learned the hard way, assiging $have_err_rpt inside the while loop
      #  fails because bash implements while loops as subshells and the 
      #  assignment is out of scope upon return.
      #
      added_pen=`cat $err_rpt | grep "$hdr"`
      if [[ ${#added_pen} -gt 0 ]]; then
         have_err_rpt=1
         echo " " >> $err_rpt
      fi

   fi 

   if [[ "$have_err_rpt" -gt 0 ]]; then
   
      echo " " >> $err_rpt
      echo " " >> $err_rpt

      echo " *********************** WARNING ***************************" >> $err_rpt
      echo " " >> $err_rpt
      echo "    This is an automated email.  Replies to sender "  >> $err_rpt
      echo "    will not be received.  Please direct replies to:" >> $err_rpt
      echo " " >> $err_rpt
      echo "                 edward.safford@noaa.gov"             >> $err_rpt
      echo " " >> $err_rpt
      echo " *********************** WARNING ***************************" >> $err_rpt

      #----------------------------------------------------------------
      #  Now mail it!
      #
      if [[ ${#MAIL_CC} -gt 1 ]]; then
         mail -s "OznMon Error Report" -c "${MAIL_CC}" ${MAIL_TO} < ${err_rpt}
      else
         mail -s "OznMon Error Report" ${MAIL_TO} < ${err_rpt}
      fi

   fi

fi 

echo end mk_err_rpt.sh

#exit( $err )
