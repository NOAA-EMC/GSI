#!/bin/bash

#----------------------------------------------------------------
#  Check the contents of the radstat file and compare to
#  the ${run}_radmon_satype.txt file.  Report any missing 
#  or zero sized diag files.
#    

   function usage {
     echo "Usage:  radmon_mk_warning -rad radstat --sat satype --out output "
     echo ""
     echo "            --pen bad pen file "
     echo ""
     echo "            --obs low count file "
     echo ""
     echo "            --diag missing diag file"
     echo ""
     echo "            -o,--out output file name (required)"
     echo "              File name for warning report."
   }


echo "--> radmon_mk_warning.sh"


#--------------------------
#  Process input arguments
#
   nargs=$#
   if [[ $nargs -le 0 ]]; then
      usage
      exit 1
   fi

   while [[ $# -ge 1 ]]
   do
      key="$1"
      echo $key

      case $key in
         --pen)
            bad_pen_file="$2"
            shift # past argument
         ;;
         --obs)
            low_obs_file="$2"
            shift # past argument
         ;;
         --diag)
            missing_diag_file="$2"
            shift # past argument
         ;;
         -o|--out)
            output_file="$2"
            shift # past argument
         ;;
         *)
            #unspecified key 
            echo " unsupported key = $key"
         ;;
      esac

      shift
   done

#   set -ax

   echo " bad_pen_file      = ${bad_pen_file}"
   echo " missing_diag_file = ${missing_diag_file}"
   echo " low_obs_file      = ${low_obs_file}"
   echo " output_file       = ${output_file}"


   #---------------------------
   #  report header
   #
   echo "Radiance Monitor warning report"   >> ${output_file}
   echo ""                                  >> ${output_file}
   echo "     Net:  ${RADMON_SUFFIX}"       >> ${output_file}
   echo "     RUN:  ${RUN}"                 >> ${output_file}
   echo "   Cycle:  ${PDATE}"               >> ${output_file}
   echo ""                                  >> ${output_file}
   echo ""                                  >> ${output_file}
   echo "=================================" >> ${output_file}
   echo ""                                  >> ${output_file}
   echo ""                                  >> ${output_file}

   #---------------------------
   #  missing diag report
   #
   if [[ -s ${missing_diag_file} ]]; then

      echo ""                                  >> ${output_file}
      echo "Problem Reading Diagnostic File"   >> ${output_file}
      echo ""                                  >> ${output_file}

      echo "Problems were encountered reading the diagnostic file for" >> ${output_file}
      echo "the following sources:"            >> ${output_file}
      echo ""                                  >> ${output_file}
      cat ${missing_diag_file}                 >> ${output_file}
      echo ""                                  >> ${output_file}
      echo ""                                  >> ${output_file}
      echo "=================================" >> ${output_file}
      echo ""                                  >> ${output_file}
      echo ""                                  >> ${output_file}
   fi


   #---------------------------
   #  low count report
   #
   if [[ -s $low_obs_file ]]; then

      echo " "                                             >> ${output_file}
      echo "The following channels report abnormally low " >> ${output_file}
      echo "     observational counts in latest 2 cycles:" >> ${output_file}
      echo " "                                             >> ${output_file}
      echo "Satellite/Instrument                               Obs Count   Avg Count" >> ${output_file}
      echo "====================                               =========   =========" >> ${output_file}

      cat ${low_obs_file}                                  >> ${output_file}
      echo "================================="             >> ${output_file}
      echo ""                                              >> ${output_file}
      echo ""                                              >> ${output_file}
   fi

   #---------------------------
   #  penalty report
   #
   if [[ -s $bad_pen_file ]]; then
      echo " "                                             >> ${output_file}
      echo " "                                             >> ${output_file}
      echo " "                                             >> ${output_file}

      echo "Penalty values outside of the established normal range were found" >> ${output_file}
      echo "for these sensor/channel/regions in the past two cycles: "         >> ${output_file}

      echo " "                                             >> ${output_file}
      echo "Questionable Penalty Values "                  >> ${output_file}
      echo "============ ======= ======      Cycle                 Penalty          Bound " >> ${output_file}
      echo "                                 -----                 -------          ----- " >> ${output_file}
      echo " "                                             >> ${output_file}
      cat $bad_pen_file                                    >> ${output_file}
      echo " "                                             >> ${output_file}
      echo " "                                             >> ${output_file}
      echo " "                                             >> ${output_file}
   fi


   #-------------------
   #  report footer
   #
   echo " "                                                           >> ${output_file}
   echo " "                                                           >> ${output_file}
   echo " "                                                           >> ${output_file}
   echo "*********************** WARNING ***************************" >> ${output_file}
   echo "This is an automated email.  Replies to sender will not be"  >> ${output_file}
   echo "received.  Please direct replies to edward.safford@noaa.gov" >> ${output_file}
   echo "*********************** WARNING ***************************" >> ${output_file}


echo "<-- radmon_mk_warning.sh"
exit
