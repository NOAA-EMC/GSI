#!/bin/ksh

#-----------------------------------------------------------------------------
#
#  script:   mk_err_rpt
#
#  purpose:  Compare the bad_[pen|obs] files from two different cycles and 
#            report all unique satnames/channel/region that appear
#            in both error files (meaning the same error has occured in two
#            successive cycles).  Report this by category (obs and 
#            penalty values to the error file.
#
#  arguments:  file1 	bad_obs||bad_pen file name
#              file2 	bad_obs||bad_pen file name
#              type   	obs || pen
#              cycle1 	cycle time of file 1 (HH)
#              cycle2 	cycle time of file 2 (HH)
#              diag_rpt diag report text file
#              outfile	output file name 
#-----------------------------------------------------------------------------


set -ax
export list=$list0

SUFFIX='opr'

nargs=$#
echo nargs = $nargs

if [[ nargs -eq  7 ]]; then
   file1=$1
   file2=$2
   type=$3
   cycle1=$4
   cycle2=$5
   diag_rpt=$6
   outfile=$7

   echo $type

   have_diag_rpt=0
   if [[ -s $diag_rpt ]]; then
      have_diag_rpt=1
   fi


#-----------------------------------------------------------------------------
#  read each line in the $file1 
#  search $file2 for the same satname, channel, and region 
#  if same combination is in both files, add the values to the output file
#  
   { while read myline;do
      bound=""

      echo $myline
      satname=`echo $myline | nawk '{print $1}'`
      echo satname = $satname
      channel=`echo $myline | nawk '{print $3}'`
      echo channel = $channel
      region=`echo $myline | nawk '{print $5}'`
      echo region = $region
      value1=`echo $myline | nawk '{print $7}'`
      echo value1 = $value1
      bound=`echo $myline | nawk '{print $9}'`

#
#     Check findings against diag_report.  If the satellite/instrument is on the 
#     diagnostic report it means the diagnostic file file for the
#     satelite/instrument is missing for this cycle, so skip any additional
#     error checking for that source.  Otherwise, evaluate as per normal.
#

      diag_match=""
      diag_match_len=0 

      if [[ $have_diag_rpt == 1 ]]; then
         diag_match=`nawk "/$satname/" $diag_rpt`
         diag_match_len=`echo ${#diag_match}`
      fi


      if [[ $diag_match_len == 0 ]]; then  

         if [[ $type == "chan" ]]; then
            match=`nawk "/$satname/ && /channel=  $channel/" $file2`
         else
            match=`nawk "/$satname/ && /channel=  $channel / && /region=  $region /" $file2`
         fi
         match_len=`echo ${#match}`

         if [[ $match_len > 0 ]]; then
            echo $match_len
            value2=`echo $match | nawk '{print $7}'`
            bound2=`echo $match | nawk '{print $9}'`

            if [[ $type == "chan" ]]; then
               tmpa="$satname  channel= $channel"
               tmpb=""

            elif [[ $type == "pen" ]]; then
               tmpa="$satname  channel= $channel region= $region"
               tmpb="$cycle1         	$value1	$bound"

            else
               tmpa="$satname  channel= $channel region= $region"
               tmpb="$cycle1: $type= $value1"
            fi

            line1="$tmpa $tmpb"
            echo "$line1" >> $outfile

            if [[ $type != "chan" ]]; then
               tmpc=`echo $tmpa |sed 's/[a-z]/ /g' | sed 's/[0-9]/ /g' | sed 's/=/ /g' | sed 's/_/ /g' | sed 's/-/ /g'`

               if [[ $type == "pen" ]]; then
                  line2=" $tmpc $cycle2         	$value2	$bound2"
               else
                  line2=" $tmpc $cycle2: $type= $value2"
               fi 

               echo "$line2" >> $outfile
            fi

            #----------------------------------------------------------
            #  Deterimine channel grouping number.
            #
            #  All sources except for airs_aqua and iasi_metop-a have 
            #  consecutively numbered channels.  In building the link 
            #  to the web site images, the channels are grouped 4 to an 
            #  image.  So for all other channels we can figure out from 
            #  the channel number what the channel group number is.  
            #  For airs_aqua and iasi_metop-a we have to go to the 
            #  *.ctl file and find the channel in question.
            #

            ctlfile="${satname}.ctl"
            changrp=`${SCRIPTS}/get_channel_grp.pl ${ctlfile} ${channel}`
            line3="   http://www.emc.ncep.noaa.gov/gmb/gdas/radiance/esafford/opr/index.html?sat=$satname&region=region$region&channel=$changrp&stat=$type"
            if [[ $changrp -gt 0 ]]; then
               line3="   http://www.emc.ncep.noaa.gov/gmb/gdas/radiance/esafford/opr/index.html?sat=$satname&region=region$region&channel=$changrp&stat=$type"
               echo "$line3" >> $outfile
               echo "" >> $outfile
            fi
         fi
      fi
   done } < $file1

else
   #----------------------------------------------
   #  display useage message
   #
   echo Script mk_err_rpt.sh requires 6 arguments:
   echo    file1    -- bad opr or pen file
   echo    file2    -- bad opr or pen file
   echo    type     -- opr or pen
   echo    cycle1   -- cycle of file 1
   echo    cycle2   -- cycle HH of file2
   echo    outfile  -- output file name
fi

