#!/bin/sh
set -ax

#
# install_html.sh
#
# Given a suffix and a global/regional flag as inputs, build the
# html necessary for a radiance monitor web site and tranfer it to
# the server.
#

function usage {
  echo "Usage:  install_html.sh suffix area"
  echo "            Suffix is data source identifier that matches data in "
  echo "              the $TANKDIR/stats directory."
  echo "            area is either 'glb' or 'rgn' (global or regional)"
}

echo "BEGIN install_html.sh"
echo ""

nargs=$#
if [[ $nargs -ne 2 ]]; then
   usage
   exit 2
fi

SUFFIX=$1
echo SUFFIX = $SUFFIX
export RAD_AREA=$2

this_file=`basename $0`
this_dir=`dirname $0`

top_parm=${this_dir}/../../parm

if [[ -s ${top_parm}/RadMon_config ]]; then
   . ${top_parm}/RadMon_config
else
   echo "ERROR:  Unable to source ${top_parm}/RadMon_config"
   exit
fi

if [[ -s ${top_parm}/RadMon_user_settings ]]; then
   . ${top_parm}/RadMon_user_settings
else
   echo "ERROR:  Unable to source ${top_parm}/RadMon_user_settings"
   exit
fi


#--------------------------------------------------------------
#  source plot_rad_conf to get WEB_SVR, WEB_USER, WEBDIR
#
. ${RADMON_IMAGE_GEN}/parm/plot_rad_conf


#--------------------------------------------------------------
#  Get the area for this SUFFIX from the data_map file
#

if [[ $RAD_AREA == "glb" ]]; then 
   new_webdir=${WEBDIR}/${SUFFIX}
   . ${RADMON_IMAGE_GEN}/parm/glbl_conf
else
   new_webdir=${WEBDIR}/regional/${SUFFIX}
   . ${RADMON_IMAGE_GEN}/parm/rgnl_conf
fi

echo RAD_AREA    = $RAD_AREA
echo TANKverf = $TANKverf


#--------------------------------------------------------------
#  Create a temporary working directory.
#
workdir=$STMP_USER/${SUFFIX}_html
rmdir $workdir
mkdir $workdir
cd $workdir


#--------------------------------------------------------------
#  source glbl/rgnl_conf to get the satype list
#
use_static_satype=${STATIC_SATYPE:-0}
echo "use_static_satype =  $use_static_satype"

#-------------------------------------------------------------
#  If use_static_satype == 0 then assemble the SATYPE list from
#  available data files in $TANKverf angle*
#  If use_static_satype == 1 then load SATYPE from the SATYPE.txt
#  file.
#-------------------------------------------------------------
if [[ $use_static_satype -eq 0 ]]; then

   #-----------------------------------------------------------
   #  Find the first date with data.  Start at today and work
   #  backwards.  Stop after 90 days and exit.
   #
   PDATE=`${IG_SCRIPTS}/find_cycle.pl 1 ${TANKverf}`

   echo PDATE= $PDATE

   limit=`$NDATE -2160 $PDATE`		# 90 days
   echo limit, PDATE = $limit, $PDATE

   #-----------------------------------------------------------
   #  Build test_list which will contain all data files for
   #  one cycle in $PDATE. 
   data_found=0
   while [[ data_found -eq 0 && $PDATE -ge $limit ]]; do
      PDY=`echo $PDATE|cut -c1-8`

      if [[ -d $TANKverf/radmon.${PDY} ]]; then
         test00=`ls $TANKverf/radmon.${PDY}/angle.*${PDY}00*.ieee_d* | wc -l`
         test06=`ls $TANKverf/radmon.${PDY}/angle.*${PDY}06*.ieee_d* | wc -l`
         test12=`ls $TANKverf/radmon.${PDY}/angle.*${PDY}12*.ieee_d* | wc -l`
         test18=`ls $TANKverf/radmon.${PDY}/angle.*${PDY}18*.ieee_d* | wc -l`
         if [[ $test00 -gt 0 ]]; then
            test_list=`ls $TANKverf/radmon.${PDY}/angle.*${PDY}00*.ieee_d*`
            data_found=1
         elif [[ $test06 -gt 0 ]]; then
            test_list=`ls $TANKverf/radmon.${PDY}/angle.*${PDY}06*.ieee_d*`
            data_found=1
         elif [[ $test12 -gt 0 ]]; then
            test_list=`ls $TANKverf/radmon.${PDY}/angle.*${PDY}12*.ieee_d*`
            data_found=1
         elif [[ $test18 -gt 0 ]]; then
            test_list=`ls $TANKverf/radmon.${PDY}/angle.*${PDY}18*.ieee_d*`
            data_found=1
         fi
      else
        test=`ls $TANKverf/angle/*.${PDATE}*.ieee_d* | wc -l`
        if [[ $test -gt 0 ]]; then
           test_list=`ls $TANKverf/angle/*.${PDATE}.ieee_d*`
           data_found=1
        else
           PDATE=`$NDATE -24 $PDATE`
           echo PDATE = $PDATE
        fi
      fi
   done

   if [[ $data_found -eq 0 ]]; then
      echo Unable to locate any data files in the past 90 days for $SUFFIX 
      echo in $TANKverf/angle.
      exit
   fi

   #-----------------------------------------------------------
   #  Go through test_list  and identify all unique 
   #  sat_instrument combinations.  The results are the 
   #  SATYPE list for this source.
   # 

   for test in ${test_list}; do
      this_file=`basename $test`
      tmp=`echo "$this_file" | cut -d. -f1`
      if [[ $tmp == "angle" ]]; then
         tmp=`echo "$this_file" | cut -d. -f2`
      fi 
   
      #----------------------------------------------------------   
      #  remove sat/instrument_anl names so we don't end up
      #  with both "airs_aqua" and "airs_aqua_anl" if analysis
      #  files are being generated for this source.
      #----------------------------------------------------------   
      test_anl=`echo $tmp | grep "_anl"`
      if [[ $test_anl = "" ]]; then
         SATYPE_LIST="$SATYPE_LIST $tmp"
      fi
   done

   export SATYPE=$SATYPE_LIST
else
   TANKDIR_INFO=${TANKverf}/info
   STATIC_SATYPE_FILE=${TANKDIR_INFO}/SATYPE.txt

   #-------------------------------------------------------------
   #  Load the SATYPE list from the STATIC_SATYPE_FILE or exit
   #  if unable to locate it.
   #-------------------------------------------------------------
   if [[ -s $STATIC_SATYPE_FILE ]]; then
      SATYPE=""
      SATYPE=`cat ${STATIC_SATYPE_FILE}`
   else
      echo Unable to locate $STATIC_SATYPE_FILE, must exit.
      cd $workdir
      cd ../
      rm -rf $workdir
      exit
   fi
fi

echo $SATYPE

#--------------------------------------------------------------
#  Use the SATYPE list to construct the platform table.
#
UNSORTED_LIST=./unsorted.txt
>$UNSORTED_LIST
export SORTED_LIST=./sorted.txt
>$SORTED_LIST


for satype in $SATYPE; do
   ins=${satype%_*}
   tmp="${ins}_"
   sat=${satype#$tmp} 
#   sat=${sat%-*}

   sat_num=`echo $sat | tr -d '[[:alpha:]]'`	

   #-----------------------------------------------------------------
   # If sat_num has a length > 0 then we have a goes or noaa series
   # satellite.  Otherwise, convert sat to upper case and stop there.
   #
   if [[ ${#sat_num} -gt 0 ]]; then
      char=`expr substr $sat 1 1`
      if [[ $char == "g" ]]; then
         sat="GOES-${sat_num}"
      elif [[ $char == "n" ]]; then
         sat="NOAA-${sat_num}"
      else
         sat=`echo $sat | tr 'a-z' 'A-Z'`
      fi
   else
      sat=`echo $sat | tr 'a-z' 'A-Z'`
   fi


   #-----------------------------------------------------------------
   #  
   #
   amsu_test=`expr match ${ins} "amsu"`
   hirs_test=`expr match ${ins} "hirs"`
   ins_num=`echo $ins | tr -d '[[:alpha:]]'`	

   if [[ $amsu_test -gt 0 ]]; then
      char=`expr substr $ins 5 5`
      char=`echo $char | tr -s 'a-z' 'A-Z'`
      ins="AMSU-${char}"
   elif [[ $hirs_test -gt 0 ]]; then
      ins="HIRS/${ins_num}"
   else
      ins=`echo $ins | tr -s 'a-z' 'A-Z'`
   fi

   echo $sat $ins $satype >> $UNSORTED_LIST

done

#--------------------------------------------------------------
#  Sort the list by Satellite 
#
`sort -d $UNSORTED_LIST > $SORTED_LIST`
#rm -f $UNSORTED_LIST

#--------------------------------------------------------------
#  Read the sorted list and create the platform table
PLATFORM_TBL=./platform.txt
> ${PLATFORM_TBL}
TIME_PLATFORM_TBL=./time_platform.txt
> ${TIME_PLATFORM_TBL}

#echo '<TR><TD ALIGN=LEFT><B> Select Platform:<br>' >> $PLATFORM_TBL
#echo '<SELECT NAME="sat" size=1 OnChange=plot()>' >> $PLATFORM_TBL

quote='"'
id='  id="'
extra='">'
end_option='</OPTION>'

while read line; do
   sat=`echo $line | gawk '{print $1}'`
   ins=`echo $line | gawk '{print $2}'`
   satype=`echo $line | gawk '{print $3}'`

   hline='<OPTION VALUE="'
   hline=${hline}${satype}

   tline=${hline}${quote}${id}${satype}${extra}
   hline=${hline}${extra}
  
   hline="${hline} ${sat} ${ins} ${end_option}"
   tline="${tline} ${sat} ${ins} ${end_option}"

   echo $hline >> $PLATFORM_TBL
   echo $tline >> $TIME_PLATFORM_TBL
done < "$SORTED_LIST"
#<OPTION VALUE="sndrd1_g11"    id="sndrd1_g11"   > GOES-11 SNDRD1 </OPTION>

echo '</SELECT><P>' >> $PLATFORM_TBL
echo '</TD></TR>' >> $PLATFORM_TBL


#--------------------------------------------------------------
#  Edit the html files to add the platform table to each.
#
html_files="bcoef bcor bcor_angle comp horiz summary time"

for file in $html_files; do
   $NCP ${RADMON_IMAGE_GEN}/html/$file.html.$RAD_AREA .
   
   html_file=$file.html.$RAD_AREA
   tmp_html=./tmp_$file.html
   rm -f $tmp_html 

   found_platform_tbl=0
   finished_platform_tbl=0
   select_name_line=""

   while read line; do
      if [[ $found_platform_tbl -eq 1 && $finished_platform_tbl -eq 1 ]]; then
         echo $line >> $tmp_html
      elif [[ $found_platform_tbl -eq 0 ]]; then
         test_line=`echo $line | grep "Select Platform"`
         if [[ ${#test_line} -eq 0 ]]; then
            echo $line >> $tmp_html
         else
            found_platform_tbl=1
         fi
      else
         if [[ ${#select_name_line} -eq 0 ]]; then
            select_name_line=`echo $line | grep "SELECT NAME"`
         fi

         test_line=`echo $line | grep "</TD></TR>"`
         if [[ ${#test_line} -gt 0 ]]; then
            echo '<TR><TD ALIGN=LEFT><B> Select Platform:<br>' >> $tmp_html
            echo $select_name_line >> $tmp_html

             if [[ $file == "time" ]]; then
                `cat $TIME_PLATFORM_TBL >> $tmp_html`
             else
                `cat $PLATFORM_TBL >> $tmp_html`
             fi
            finished_platform_tbl=1
         fi
      fi

   done < $html_file

   rm $html_file
   mv $tmp_html $html_file
 
done

#--------------------------------------------------------------
# Generate the intro.html.$RAD_AREA file.
#
$NCP ${RADMON_IMAGE_GEN}/html/mk_intro.sh .

./mk_intro.sh 


#--------------------------------------------------------------
#  Copy the menu.html file and change "Experimental" to
#  "Operational" if the suffix is opr or nrx (operational GDAS
#  or NDAS.
#
$NCP ${RADMON_IMAGE_GEN}/html/menu.html.$RAD_AREA .

if [[ $SUFFIX == "opr" || $SUFFIX == "nrx" ]]; then
   tmp_menu=./tmp_menu.html.${RAD_AREA}
   sed s/Experimental/Operational/1 menu.html.${RAD_AREA} > ${tmp_menu}
   mv -f ${tmp_menu} menu.html.${RAD_AREA}
fi


$NCP ${RADMON_IMAGE_GEN}/html/index.html.$RAD_AREA .
html_files="bcoef bcor_angle  bcor comp horiz index intro menu summary time"

#--------------------------------------------------------------
#  If we're running on the CCS or WCOSS, push the html files to 
#  the web server.  If we're on zeus move the html files to
#  the $IMGNDIR so they can be pulled from the server.
#  Make the starting directory on the server and copy the
#  html files to it.
#
if [[ $MY_MACHINE = "ccs" || $MY_MACHINE = "wcoss" ]]; then
   ssh -l ${WEB_USER} ${WEB_SVR} "mkdir -p ${new_webdir}"
   for file in $html_files; do
      scp ${file}.html.${RAD_AREA} ${WEB_USER}@${WEB_SVR}:${new_webdir}/${file}.html
   done

   subdirs="angle bcoef bcor comp horiz summary time"
   for dir in $subdirs; do
      ssh -l ${WEB_USER} ${WEB_SVR} "mkdir -p ${new_webdir}/pngs/${dir}"
   done
else
   if [[ ! -d ${IMGNDIR} ]]; then
      mkdir -p ${IMGNDIR}
   fi
   imgndir=`dirname ${IMGNDIR}`

   for file in $html_files; do
      $NCP ${file}.html.${RAD_AREA} ${imgndir}/${file}.html
   done
   
   subdirs="angle bcoef bcor comp horiz summary time"
   for dir in $subdirs; do
      mkdir -p ${IMGNDIR}/${dir}
   done
fi

cd $workdir
cd ../
rm -rf $workdir

echo ""
echo "END install_html.sh"

exit
