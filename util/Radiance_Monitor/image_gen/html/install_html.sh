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
  echo "Usage:  install_html.sh suffix glb/rgn"
  echo "            Suffix is data source identifier that matches data in "
  echo "              the $TANKDIR/stats directory."
}


echo "BEGIN install_html.sh"
echo ""

nargs=$#
if [[ $nargs -ne 1 ]]; then
   usage
   exit 1
fi

SUFFIX=$1
echo SUFFIX = $SUFFIX

this_file=`basename $0`
this_dir=`dirname $0`

top_parm=${this_dir}/../../parm

if [[ -s ${top_parm}/RadMon_config ]]; then
   . ${top_parm}/RadMon_config
else
   echo "ERROR:  Unable to source ${top_parm}/RadMon_config"
   exit
fi


#--------------------------------------------------------------
#  source plot_rad_conf to get WEB_SVR, WEB_USER, WEBDIR
#
. ${RADMON_IMAGE_GEN}/parm/plot_rad_conf


#--------------------------------------------------------------
#  Get the area for this SUFFIX from the data_map file
#
export AREA=`${SCRIPTS}/get_area.sh ${SUFFIX} ${DATA_MAP}`
if [[ $AREA == "" ]]; then
   echo "ERROR:  Suffix $SUFFIX not found in ${DATA_MAP} file."
   exit
elif [[ $AREA == "glb" ]]; then 
   new_webdir=${WEBDIR}/${SUFFIX}
   . ${RADMON_IMAGE_GEN}/parm/glbl_conf
else
   new_webdir=${WEBDIR}/regional/${SUFFIX}
   . ${RADMON_IMAGE_GEN}/parm/rgnl_conf
fi

echo AREA    = $AREA
echo TANKDIR = $TANKDIR


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
use_static_satype=`${SCRIPTS}/get_satype.sh ${SUFFIX} ${DATA_MAP}`
if [[ $use_static_satype == "" ]]; then
   echo "ERROR:  Suffix $SUFFIX not found in ${DATA_MAP} file."
   exit
fi

echo "use_static_satype =  $use_static_satype"

#-------------------------------------------------------------
#  If use_static_satype == 0 then assemble the SATYPE list from
#  available data files in $TANKDIR/angle
#  If use_static_satype == 1 then load SATYPE from the SATYPE.txt
#  file.
#-------------------------------------------------------------
if [[ $use_static_satype -eq 0 ]]; then

   #-----------------------------------------------------------
   #  Find the first date with data.  Start at today and work
   #  backwards.  Stop after 90 days and exit.
   #
   PDATE=`date +%Y%m%d`
   PDATE=${PDATE}00
   echo PDATE= $PDATE

   limit=`$NDATE -2160 $PDATE`		# 90 days
   echo limit, PDATE = $limit, $PDATE

   data_found=0
   while [[ data_found -eq 0 && $PDATE -ge $limit ]]; do
      test=`ls $TANKDIR/angle/*.${PDATE}*.ieee_d* | wc -l`
      if [[ $test -gt 0 ]]; then
         data_found=1
      else
         PDATE=`$NDATE -24 $PDATE`
         echo PDATE = $PDATE
      fi
   done

   if [[ $data_found -eq 0 ]]; then
      echo Unable to locate any data files in the past 90 days for $SUFFIX 
      echo in $TANKDIR/angle.
      exit
   fi

   #-----------------------------------------------------------
   #  Build test_list which will contain all data files for
   #  the $PDATE cycle.  Go through this list and identify 
   #  all unique sat_instrument combinations.  That is the 
   #  SATYPE list for this source.
   # 
   test_list=`ls $TANKDIR/angle/*.${PDATE}.ieee_d*`

   for test in ${test_list}; do
      this_file=`basename $test`
      tmp=`echo "$this_file" | cut -d. -f1`
      echo $tmp
      SATYPE_LIST="$SATYPE_LIST $tmp"
   done

   export SATYPE=$SATYPE_LIST
else
   TANKDIR_INFO=${TANKDIR}/info
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
   sat=${sat%-*}

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
	 echo WARNING ==> unable to parse $sat into recognizable satellite name
      fi
   else
      sat=`echo $sat | tr -s 'a-z' 'A-Z'`
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
rm -f $UNSORTED_LIST

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
   sat=`echo $line | nawk '{print $1}'`
   ins=`echo $line | nawk '{print $2}'`
   satype=`echo $line | nawk '{print $3}'`

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
html_files="bcoef bcor bcor_angle horiz summary time"

for file in $html_files; do
   $NCP ${RADMON_IMAGE_GEN}/html/$file.html.$AREA .
   
   html_file=$file.html.$AREA
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
# Generate the intro.html.$AREA file.
#
$NCP ${RADMON_IMAGE_GEN}/html/mk_intro.sh .

mk_intro.sh 


#--------------------------------------------------------------
#  Copy the menu.html file and change "Experimental" to
#  "Operational" if the suffix is opr or nrx (operational GDAS
#  or NDAS.
#
$NCP ${RADMON_IMAGE_GEN}/html/menu.html.$AREA .

if [[ $SUFFIX == "opr" || $SUFFIX == "nrx" ]]; then
   tmp_menu=./tmp_menu.html.${AREA}
   sed s/Experimental/Operational/1 menu.html.${AREA} > ${tmp_menu}
   mv -f ${tmp_menu} menu.html.${AREA}
fi

 
#--------------------------------------------------------------
#  make the starting directory on the server and copy the
#  html files to it.
#
$NCP ${RADMON_IMAGE_GEN}/html/index.html.$AREA .
html_files="bcoef bcor_angle  bcor horiz index intro menu summary time"

ssh -l ${WEB_USER} ${WEB_SVR} "mkdir -p ${new_webdir}"
for file in $html_files; do
   scp ${file}.html.${AREA} ${WEB_USER}@${WEB_SVR}:${new_webdir}/${file}.html
done


#--------------------------------------------------------------
#  make the image directories
#
subdirs="angle bcoef bcor horiz summary time"
for dir in $subdirs; do
   ssh -l ${WEB_USER} ${WEB_SVR} "mkdir -p ${new_webdir}/pngs/${dir}"
done

cd $workdir
cd ../
rm -rf $workdir

echo ""
echo "END install_html.sh"

exit
