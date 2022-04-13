#!/bin/sh
set -ax

#--------------------------------------------------------------------
#--------------------------------------------------------------------
#  install_rgn.sh
#
#  Given a suffix and a global/regional flag as inputs, build the
#  html necessary for a radiance monitor web site and tranfer it to
#  the server (rgn only, regional is handled by Install_html.sh).
#--------------------------------------------------------------------
#--------------------------------------------------------------------

function usage {
  echo "Usage:  install_rgn.sh suffix"
  echo "            Suffix is data source identifier that matches data in "
  echo "              the $TANKDIR/stats directory."
}

echo "BEGIN install_rgn.sh"
echo ""

nargs=$#
if [[ $nargs -lt 0 ]]; then
   usage
   exit 2
fi

SUFFIX=$1
echo SUFFIX = $SUFFIX
RAD_AREA="rgn"

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
#  Get the area for this SUFFIX from the data_map file
#
new_webdir=${WEBDIR}/${SUFFIX}

echo RAD_AREA = $RAD_AREA
echo TANKverf = $TANKverf


#--------------------------------------------------------------
#  Create a temporary working directory.
#
workdir=$STMP_USER/${SUFFIX}_html
rmdir $workdir
mkdir $workdir
cd $workdir


#-------------------------------------------------------------
#  Assemble the SATYPE list from available data files in 
#  $TANKverf using angle.* files.
#-------------------------------------------------------------

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
#
#  Note:  with the new nam (rapid refresh) the data file name has changed
#         from this:
#              angle.[sat_name].[cycle].ieee_d.gz  
#         to this:
#              t[cc]z.angle.[sat_name].[cycle].ieee_d.tm[HH].gz
#
#         so to cope we'll have to build a list and step through file names 
#         until we find "angle" then take the next field to get the sat names.

data_found=0
while [[ data_found -eq 0 && $PDATE -ge $limit ]]; do
   PDY=`echo $PDATE|cut -c1-8`

   if [[ ${TANK_USE_RUN} -eq 1 ]]; then
      test_dir=${TANKverf}/${RUN}.${PDY}/${MONITOR}
   else
      test_dir=${TANKverf}/${MONITOR}.${PDY}
   fi

   echo "test_dir = ${test_dir}"

   if [[ -d ${test_dir} ]]; then
      echo " test_dir is GO "

      test00=`ls ${test_dir}/*angle.*${PDY}00*.ieee_d* | wc -l`
      test06=`ls ${test_dir}/*angle.*${PDY}06*.ieee_d* | wc -l`
      test12=`ls ${test_dir}/*angle.*${PDY}12*.ieee_d* | wc -l`
      test18=`ls ${test_dir}/*angle.*${PDY}18*.ieee_d* | wc -l`
      if [[ $test00 -gt 0 ]]; then
         test_list=`ls ${test_dir}/*angle.*${PDY}00*.ieee_d*`
         data_found=1
      elif [[ $test06 -gt 0 ]]; then
         test_list=`ls ${test_dir}/*angle.*${PDY}06*.ieee_d*`
         data_found=1
      elif [[ $test12 -gt 0 ]]; then
         test_list=`ls ${test_dir}/*angle.*${PDY}12*.ieee_d*`
         data_found=1
      elif [[ $test18 -gt 0 ]]; then
         test_list=`ls ${test_dir}/*angle.*${PDY}18*.ieee_d*`
         data_found=1
      fi
   else
      echo " test_dir is NOGO "
   fi

   if [[ data_found -eq 0 ]]; then
     PDATE=`$NDATE -24 $PDATE`
     echo PDATE = $PDATE
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
   tmp1=`echo "$this_file" | cut -d. -f1`
   tmp2=`echo "$this_file" | cut -d. -f2`
   if [[ $tmp1 == "angle" ]]; then
      tmp=`echo "$this_file" | cut -d. -f2`
   elif [[ $tmp2 == "angle" ]]; then
      tmp=`echo "$this_file" | cut -d. -f3`
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


#----------------------------------------------------------   
#  use sort to remove duplicate entries in SATYPE_LIST
#----------------------------------------------------------   
unique_satype=`echo $(printf '%s\n' $SATYPE_LIST | sort -u)`
export SATYPE=$unique_satype

if [[ ${#SATYPE} -le 0 ]]; then  
  echo "SATYPE list is zero length, unable to complete html installation"
  exit 
fi

echo "SATYPE = $SATYPE"

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

echo
echo
cat $SORTED_LIST

#--------------------------------------------------------------
#  Read the sorted list and create the platform table
#
PLATFORM_TBL=./platform.txt
> ${PLATFORM_TBL}
TIME_PLATFORM_TBL=./time_platform.txt
> ${TIME_PLATFORM_TBL}

quote='"'
id='  id="'
extra='">'
end_option='</OPTION>'

while read line; do
   sat=`echo $line | gawk '{print $1}'`
   ins=`echo $line | gawk '{print $2}'`
   satype=`echo $line | gawk '{print $3}'`

   hline='<OPTION value="'
   hline=${hline}${satype}

   hline=${hline}${quote}${id}${satype}${extra}
   hline="${hline} ${sat} ${ins} ${end_option}"

   echo $hline >> $PLATFORM_TBL
done < "$SORTED_LIST"


imgndir=`dirname ${IMGNDIR}`

#--------------------------------------------------------------
#  Edit the html files to add the platform table to each.
#
mod_html_files="plot_summary.html.rgn plot_time.html.rgn plot_angle.html.rgn plot_bcoef.html.rgn"

for html_file in $mod_html_files; do
   echo "processing ${html_file}"
   $NCP ${RADMON_IMAGE_GEN}/html/${html_file} .
   
   tmp_html=./tmp_${html_file}
   rm -f ${tmp_html}

   #  copy the $file from start to 'INSERT_TABLE' comment
   sed -e '/INSERT_TABLE/,$d' ${html_file} > ${tmp_html}

   #  add the $PLATFORM_TBL (built above)
   `cat $PLATFORM_TBL >> ${tmp_html}`

   #  copy the $file from 'END_TABLE_INSERT' comment to end
   sed -n '/END_TABLE_INSERT/,$p' ${html_file} >> ${tmp_html}

   rm $html_file

   #  rm the .rgn extension on the html file names
   new_html_file=`echo $html_file | cut -d'.' -f-2`
   echo "new_html_file = $new_html_file"

   #  switch all 'INSERT_SUFFIX' tags to the actual suffix
   #  and route output to $new_html_file and we're done.
   sed s/INSERT_SUFFIX/${SUFFIX}/g ${tmp_html} > ${new_html_file}

done

#--------------------------------------------------------------
#  Optionally enable comparison plots to the operational
#    GDAS data
#
#  THIS remains a global only option at present but I'll leave this in
#  place so developing a comparison to the operational NDAS is easier

#set +ax
#comp_html_files="plot_summary.html plot_time.html"
#
#echo "Do you wish to enable data plots to include comparison to operational GDAS data?"
#echo -n "  Enter YES to enable, any other input to disable.  > "
#read text
#short=`echo $text | cut -c1`
#
#if [[ $short = "Y" || $short = "y" ]]; then
#
#   for html_file in $comp_html_files; do
#      echo "processing ${html_file}"
#
#      tmp_html=./tmp_${html_file}
#      rm -f ${tmp_html}
#
#      sed '/OPTIONAL_COMPARE/d' ./${html_file} > ${tmp_html}
#      mv -f ${tmp_html} ${html_file}
#   done
#fi
#
#set -ax

#--------------------------------------------------------------
# Generate the intro.html file.
#
$NCP ${RADMON_IMAGE_GEN}/html/mk_intro.sh .
$NCP ${RADMON_IMAGE_GEN}/html/intro.html  intro.html.stock 

./mk_intro.sh 


#--------------------------------------------------------------
#  Copy the index.html file and change INSERT_SUFFIX to actual suffix.
index_file="index.html.$RAD_AREA"
tmp_index="tmp.index.html"
new_index="index.html"

$NCP ${RADMON_IMAGE_GEN}/html/${index_file} ${new_index}
 
sed s/INSERT_SUFFIX/${SUFFIX}/g ${new_index} > ${tmp_index}
if [[ $SUFFIX == "wopr" || $SUFFIX == "nrx" ]]; then
   sed s/Experimental/Operational/1 ${tmp_index} > ${new_index}
fi

#if [[ ! -s ${new_index} ]]; then
#   if [[ -s ${tmp_index} ]]; then
#      $NCP ${tmp_index} ${new_index}
#   else
#      $NCP ${index_file} ${new_index}
#   fi
#fi

if [[ -s ${tmp_index} ]]; then
   rm ${tmp_index}
fi
if [[ -s ${index_file} ]]; then
   rm ${index_file}
fi

#--------------------------------------------------------------
#  Make starting directory in $imgndir and copy over html, 
#  misc, and thumb images.
#

if [[ ! -d ${IMGNDIR} ]]; then
   mkdir -p ${IMGNDIR}
fi
imgndir=`dirname ${IMGNDIR}`

#-----------------------
#  move html files to imgndir
#
all_html_files=`ls *.html`
for file in $all_html_files; do
   $NCP ${file} ${imgndir}/${file}
done
   
#-----------------------
#  mk image dirs 
#
subdirs="angle bcoef summary time"
for dir in $subdirs; do
   mkdir -p ${imgndir}/pngs/${dir}
done

#-----------------------
#  supporting files
#
support_files="jsuri-1.1.1.js stats.js latest_cycle.php"
for file in $support_files; do
   $NCP ${RADMON_IMAGE_GEN}/html/${file} ${imgndir}/.
done

#-----------------------
#  arrow graphics
#
arrow_files="arrowleft.png arrowright.png"
for file in $arrow_files; do
   $NCP ${RADMON_IMAGE_GEN}/html/${file} ${imgndir}/pngs/.
done

#-----------------------
#  summary thumb images
#    If any are missing dummy one in using a copy of sndrdr1_g15.
#
thumbs="sum_thumbs.tar"
$NCP ${RADMON_IMAGE_GEN}/html/${thumbs} ${imgndir}/pngs/summary/. 
cd ${imgndir}/pngs/summary
tar -xvf ${thumbs}
rm -f ${thumbs}

for satype in $SATYPE; do
   if [[ ! -e ${satype}.summary.png ]]; then
      $NCP sndrd1_g15.summary.png ${satype}.summary.png
   fi
done

img_list=`ls *.png`			# rm any images for sources not in $SATYPE
for img in ${img_list}; do
   tmp=`echo "$img" | cut -d. -f1`
   echo $tmp
   img_match=`echo $SATYPE | grep $tmp`
   if [[ ${#img_match} -le 0 ]]; then
      rm -f ${img}
   fi
done


#---------------------------------------------------
# if on wcoss then cd $imgndir and do the rsync here
#
if [[ $MY_MACHINE = "wcoss" ]]; then
   if [[ ${imgndir} != "/" ]]; then	      # sanity check to avoid serious embarrassment
      /usr/bin/rsync -ave ssh  --exclude *.ctl.${Z} ${imgndir}/ \
         ${WEB_USER}@${WEB_SVR}.ncep.noaa.gov:${WEBDIR}/
   fi
fi

#------------------------
# clean up $workdir
#
#cd $workdir
#cd ../
#rm -rf $workdir

echo ""
echo "END install_rgn.sh"

exit
