#!/bin/bash

#--------------------------------------------------------------------
#--------------------------------------------------------------------
#  install_glb.sh
#
#  Given a suffix and a global/regional flag as inputs, build the
#  html necessary for a radiance monitor web site and tranfer it to
#  the server (glb only, regional is handled by Install_html.sh).
#--------------------------------------------------------------------
#--------------------------------------------------------------------

echo "BEGIN install_glb.sh"
echo ""
echo ""

do_cmp=0
cmp_src=""

#--------------------------------------------------------------
#  Allow user to enable comparison plots 
#
echo "Do you wish to enable data plots to include comparison to"
echo " operational GDAS data, or another data source?"
echo ""
echo -n "  Enter YES to enable comparison plots, any other input to disable.  > "
read text
short=`echo $text | cut -c1`

if [[ $short = "Y" || $short = "y" ]]; then
   do_cmp=1
   cmp_src="GDAS"

   echo "Please specify the suffix of your comparison data source,"
   echo "  or just hit the return key to use the operational GDAS as "
   echo "  the comparison source"
   echo ""
   echo -n " > "
   read text

   if [[ ${#text} -gt 0 ]]; then
     cmp_src=${text}
   fi

   echo "${cmp_src} will be used as the comparison source."
fi


SUFFIX=$RADMON_SUFFIX
RAD_AREA="glb"

this_file=`basename $0`
this_dir=`dirname $0`
new_webdir=${WEBDIR}/${SUFFIX}


#--------------------------------------------------------------
#  Create a temporary working directory.
#
workdir=$STMP_USER/${SUFFIX}_html
if [[ -e $workdir ]]; then
   rm -rf $workdir
fi
mkdir $workdir
cd $workdir


#-------------------------------------------------------------
#  Assemble the SATYPE list from available data files in 
#  $TANKverf using angle.* files.
#-------------------------------------------------------------

#-----------------------------------------------------------
#  Find the first date with data.  Start at today and work
#  backwards.  If not found stop after 90 days and exit.
#
PDATE=`${IG_SCRIPTS}/nu_find_cycle.pl --dir ${TANKverf} --cyc 1`
limit=`$NDATE -2160 $PDATE`		# 90 days

#-----------------------------------------------------------
#  Build test_list which will contain all data files for
#  one cycle in $PDATE. 

data_found=0
while [[ data_found -eq 0 && $PDATE -ge $limit ]]; do
   PDY=`echo $PDATE|cut -c1-8`
   CYC=`echo $PDATE|cut -c9-10`

   test_dir=${TANKverf}/${RUN}.${PDY}/${CYC}/${MONITOR}
   if [[ ! -d ${test_dir} ]]; then
      test_dir=${TANKverf}/${RUN}.${PDY}/${MONITOR}
   fi
   if [[ ! -d ${test_dir} ]]; then
      test_dir=${TANKverf}/${RUN}.${PDY}
   fi
   
   if [[ -d ${test_dir} ]]; then
      echo " test_dir is GO "

      if [[ -e ${test_dir}/radmon_angle.tar || -e ${test_dir}/radmon_angle.tar.gz ]]; then
         gzipped=0
         if [[ -e ${test_dir}/radmon_angle.tar.gz ]]; then
            gunzip ${test_dir}/radmon_angle.tar.gz
            gzipped=1
         fi 

         test_list=`tar -tf ${test_dir}/radmon_angle.tar`
	 data_found=1 

         if [[ $gzipped -eq 1 ]]; then
            gzip ${test_dir}/radmon_angle.tar
         fi
      else
         test=`ls ${test_dir}/angle.*${PDATE}*.ieee_d* | wc -l`
         if [[ $test -gt 0 ]]; then
            test_list=`ls ${test_dir}/angle.*${PDATE}*.ieee_d*`
            data_found=1
	 fi
      fi
   else
      echo "test_dir is NOGO"
   fi

   if [[ data_found -eq 0 ]]; then
     PDATE=`$NDATE -24 $PDATE`
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
#  SATYPE list for this source.  Avoid _anl and ctl files
#  since they will give us duplicate satypes.
# 
for test in ${test_list}; do
   this_file=`basename $test`

   test_anl=`echo $this_file | grep "_anl"`
   test_ctl=`echo $this_file | grep "ctl"`
   if [[ $test_anl != "" || $test_ctl != "" ]]; then
      continue
   fi

   tmp=`echo "$this_file" | cut -d. -f1`
   if [[ $tmp == "angle" ]]; then
      tmp=`echo "$this_file" | cut -d. -f2`
   fi 

   SATYPE_LIST="$SATYPE_LIST $tmp"
done

export SATYPE=$SATYPE_LIST

if [[ ${#SATYPE} -le 0 ]]; then  
  echo "SATYPE list is zero length, unable to complete html installation"
  exit 
fi


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
   #  Certain instruments require specific formatting.
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
`sort -d -u $UNSORTED_LIST > $SORTED_LIST`

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
mod_html_files="plot_summary.html plot_time.html plot_angle.html plot_bcoef.html"

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

   #  switch all 'INSERT_SUFFIX' tags to the actual suffix
   #  and route output to $html_file and we're done.
   sed s/INSERT_SUFFIX/${SUFFIX}/g ${tmp_html} > ${html_file}
   rm ${tmp_html}

done

#--------------------------------------------------------------
#  Enable comparison plots
#
if [[ $do_cmp == 1 ]]; then

   comp_html_files="plot_summary.html plot_time.html"

   #-------------------------------------------------------------------------
   #  If cmp_src == GDAS we only have to uncomment the comparison check box
   #  in the html files.  If it's another source then we'll have to change
   #  the values of compSrc, compName, and compHome in the html files.
   #

   for html_file in $comp_html_files; do
      echo "processing ${html_file}"

      tmp_html=./tmp_${html_file}
      rm -f ${tmp_html}

      #----------------------------------------------------------------------------
      # remove the OPTIONAL_COMPARE lines which uncomments the comparison check box
      sed '/OPTIONAL_COMPARE/d' ./${html_file} > ${tmp_html}
      mv -f ${tmp_html} ${html_file}

      #---------------------------------------------------------------
      # if we're using a source other than GDAS make that change here
      if [[ $cmp_src != "GDAS" ]]; then
         cmp_sc_line="            var compSrc  = \"${cmp_src}\";"
         cmp_nm_line="            var compName = \"${cmp_src}\";"
         cmp_hm_line="            var compHome = \"../${cmp_src}/\";"

         sed -i "/var compSrc /c ${cmp_sc_line}" ${html_file}
         sed -i "/var compName /c ${cmp_nm_line}" ${html_file}
         sed -i "/var compHome /c ${cmp_hm_line}" ${html_file}
      fi

   done
fi

#--------------------------------------------------------------
# Generate the intro.html file.
#
$NCP ${RADMON_IMAGE_GEN}/html/mk_intro.sh .
$NCP ${RADMON_IMAGE_GEN}/html/intro.html  intro.html.stock 

./mk_intro.sh 
rm mk_intro.sh

#--------------------------------------------------------------
#  Copy the index.html file and change INSERT_SUFFIX to actual suffix.
index_file="index.html.$RAD_AREA"
tmp_index="tmp.index.html"
new_index="index.html"

$NCP ${RADMON_IMAGE_GEN}/html/${index_file} .
sed s/INSERT_SUFFIX/${SUFFIX}/g $index_file > ${tmp_index}
if [[ $SUFFIX == "GFS" || $SUFFIX == "nrx" ]]; then
   sed s/Experimental/Operational/1 ${tmp_index} > ${new_index}
fi

if [[ ! -s ${new_index} ]]; then
   if [[ -s ${tmp_index} ]]; then
      $NCP ${tmp_index} ${new_index}
   else
      $NCP ${index_file} ${new_index}
   fi
fi

rm ./${index_file}


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
all_html_files="${mod_html_files} index.html intro.html"
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
#  js files
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
#    If any are missing dummy one in using ssmis_f18.
#
thumbs="sum_thumbs.tar"
$NCP ${RADMON_IMAGE_GEN}/html/${thumbs} ${imgndir}/pngs/summary/. 
cd ${imgndir}/pngs/summary
tar -xvf ${thumbs}
rm -f ${thumbs}

for satype in $SATYPE; do
   if [[ ! -e ${satype}.summary.png ]]; then
      $NCP ssmis_f18.summary.png ${satype}.summary.png
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
if [[ $MY_MACHINE = "wcoss_d" || $MY_MACHINE = "wcoss2" ]]; then

   if [[ ${imgndir} != "/" ]]; then	      # sanity check to avoid serious embarrassment
      /usr/bin/rsync -ave ssh  --exclude *.ctl.${Z} ${imgndir}/ \
         ${WEB_USER}@${WEB_SVR}.ncep.noaa.gov:${WEBDIR}/${SUFFIX}/
   fi

   ssh ${WEB_USER}@${WEB_SVR} mkdir ${WEBDIR}/${SUFFIX}/gdas
   ssh ${WEB_USER}@${WEB_SVR} ln -s ${WEBDIR}/${SUFFIX}/pngs ${WEBDIR}/${SUFFIX}/gdas/pngs
   
fi

#------------------------
# clean up $workdir
#
cd $workdir
cd ../
rm -rf $workdir

echo ""
echo "END install_glb.sh"

exit
