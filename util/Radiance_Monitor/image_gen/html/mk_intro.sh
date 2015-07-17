#!/bin/sh
set -ax

#-------------------------------------------------------------
#
# mk_intro.sh
#
# Customize the intro.html file for this source.  The basic
# idea is to copy the contents of the intro.html file and 
# modify the <table> element to match the satype list for this 
# data source and evenly distribute the thumbnail images in 4 
# rows.
#
#-------------------------------------------------------------

echo
echo BEGIN mk_intro.sh

#-------------------------------------------------------------
#  Both SORTED_LIST, SATYPE, and RAD_AREA should be exported from 
#  the install_html.sh script.  Exit if they are not defined.
#
if [[ ${#SORTED_LIST} -eq 0 ]]; then
   echo 'ERROR --> $SORTED_LIST is empty, unable to generate intro.html.$RAD_AREA file.'
   echo '  exiting mk_intro.sh'
   exit
fi

if [[ ${#SATYPE} -eq 0 ]]; then
   echo 'ERROR --> $SORTED_LIST is empty, unable to generate intro.html.$RAD_AREA file.'
   echo '  exiting mk_intro.sh'
   exit
fi

if [[ ${#RAD_AREA} -eq 0 ]]; then
   echo 'ERROR --> $RAD_AREA is empty, unable to generate intro.html.$RAD_AREA file.'
   echo '  exiting mk_intro.sh'
   exit
fi


#-------------------------------------------------------------
#  Cound the number of sat/instrument sources in the SATYPE
#  list.  Using num_rows of 4 figure out how many columns 
#  are in each row.
#
ctr=0
for sat in $SATYPE; do
   ctr=`expr $ctr + 1`
done

num_rows=4
remainder=`expr $ctr % $num_rows`
cols=`expr $ctr / $num_rows`
num_sats=$ctr

echo "  ctr       = $ctr"
echo "  cols      = $cols"
echo "  remainder = $remainder"
echo "  num_sats  = $num_sats"

#-------------------------------------------------------------
#  Build the intro.html file.
#
outfile=intro.html
>$outfile

infile=intro.html.stock
table=tmp_table.txt
>$table

ctr=0
extra=0
row_ctr=0


echo '  <table>' >> $table

#-------------------------------------------------------------
#  Add the first table row.
#
row_end=$cols
if [[ $extra < $remainder ]]; then
   row_end=`expr $row_end + 1`
   extra=`expr $extra + 1`
   echo '   <tr>' >> $table				
fi

quote='"'

#-------------------------------------------------------------
#  Read the SORTED_LIST and construct the table.
#
while read line; do
   sat=`echo $line | gawk '{print $1}'`
   ins=`echo $line | gawk '{print $2}'`
   satype=`echo $line | gawk '{print $3}'`

   ctr=`expr $ctr + 1`
   echo ctr, row_end = $ctr, $row_end			# load table data info

   echo '      <td>' >> $table
   echo "         <a href=${quote}./plot_summary.html?sat=${satype}${quote} target=${quote}fmain${quote}>" >> $table
   echo "            <img src=${quote}pngs/summary/${satype}.summary.png${quote}>" >> $table
   echo "         </a>" >> $table
   echo "         <div>${sat} ${ins} </div>" >> $table
   echo "      </td>" >> $table
   echo "" >> $table

   if [[ $ctr = $row_end ]]; then	
      echo '  </tr>' >> $table				# end table row

      if [[ $ctr != $num_sats ]]; then			
#         row_end=`expr $row_end + $num_rows`
         row_end=`expr $row_end + $cols`
         if [[ $extra < $remainder ]]; then
            row_end=`expr $row_end + 1`
            extra=`expr $extra + 1`
         fi

         echo '  <tr>' >> $table				# start new table row
      fi

   fi

done < "$SORTED_LIST"

#echo '  </table>' >> $table

#----------------------------------------------------------------
#  process stock intro.html file and add in the customized table
#
table_start=0
table_end=0
table_dump=0

#--------------------------------------------------------------
#  Edit the html files to add the platform table to each.
#
echo "processing ${infile}"

   #  copy the $file from start to <table> tag
   sed -e '/<table>/,$d' ${infile} > ${outfile}

   #  add the $PLATFORM_TBL (built above)
   `cat $table >> ${outfile}`

   #  copy the $file from 'END_TABLE_INSERT' comment to end
   sed -n '/<\/table>/,$p' ${infile} >> ${outfile}


#while read line; do
#  echo $line
#  test_start=`echo $line | grep "<table>"`
#  test_end=`echo $line | grep "</table>"`
##  echo "test_start len = ${#test_start}
##  echo "test_end len = ${#test_end}
#
#  if [[ $table_start -eq 0 && ${#test_start} -gt 0 ]]; then
#     table_start=1
#  fi
#
#  if [[ ${table_start} -eq 0 || ${table_end} -eq 1 ]]; then
#     echo $line >> $outfile 
#  elif [[ ${table_start} -eq 1 && ${table_dump} -eq 0 ]]; then
#     cat $table >> $outfile
#     table_dump=1
#  fi 
# 
#  if [[ $table_end -eq 0 && ${#test_end} -gt 0 ]]; then
#     table_end=1
#  fi
#
#done < $infile


echo END mk_intro.sh
echo
