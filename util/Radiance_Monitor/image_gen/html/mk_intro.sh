#!/bin/sh
set -ax

#-------------------------------------------------------------
#
# mk_intro.sh
#
# Build the intro.html file for this source.  
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
outfile=intro.html.$RAD_AREA
>$outfile


ctr=0
extra=0
row_ctr=0

echo '<!doctype html public "-//w3c//dtd html 4.0 transitional//en">' >> $outfile
echo '<html>' >> $outfile
echo '<head>' >> $outfile
echo '  <meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">' >> $outfile
echo '  <meta name="GENERATOR" content="Mozilla/4.5 [en] (Win98; U) [Netscape]">' >> $outfile
echo '  <title>Mean Bias Corrrection Coefficients</title>' >> $outfile
echo '  <base target="rightframe">' >> $outfile
echo '</head>' >> $outfile
echo '' >> $oufile

echo '<body text="#000000" bgcolor="#FFFFFF">' >> $outfile
echo '  <style type="text/case">' >> $outfile
echo '    boyd{margin-left: 10%;margin-right: 10%;} ' >> $outfile
echo '    p{ text-indent: 2em;}' >> $outfile
echo ' </style> ' >> $outfile
echo '' >> $outfile

echo '   <h2>Introduction</h2>' >> $outfile
echo '   <P>     The purposes of monitoring the data assimilation system are' >> $outfile
echo '   to ensure the proper performance of the assimilation system and' >> $outfile
echo '   to diagnose problems with the system for future improvements.' >> $outfile
echo '   Furthermore, it provides statistics for use in the assimilation' >> $outfile
echo '   system.  The statistics shown here include the bias correction' >> $outfile
echo '   and the observed-minus-simulated radiance statistics (with or' >> $outfile
echo '   without bias correction).</P>' >> $outfile
echo '' >> $outfile

echo '   <P>     The sources of the departure statistics between the observed' >> $outfile
echo '   and the simulated radiance include:' >> $outfile
echo '   (i) errors in the observed data' >> $outfile
echo '   (ii) errors in the profile obtained from the forecast model, and' >> $outfile
echo '   (iii) errors in the radiative-transfer model used to compute' >> $outfile
echo '   radiance from the forecast.</P>' >> $outfile
echo '' >> $outfile

echo '   <P> Some observation numbers may have the decimal points because they are normalized by ' >> $outfile
echo '     the cycle numbers.' >> $outfile
echo '' >> $outfile

echo '</body>' >> $outfile
echo '' >> $outfile

echo '<br>' >> $outfile
echo '<h2>Summary plots</h2>' >> $outfile
echo 'Click on the thumbnail to see the full image' >> $outfile
echo '<br>' >> $outfile
echo '<br>' >> $outfile
echo '' >> $outfile
echo '  <table border=0 cellpadding=1>' >> $outfile

#-------------------------------------------------------------
#  Add the first table row.
#
#row_end=$num_rows
row_end=$cols
if [[ $extra < $remainder ]]; then
   row_end=`expr $row_end + 1`
   extra=`expr $extra + 1`
   echo '   <tr>' >> $outfile				
fi

quote='"'

#-------------------------------------------------------------
#  Read the SORTED_LIST and construct the table elements.
#
while read line; do
   sat=`echo $line | gawk '{print $1}'`
   ins=`echo $line | gawk '{print $2}'`
   satype=`echo $line | gawk '{print $3}'`

   ctr=`expr $ctr + 1`
   echo ctr, row_end = $ctr, $row_end			# load table data info

   echo '      <td>' >> $outfile
   echo "         <center> <a href=${quote}pngs/summary/${satype}.summary.png${quote} target=${quote}fmain${quote}>" >> $outfile
   echo "            <img src=${quote}pngs/summary/${satype}.summary.png${quote} width=${quote}70${quote} height=${quote}90${quote} border=${quote}1${quote}>" >> $outfile
   echo "            </a></center>" >> $outfile
   echo "         <center>${sat} ${ins} </center>" >> $outfile
   echo "      </td>" >> $outfile
   echo "" >> $outfile

   if [[ $ctr = $row_end ]]; then	
      echo '  </tr>' >> $outfile			# end table row

      if [[ $ctr != $num_sats ]]; then			
#         row_end=`expr $row_end + $num_rows`
         row_end=`expr $row_end + $cols`
         if [[ $extra < $remainder ]]; then
            row_end=`expr $row_end + 1`
            extra=`expr $extra + 1`
         fi

         echo '  <tr>' >> $outfile			# start new table row
      fi

   fi

done < "$SORTED_LIST"

#   <td>^M
#  <center>^M
#   <a href="pngs/summary/sndrd1_g11.summary.png" target="fmain"><img src="pngs/summary/sndrd1_g11.summary.png"^M
#    width="70" height="90" border="1"></a></center>^M
#   <center>GOES-11 SNDRD1</center>^M
#   <br>^M

echo '  </table>' >> $outfile
echo '</html>' >> $outfile

echo END mk_intro.sh
echo
