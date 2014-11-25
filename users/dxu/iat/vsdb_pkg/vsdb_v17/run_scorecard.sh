#!/bin/ksh
### This script is used to create the EMC Verification Scorecard.
### An html graphic is uploaded to the web at the end of this script.
### Users must run vsdbjob_submit.sh first with create maps set (MAKEMAPS=YES) 
### for the scorecard text files to be created which are read by this script.
### Users should only need to change the 9 variables at the top of this script.
### Written by: DaNa Carlis and Rebecca LaPorta 11/5/2013
### Update history: 
###  11/6/2014, Deyong Xu / RTi @ JCSDA, 1) initialized var "rundir" so this script could run 
###     outside vsdb main running script. 2) highlighten input and output settings for clarification.
###     3) replace "exit 88" with "continue" so script won't crush in the middle due to missing some of score
###     card files and continue to generate score card.
###  11/25/2014, Deyong Xu / RTi @ JCSDA, Fixed the column shift bug in score card 
### 

set -x
#-----------------------------------
# 1. Score card inputs:
#-----------------------------------
# a) VSDB package location
vsdbhome=${vsdbhome:-/scratch2/portfolios/NESDIS/h-sandy/noscrub/Deyong.Xu/vsdb_pkg/vsdb_v17}
# b) Score card text file location
export rundir=${rundir:-/scratch2/portfolios/NESDIS/h-sandy/noscrub/Deyong.Xu/vsdb_workspace/data/stmp/Deyong.Xu/nwpvrfy29853/acrms29853}
# c) Date range
export SDATE=${DATEST:-20130723}
export EDATE=${DATEND:-20130808}
# d) Experiment name
export mdlist=${mdlist:-"prt670 prthin1"}       ;#Can only compare 2 experiments 

#-----------------------------------
# 2. Score card inputs:
#-----------------------------------
# final scorecard location
export mapdir=${mapdir:-~/scorecard}  ; #place to save output in local machine 

#-----------------------------------------------------------
# 3. Auxillary setting not interested to most of users. 
#-----------------------------------------------------------
export webhostid=${webhostid:-"$LOGNAME"}              ;#login id on rzdm webhost
#export webhostid=${webhostid:-"wx23dc"}              ;#login id on rzdm webhost
export webhost=${webhost:-"emcrzdm.ncep.noaa.gov"}     ;#login id on webhost
export ftpdir=${ftpdir:-/home/people/emc/www/htdocs/gmb/$webhostid/vsdb}  ; #where maps are  displayed
export doftp=${doftp:-"NO"}                           ; #whether or not sent html files to scardftp 

#Calculate total number of days
y1=`echo $SDATE |cut -c 1-4 `
m1=`echo $SDATE |cut -c 5-6 `
d1=`echo $SDATE |cut -c 7-8 `
y2=`echo $EDATE   |cut -c 1-4 `
m2=`echo $EDATE   |cut -c 5-6 `
d2=`echo $EDATE   |cut -c 7-8 `
ndays=`${vsdbhome}/map_util/days.sh -a $y2 $m2 $d2 - $y1 $m1 $d1`
export ndays=`expr $ndays + 1 `

#User can find the location of scorecard text files from vsdbjob_submit.sh
scoredir=${scoredir:-$rundir/score}                    ; #location of scard text files
mkdir -p $scoredir
cd $scoredir || exit
#Undefined value
undef=-9999.0
#--------------------------------------------------------------------
## -- Users should not have to change any of the parameters below--##
## -- verification parameters
export statlist=${statlist:-"cor rms bias"}
export reglist=${reglist:-"G2PNA G2NHX G2SHX G2TRO"}
#export day=${day:-"1 3 5 6 8 10"}
export day=${day:-"1 3 5 "}

# Get number of columns per region
colno=`echo ${day} |wc -w`

#Verification scorecard html filename
htfile=scorecard.html
legfile=legend.html
mainfile=mainindex.html
cssfile=scorecard.css
rm -f $htfile $legfile $mainfile $cssfile

#Create Header for HTML FILE
cat <<EOF >> $htfile
<link type="text/css" rel="stylesheet" href="scorecard.css"/>
<table class="first" cellpadding="0.2">
 <tbody align="center">
  <!--Regions-->
  <tr>
   <th></th>
   <th></th>
   <th></th>
EOF

for reg1 in $reglist; do
if [[ $reg1 = "G2" ]] ; then
   world=$(echo "<th colspan=${colno}>Globe</th>")
elif [[ $reg1 = "G2NHX" ]] ; then
   world=$(echo "<th colspan=${colno}>N. Hemisphere</th>")
elif [[ $reg1 = "G2SHX" ]] ; then
   world=$(echo "<th colspan=${colno}>S. Hemisphere</th>")
elif [[ $reg1 = "G2TRO" ]] ; then
   world=$(echo "<th colspan=${colno}>Tropics</th>") 
elif [[ $reg1 = "G2PNA" ]] ; then
   world=$(echo "<th colspan=${colno}>N. American</th>")
fi

cat <<EOF >> $htfile
    $world
EOF
done

cat <<LSF >> $htfile
  </tr>
  <!--Days-->
   <tr>
    <td></td>
    <td></td>
    <td></td>
LSF

#How many regions will be displayed on scorecard
regcount=`echo $reglist |wc -w`
ct=1
while [[ $ct -le $regcount ]]; do

 for pday in $day; do
   if [[ $pday -eq 1 ]]; then
     sday=$(echo '<td>Day 1</td>')
   elif [[ $pday -eq 3 ]]; then
     sday=$(echo '<td style="background:#E6E6E6">Day 3</td>')
   elif [[ $pday -eq 5 ]]; then
     sday=$(echo '<td>Day 5</td>')
   elif [[ $pday -eq 6 ]]; then
     sday=$(echo '<td style="background:#E6E6E6">Day 6</td>')
   elif [[ $pday -eq 8 ]]; then
     sday=$(echo '<td>Day 8</td>')
   elif [[ $pday -eq 10 ]]; then
     sday=$(echo '<td style="background:#E6E6E6">Day 10</td>')
   fi

cat <<LSF >> $htfile
    $sday
LSF
 done
 ct=$(( $ct + 1 ))
done #End of while loop

cat <<EOF >> $htfile
   </tr>
EOF


set -A mdname $mdlist
mdnamec1=$(echo ${mdname[0]} |tr "[a-z]" "[A-Z]") 
mdnamec2=$(echo ${mdname[1]} |tr "[a-z]" "[A-Z]") 

echo ${mdnamec1}
echo ${mdnamec2}

for stat in $statlist ; do  #Loop over cor, rms, bias
if [[ $stat = "cor" ]] ; then
  statname="Anomaly Correlation"
  vnamlist="HGT WIND T PMSL"
  levlist="P250 P500 P700 P1000"
  statspan=11
elif [[ $stat = "rms" ]] ; then
  statname="RMSE"
  vnamlist="HGT WIND T"
  levlist="P10 P20 P50 P100 P200 P500 P700 P850 P1000"
  statspan=27
  rspan=9
elif [[ $stat = "bias" ]] ; then
  statname="Bias"
  vnamlist="HGT WIND T"
  levlist="P10 P20 P50 P100 P200 P500 P700 P850 P1000"
  statspan=27
  rspan=9
fi

cat <<EOF >> $htfile
   <tr>
    <th id="thside" rowspan="$statspan">$statname</th>
EOF

for vnam in $vnamlist ; do #Loop over HGT, T, U, V, WIND
if [[ $stat = "cor" && $vnam = "HGT" ]] ; then
  rspan=4
elif [[ $stat = "cor" && $vnam = "T" ]] ; then
  levlist="P250 P500 P850"
  rspan=3
elif [[ $stat = "cor" && $vnam = "U" ]] ; then
  levlist="P250 P500 P850"
  rspan=3
elif [[ $stat = "cor" && $vnam = "V" ]] ; then
  levlist="P250 P500 P850"
  rspan=3
elif [[ $stat = "cor" && $vnam = "WIND" ]] ; then
  levlist="P250 P500 P850"
  rspan=3
elif [[ $stat = "cor" && $vnam = "PMSL" ]] ; then
  levlist="MSL"
  rspan=1
fi

if [[ $vnam = "HGT" ]] ; then
  vname="Heights"
elif [[ $vnam = "PMSL" ]] ; then
  vname="MSLP"
elif [[ $vnam = "WIND" && $stat = "cor" ]] ; then
  vname="Vector Wind"
elif [[ $vnam = "WIND" && $stat = "rms" ]] ; then
  vname="Vector Wind"
elif [[ $vnam = "WIND" && $stat = "bias" ]] ; then
  vname="Wind Speed"
elif [[ $vnam = "T" ]] ; then
  vname="Temp"
elif [[ $vnam = "U" ]] ; then
  vname="U-Wind"
elif [[ $vnam = "V" ]] ; then
  vname="V-Wind"
fi

cat <<EOF >> $htfile
     <td rowspan="${rspan}">$vname</td>
EOF

for lev in $levlist ; do  #Loop over P1000, P500, P250mlist 

if [[ $lev = "P1000" ]] ; then
  lev1=$(echo $lev |cut -c 2-5)
else
  lev1=$(echo $lev |cut -c 2-4)
fi

if [[ $vnam = "PMSL" ]] ; then
  lev1=$lev
cat <<EOF >> $htfile
     <td>${lev1}</td>
EOF
else
cat <<EOF >> $htfile
     <td>${lev1}hPa</td> 
EOF

fi

for reg in $reglist ; do  #Loop over G2, NH, SH
if [[ $reg = "G2" ]] ; then
  area="Global"
elif [[ $reg = "G2NHX" ]] ; then
  area="N. Hem"
elif [[ $reg = "G2SHX" ]] ; then
  area="S. Hem"
elif [[ $reg = "G2TRO" ]] ; then
  area="Tropics"
elif [[ $reg = "G2PNA" ]] ; then
  area="North America"
fi
echo $area

for dd in $day ; do      #Loop over day 1,3,5,6,8,10


namedaily=${vnam}_${lev}_${reg}

file1=${scoredir}/score_${stat}_${namedaily}_${mdnamec1}_day${dd}.txt
file2=${scoredir}/score_${stat}_${namedaily}_${mdnamec2}_day${dd}.txt
file3=${scoredir}/score_${stat}_conflimit_${namedaily}_${mdnamec2}_day${dd}.txt

if [[ ! -s "$file1" || ! -s "$file2" || ! -s "$file3" ]] ; then
  echo "All text files do not exist to create verif scorecard" 
  echo "Check that all files exist and have size"
  echo "Check file1: " $file1
  ls -l $file1
  echo "Check file2: " $file2
  ls -l $file2
  echo "Check file3: " $file3
  ls -l $file3
  #dxu  exit 88
  continue
fi

#Set default score
score1=$undef
score2=$undef
conf1=$undef

while read line          
do           
  score1=$(echo $line | awk '{print $1}' | bc)
done < ${file1}

while read line
do
  score2=$(echo $line | awk '{print $1}' | bc)
done < ${file2}

while read line
do
  conf1=$(echo $line | awk '{print $1}' | bc)
done < ${file3}

if [[ $ndays -ge 80 ]] ;then
  alpha1=1.960  ; #95% confidence level
  alpha2=2.576  ; #99% confidence level
  alpha3=3.291  ; #99.9% confidence level
elif [[ $ndays -ge 40 && $ndays -lt 80 ]] ;then
  alpha1=2.0    ; #95% confidence level
  alpha2=2.66   ; #99% confidence level
  alpha3=3.46   ; #99.9% confidence level
elif [[ $ndays -ge 20 && $ndays -lt 40 ]] ;then
  alpha1=2.042  ; #95% confidence level
  alpha2=2.75   ; #99% confidence level
  alpha3=3.646  ; #99.9% confidence level
elif [[ $ndays -lt 20 ]] ; then
  alpha1=2.228  ; #95% confidence level
  alpha2=3.169  ; #99% confidence level
  alpha3=4.587  ; #99.9% confidence level
fi

if [[ $score1 == $undef || $score2 == $undef || $conf1 == $undef ]] ; then
  ds=$undef
else
  if [[ $stat = "bias" ]] ;then
    let ds="(abs($score1) - abs($score2)) / $conf1"
  elif [[ $stat = "rms" ]] ;then
    let ds="($score1 - $score2) / $conf1"
  else
    let ds="($score2 - $score1) / $conf1"
  fi
  let ds95="$ds"
  let ds99="$ds * $alpha1 / $alpha2"
  let ds999="$ds * $alpha1 / $alpha3"
  ds=`printf "%7.3f" $ds`
  ds95=`printf "%7.3f" $ds95`
  ds99=`printf "%7.3f" $ds99`
  ds999=`printf "%7.3f" $ds999`
fi

if (( $ds999 >= 1 )); then
  sym=$(echo '<td><font color="#009120">&#9650;</font></td>')
  state=$(echo "$mdnamec2 is better than $mdnamec1 at the 99.9% significance level")
elif (( $ds999 < 1 && $ds99 >= 1 )); then
  sym=$(echo '<td><font color="#009120">&#9652;</font></td>')
  state=$(echo "$mdnamec2 is better than $mdnamec1 at the 99% significance level")
elif (( $ds99 < 1 && $ds95 >= 1 )); then
  sym=$(echo '<td style="background:#A9F5A9"></td>')
  state=$(echo "$mdnamec2 is better than $mdnamec1 at the 95% significance level")
elif (( $ds95 > -1 && $ds95 < 1 )); then
  sym=$(echo '<td style="background:#BDBDBD"></td>')
  state=$(echo "No statistically significant difference between $mdnamec2 and $mdnamec1")
elif (( $ds95 <= -1 && $ds99 > -1 )); then
  sym=$(echo '<td style="background: #F5A9BC"></td>')
  state=$(echo "$mdnamec2 is worse than $mdnamec1 at the 95% significance level")
elif (( $ds99 <= -1 && $ds999 > -1 )); then
  sym=$(echo '<td><font color="#FF0000">&#9662;</font></td>')
  state=$(echo "$mdnamec2 is worse than $mdnamec1 at the 99% significance level")
elif (( $ds999 <= -1 && $ds999 > -100.0 )); then
  sym=$(echo '<td><font color="#FF0000">&#9660;</font></td>')
  state=$(echo "$mdnamec2 is worse than $mdnamec1 at the 99.9% significance level")
elif (( $ds999 < -100.0 )); then
  sym=$(echo '<td>M</td>')
fi

#For Anom Correlation Temperature in tropics, make table value blank
if [[ $stat = "cor" && $reg = "G2TRO" ]] ; then
  sym=$(echo '<td style="background:#58ACFA"></td>')
fi

cat <<EOFF >> $htfile
       $sym
EOFF

done #End of day loop

done #End of region loop
cat <<EOF >> $htfile
   </tr>

   <tr>
EOF

done #End of level loop

done #End of vnam HGT WIND loop

done #End of stat cor bias rmse

cat <<EOF >> $htfile
    </tbody>
   </table>
EOF


#Create the legend.html file
cat <<legend >> $legfile
<link type="text/css" rel="stylesheet" href="scorecard.css"/>
<div>
    <table class="second" cellpadding="0.2">
    <tr>
       <th colspan=2>EMC Verification Scorecard</th>
    </tr>
    <tr>
       <th colspan=2>Symbol Legend</th>
    </tr>
    <tr class="legend">
      <td><font color="#009120">&#9650;</font></td>
      <td>$mdnamec2 is better than $mdnamec1 at the 99.9% significance level</td>
    </tr>
    <tr class="legend">
      <td><font color="#009120">&#9652;</font></td>
      <td>$mdnamec2 is better than $mdnamec1 at the 99% significance level</td>
    </tr>
    <tr class="legend">
      <td style="background:#A9F5A9"></td>
      <td>$mdnamec2 is better than $mdnamec1 at the 95% significance level</td>
    </tr>
    <tr class="legend">
      <td style="background:#BDBDBD"></td>
      <td>No statistically significant difference between $mdnamec2 and $mdnamec1</td>
    </tr>
    <tr class="legend">
      <td style="background: #F5A9BC"></td>
      <td>$mdnamec2 is worse than $mdnamec1 at the 95% significance level</td>
    </tr>
    <tr class="legend">
      <td><font color="#FF0000">&#9662;</font></td>
      <td>$mdnamec2 is worse than $mdnamec1 at the 99% significance level</td>
    </tr>
    <tr class="legend">
      <td><font color="#FF0000">&#9660;</font></td>
      <td>$mdnamec2 is worse than $mdnamec1 at the 99.9% significance level
    </tr>
    <tr class="legend">
      <td style="background:#58ACFA"</td>
      <td>Not statistically relevant</td>
    </tr>
    <tr>
      <th colspan=2>Start Date: $SDATE </th>
    </tr>
    <tr>
      <th colspan=2>End Date: $EDATE </th>
    </tr>
legend

#Create css file for border
cat << CSSEND >> $cssfile
body {
        margin : 0;
        padding : 0;
        background-color : #ffffff;
        color : #000000;
        }
.first {
        border-collapse: collapse;
        border: 3px solid black;
        }
.second {
        border-collapse: collapse;
	border: 1px dashed black;
}
.legend {
        font-size: 0.75em;
}
#bold {
        font: bold 1em Times;
	}

td {
        border-collapse: collapse;
        border: 2px solid black;
}
th {
        border-collapse: collapse;
        border: 1px solid black;
        color: red;
        font-family: Garamound;
}
#thside {
        color: blue;
        font-family: Garamound;
}
CSSEND

#Create main index file
cat << MAIN >> $mainfile
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">
<html>
  <head>
    <title>EMC Verification Scorecard</title>
      <frameset cols="20%,80%">
        <frame src="$legfile">
	<frame src="$htfile">
        <link type="text/css" rel="stylesheet" href="$cssfile"/>
  </head>
      </frameset>
</html>
MAIN

cat << EOF >scoreftp
  binary
  cd $ftpdir/www
  mkdir scorecard
  cd scorecard
  put *html
  put *css
  quit
EOF

if [[ $doftp = "YES" ]] ; then
 sftp ${webhostid}@${webhost} <scoreftp
fi

#if [ -s $mapdir/www/scorecard ]; then
# cp *html $mapdir/www/scorecard/.
# cp  *css $mapdir/www/scorecard/.
#fi

# Copy core card html files $mapdir, and create $mapdir if not exist.
if [ ! -d $mapdir ]; then
   mkdir $mapdir
fi

cp *html $mapdir
cp  *css $mapdir

exit
