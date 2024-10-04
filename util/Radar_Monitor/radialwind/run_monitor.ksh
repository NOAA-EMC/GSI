#!/bin/ksh



###### USER INPUT ##########################################################
#Use any wrf_inout3 file. It's just used to help make the baseplot.
ANALFILE=/scratch4/NCEPDEV/stmp4/Donald.E.Lippi/2015103018/save_dl2rw_rwMon/wrf_inout03
#Location of stdout file from GSI -- this is the most important!
STDOUT=/scratch4/NCEPDEV/stmp4/Donald.E.Lippi/2015103018/save_dl2rw_rwMon/stdout
############################################################################

# CHECK IF FILES EXIST.
if [ ! -s ${ANALFILE} ]; then
   echo "Analysis file does not exist: $ANALFILE"
   echo "exiting..."
   exit
fi
if [ ! -s ${STDOUT} ]; then
   echo "Stdout file does not exist: $STDOUT"
   echo "exiting..."
   exit
fi

#  FIND THE ANALYSIS TIME.
Date=`awk '/GESINFO:  Analysis date is/' $STDOUT`
YYYY=`echo $Date | cut -f 5 -d ' '`
MM=`echo $Date | cut -f 6 -d ' '`
DD=`echo $Date | cut -f 7 -d ' '`
HH=`echo $Date | cut -f 8 -d ' '`
ANAL_TIME=${YYYY}${MM}${DD}${HH}
echo $Date
echo $ANAL_TIME

#  INITIALIZE AN EMPTY ARRAY.
RADARLIST=""
typeset -A USEDRADARARRAY


#  READ IN THE FULL RADAR LIST FROM FIX DIRECTORY.
typeset -A FULLRADARARRAY
total_radars=0
file="${PWD}/radar_list"
while IFS= read -r line
do
    (( total_radars=$total_radars+1 ))
    FULLRADARARRAY[$total_radars]=`echo "$line"`
done <"$file" 
echo 'Total radars' $total_radars 
numradars=`awk '/RADAR_BUFR_READ_ALL:  num_radars_0 =/' $STDOUT | cut -f 2 -d '='`
num=1
numNotInConus=0
 while [[ num -le $numradars ]]; do
    if [ $num -lt 10 ]; then
        radari=`awk '/master list radar   '${num}'/' $STDOUT | cut -f 2 -d '='`
    elif [ $num -lt 100 ]; then
        radari=`awk '/master list radar  '${num}'/' $STDOUT | cut -f 2 -d '='`
    elif [ $num -lt 200 ]; then
        radari=`awk '/master list radar '${num}'/' $STDOUT | cut -f 2 -d '='`
    fi
    USEDRADARARRAY[${num}]=$radari 
    echo $num ${USEDRADARARRAY[$num]}
    # CHECK IF STATION IS IN THE CONUS.
    SID_1stLetter=`echo ${USEDRADARARRAY[$num]} | cut -f 1 -d ' ' | cut -c 1`
    if [ $SID_1stLetter != 'K' ]; then
        (( numNotInConus=numNotInConus+1 ))
    fi
    (( num=$num+1 ))
done

#  PARSE THE USED RADARS INFORMATION INTO SID/LAT/LON/HGT/NOB -- FROM STDOUT FILE.
(( numradars=numradars - numNotInConus ))
num=1
while [[ $num -le $numradars ]]; do
    sid[$num]=`echo ${USEDRADARARRAY[$num]} | cut -f 1 -d ' '`
    lat[$num]=`echo ${USEDRADARARRAY[$num]} | cut -f 2 -d ' '`
    lon[$num]=`echo ${USEDRADARARRAY[$num]} | cut -f 3 -d ' '`
    hgt[$num]=`echo ${USEDRADARARRAY[$num]} | cut -f 4 -d ' '`
    nob[$num]=`echo ${USEDRADARARRAY[$num]} | cut -f 5 -d ' '`
    (( num=$num+1 ))
done

#  PARSE THE FULL RADARS INFORMATION INTO SID/LAT/LON/HGT/NOB -- FROM RADAR_LIST.
fnum=1
while [[ $fnum -le $total_radars ]]; do
    echo $fnum ${FULLRADARARRAY[$fnum]}
    fsid[$fnum]=`echo ${FULLRADARARRAY[$fnum]} | cut -f 1 -d ' '`
    flat[$fnum]=`echo ${FULLRADARARRAY[$fnum]} | cut -f 2 -d ' '`
    flon[$fnum]=`echo ${FULLRADARARRAY[$fnum]} | cut -f 3 -d ' '`
    fhgt[$fnum]=`echo ${FULLRADARARRAY[$fnum]} | cut -f 4 -d ' '`
    (( fnum=$fnum+1 ))
done

echo $numradars

#  PASS ALL THE VARIABLES AND ARRAYS TO THE PLOTTING UTILITY.
python plot_radars.py $ANAL_TIME $ANALFILE $numradars $total_radars ${sid[*]} ${lat[*]} ${lon[*]} ${hgt[*]} ${nob[*]} ${fsid[*]} ${flat[*]} ${flon[*]} ${fhgt[*]}

#  MOVE MOST RECENT FIGURE TO A SAFE LOCATION.
final=`ls -t *.png | head -1` 
mkdir -p ./${ANAL_TIME}
#mv `ls -t *.png | head -1` ./${ANAL_TIME}/.
mv $final ./${ANAL_TIME}/.

PWD=`pwd`

echo "Your finished product is located: ${PWD}/${ANAL_TIME}/${final}"


