#!/bin/sh
set -x

#--------------------------------------------------------------
# William O'Connor, Oct 5, 2005
#--------------------------------------------------------------
# This script was written by Tim Marchok.  It will read your 
# atcf track file and loop through and insert your tracks one 
# by one into the regular NHC atcf file.  A new set of atcf 
# files will be created and placed into the directory you 
# specify below in "newdir".  Unless you don't mind overwriting 
# the original NHC track files, make sure "adeckdir" is 
# different from "newdir".
#
# BASIN    = AL or EP, for the basin you are verifying (you should
#            only do one or the other when verifying. Mixing basins
#            for verification gives you useless results).
# yyyy     = year for the data you have.  If you have data for more
#            than 1 year, you need to run this script more than once.
# adeckdir = directory containing original NHC atcf track files
#            or the most recent trak file you will be adding to 
# mytracks = full path name of file with your test atcf track files
# rundir   = directory from which you are running this script
# newdir   = directory which will contain the new, output atcf files 
#            that have the original NHC tracks plus your tracks 
#            added in.  This should be different from adeckdir to 
#            avoid overwriting the original NHC track files.

export exp=${exp:-${1:-prs39h}}
export BASIN=${BASIN:-${2:-AL}}
export yyyy=${yyyy:-${3:-2005}}
export adeckdir=${adeckdir:-${4:-/stmp/$LOGNAME/tracks/tpctrack}}
export mytracks=${mytracks:-${5:-/stmp/$LOGNAME/tracks/$exp/atcfunix.$exp.$yyyy}}
export rundir=${execdir:-${6:-/stmp/$LOGNAME/tracks/$exp}}
export newdir=$rundir


#**************************************************
#**************************************************
# ----------------------------------------------------------------- #
# ---------  NOTHING BELOW HERE SHOULD NEED TO BE CHANGED --------- #
# ----------------------------------------------------------------- #

cd $rundir

#checkdir=`ls -la ${newdir}/*.dat | wc -l`
# if [ $checkdir -gt 0 ]; then
#   echo " "
#   echo " !!! NOTE: The directory which will contain your new atcf files"
#   echo " !!!       (${newdir})"
#   echo " !!!       is not empty.  This means that you will end up using"
#   echo " !!!       the files in this new atcf directory as your original"
#   echo " !!!       files, and you may end up with duplicate entries for "
#   echo " !!!       a given forecast time, especially if you are using the"
#   echo " !!!       same model ID (e.g., 72UDFX) as you did in a previous "
#   echo " !!!       run.  You may want to delete these new atcf files and"
#   echo " !!!       then rerun this script.  Do you want to continue? (n/y)"
#   echo "               "
#   echo "     ---->   \c"
# 
#   read ccont
#  
#   if [ ${#ccont} -eq 0 ]; then
#     ccont=y
#   fi
# 
#   if [ ${ccont} = 'y' -o ${ccont} = 'Y' ]; then
#     echo " "
#   else
#     echo " "
#     echo "Exiting...."
#     echo " "
#     exit 0
#   fi
# fi

basin=` echo $BASIN | tr '[A-Z]' '[a-z]'`

# grep the storms for the basin you want to verify out of your
# atcf file that you've provided (you don't want to mix basins
# when you verify).  Eliminate 80- and 90-level storms with awk....

grep ${BASIN} ${mytracks} | awk 'substr($0,5,1) !~ /[8-9]/ {print $0}' \
    >${rundir}/${basin}.mytracks


while read atcfrec
do

  atcfdate=` echo "$atcfrec" | cut -c9-18`
  stormnum=` echo "$atcfrec" | cut -c5-6`

  echo " +myrec = $atcfrec"

  ainsertfile=a${basin}${stormnum}${yyyy}.dat

# If we have already created a new atcf file with at least 1 new
# track added to it, then that is the atcf file that we want to 
# modify (as opposed to going back and reading from the original
# atcf file, in which case we would lose all the new tracks that
# were added by previous iterations of this loop).

  if [ -s ${newdir}/$ainsertfile ]; then
    trkdir=${newdir}
  else
    trkdir=${adeckdir}
  fi


# Make sure that the NHC atcf file has the CARQ/date in it for
# which you have a forecast.  Check to see if we have a CARQ 
# card for the given time.  A CARQ card is the card containing
# the analysis data (provided by NHC).

  checkdate=` grep CARQ ${trkdir}/$ainsertfile | grep $atcfdate | wc -l`

  if [ $checkdate -ge 1 ]; then

#   Find the record number where your atcf date first appears in the
#   NHC atcf file.  We will then add 1 to that record number (so that
#   we include the CARQ card), then sed all the records from
#   the beginning of the file to that point, then echo in our new atcf
#   record, and then sed in all the remaining records from the original
#   atcfunix file, and create a new atcfunix file.  In this way, we have 
#   inserted our atcfunix record.

    afile=${trkdir}/$ainsertfile
    startrec=` awk 'substr($0,9,10) ~ /'${atcfdate}'/ {print NR}' ${afile} | \
               sed -n "$"p`

    endblock1=${startrec}
    let startblock2=startrec+1

    sed -n 1,${endblock1}p $afile     >${rundir}/tempatcf.dat
    echo "${atcfrec}"                >>${rundir}/tempatcf.dat
    sed -n ${startblock2},\$p $afile >>${rundir}/tempatcf.dat

    mv ${rundir}/tempatcf.dat ${newdir}/$ainsertfile

  else

    echo " !!! NOTE: NO MATCHING RECORDS FOR DATE $atcfdate in $ainsertfile"

  fi

done <${rundir}/${basin}.mytracks
