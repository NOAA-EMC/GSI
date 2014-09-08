#!/usr/bin/ksh

# This script plot tracks of individual storms using GrADS interactive display.
# 1. Developed by Tim Marchok (timothy.marchok@noaa.gov)
# 2. Modified by Fanglin Yang (Jan 2009)
#
# USAGE:  trakplot.sh <basin>
# where basin = "al" or "ep"  (al is the default)
#

basin=${1:-ep}

echo " "
echo "+++ Basin to be used is ${basin}"
echo " "

#-----------------------------------------------
chost=`echo $(hostname) |cut -c 1-1`

export exp=prhs13

if [ $chost = f ]; then
 export gradsibmv8=/apps/grads/2.0.a9/bin/grads 
 export GADDIR=/apps/grads/2.0.1a/data
 export scrdir=/scratch2/portfolios/NCEPDEV/global/save/Fanglin.Yang/VRFY/hurtrack/plot
 export netdir=/scratch2/portfolios/NCEPDEV/stmp/$LOGNAME/track/$exp/
elif [ $chost = t -o $chost = g ]; then
 export gradsibmv8=/usrx/local/GrADS/2.0.2/bin/grads
 export GADDIR=/usrx/local/GrADS/2.0.2/lib
 export arcdir=/global/save/Fanglin.Yang/VRFY/hurtrack/arch_trak
 export scrdir=/global/save/Fanglin.Yang/VRFY/hurtrack/plot                
 export netdir=/stmpd2/$LOGNAME/track/${exp}/
fi


if [ ! -d $netdir ]; then mkdir -p $netdir; fi
cd $netdir
cp -p ${scrdir}/*  .
cp -p ${arcdir}/${exp}/*  .    

${gradsibmv8} -cl "run trakplot.gs ${LOGNAME} ${basin} ${netdir}"



