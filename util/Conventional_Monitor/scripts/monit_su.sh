#!/bin/sh
#
# monitoring
#

set -ax


#--- check date

#if [ ! "$CDATE" ]; then
# echo 'ERROR: NO CDATE'
# exit
#i



#--- print log

#cho `date` "> $CDATE" [`basename $0`] >> $SCRIPTS/dasim.log
exp=$1
export CDATE=$2
export GDATE=$(/nwprod/util/exec/ndate -06 $CDATE)
export SCRIPTS=$3
export COMOUT=$4
#exp=test
#export CDATE=2005101100
#export GDATE=$(/nwprod/util/exec/ndate -06 $CDATE)
#export DATA=/ptmpp1/wx20es/test
#export SCRIPTS=/u/wx20es/home/gsiexp
#export CDUMP=gdas
#export COMOUT=$DATA

#--- parameters

DATA=/ptmpp1/wx20es/monit/$exp
mkdir -p $DATA
export WSHOME=/export/lnx42/wd20xs/gsiobr/$exp/monit
export GSCRIPTS=/u/wx20es/home/convweb/monit_gscripts
export WSSAVE=$WSHOME/save
export WSMAP=$WSHOME/map
export WSUSER=wd20xs
export WS=lnx42.ncep.noaa.gov


export ZTIME=`echo $CDATE |cut -c9-10`
export SDATE=`echo $CDATE |cut -c1-8`00


export FIGHOME=$DATA/monit

export HOMMAP=$FIGHOME/map
export FIGMAP=${CDATE}
export TARMAP=map.${CDATE}.tar
export ZIPMAP=${TARMAP}.gz



#--- preparation
  echo "yes" | ssh -l $WSUSER $WS "date"
  ssh -l $WSUSER $WS "mkdir -p ${WSSAVE}"
#  ssh -l $WSUSER $WS "mkdir -p ${WSANAL}"
#  ssh -l $WSUSER $WS "mkdir -p ${WSSTAT}"
  ssh -l $WSUSER $WS "mkdir -p ${WSMAP}"


# rsh $SP "ssh -l $WSUSER $WS 'mkdir -p ${WSSAVE} '"
# rsh $SP "ssh -l $WSUSER $WS 'mkdir -p ${WSANAL} '"
# rsh $SP "ssh -l $WSUSER $WS 'mkdir -p ${WSSTAT} '"
# rsh $SP "ssh -l $WSUSER $WS 'mkdir -p ${WSMAP} '"


##--- Tb horizontal map for humidity

cd $SCRIPTS

mkdir -p $HOMMAP/$FIGMAP
cd $HOMMAP/$FIGMAP
#cp $COMOUT/pgbanl.$CDATE .
#cp $COMOUT/pgbf06.$GDATE .
#cp  $COMOUT/pgbanl.${CDATE}.idx .
#cp  $COMOUT/pgbf06.${GDATE}.idx .
#cp $COMOUT/anal.ctl anal.ctl
#cp $COMOUT/guess.ctl guess.ctl 
grib2ctl.pl pgbanl.$CDATE > anal.ctl
  gribmap -i anal.ctl -0
  grib2ctl.pl -verf pgbf06.$GDATE > guess.ctl
  gribmap -i guess.ctl

echo 'quit' | grads -blc $GSCRIPTS/monit_anal_su_global.gs
rm -f *gr
rm -f *ctl

  cd $HOMMAP
  tar cvf $TARMAP $FIGMAP
  gzip -f $TARMAP

cat << EOF > convert.sh
  #! /bin/sh
  set -evx
  cd $WSMAP
  gzip -cd $WSSAVE/$ZIPMAP > tmp.tar
  tar xvf tmp.tar
  rm tmp.tar
#  cd $WSMAP/$FIGMAP
#  /usr/X11R6/bin/mogrify -density 96 -rotate 90 -colors 128 -quality 100 -format png *.eps
#  rm *.eps
EOF

#  rsh $SP "scp $HOMMAP/$ZIPMAP             ${WSUSER}@${WS}:${WSSAVE}"
#  rsh $SP "scp $HOMMAP/convert.sh ${WSUSER}@${WS}:${WSMAP}"
#  rsh $SP "ssh -l $WSUSER $WS 'cd ${WSMAP} ; at -f ./convert.sh now '"
  scp $HOMMAP/$ZIPMAP             ${WSUSER}@${WS}:${WSSAVE}
 scp $HOMMAP/convert.sh ${WSUSER}@${WS}:${WSMAP}
 ssh -l $WSUSER $WS "cd ${WSMAP} ; at -f ./convert.sh now "


  rm -rf $HOMMAP/$FIGMAP
  rm -rf $HOMMAP/$ZIPMAP
  rm -f  convert.sh



#--- end of script

exit
