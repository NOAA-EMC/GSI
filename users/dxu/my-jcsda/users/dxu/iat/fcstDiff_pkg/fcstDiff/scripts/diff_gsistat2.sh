#!/bin/sh
set -x

echo " ***** Today's date: "
date
#--compare gsistat and find missed satellite data 
#
# --- set up HPSS get for gdas gsistat file.
#
echo " ***** Starting Conditions "
export ENDATE=20081231
export STARTDATE=${1:-"2008010100"}
export CDATE=$STARTDATE
export CYC=${CDATE#????????}
export CDY=${CDATE%??}
export CMO=${CDATE%????}
echo " ***** --- Starting Date "
echo " ***** CDATE=$CDATE "
echo " ***** CYC=$CYC "
echo " ***** CDY=$CDY "
echo " ***** CMO=$CMO "
export INC=12
#
# --- set up starting with first gsistat file
#
   export GSISTAT1=gdas1.t${CYC}z.gsistat.${CDATE}
   if [ ! -s "$GSISTAT1" ] ; then
   hpsstar get /hpssprod/runhistory/rh${CDATE%??????}/${CDATE%????}/${CDATE%??}/com_gfs_prod_gdas.${CDATE}.tar ./gdas1.t${CYC}z.gsistat
   mv gdas1.t${CYC}z.gsistat gdas1.t${CYC}z.gsistat.${CDATE}
   fi
echo " ***** GSISTAT1=$GSISTAT1 "
#
sat1=`grep "o-g 01 rad"  gdas1.t${CYC}z.gsistat.${CDATE} |awk '{print $4} '`
ins1=`grep "o-g 01 rad"  gdas1.t${CYC}z.gsistat.${CDATE} |awk '{print $5} '`
#
# --- set up working directories
exp1=/climate/noscrub/glopara/archive/pru12d
exp2=/climate/noscrub/glopara/archive/prd09q1o
dump=gdas
export PARENT_DIR=${2:-/global/noscrub}
export PSLOT=${3:-diffgsistat}
export DATA=${PARENT_DIR}/${LOGNAME}/gsi/${PSLOT};mkdir -p $DATA||exit 1
rundir=/stmp/$LOGNAME/$PSLOT
mkdir -p $rundir; cd $rundir
#
# --- start loop
#

    while [[ $CDATE -lt $ENDATE ]] ; do
     export DATE2=$(ndate $INC $CDATE)
     export CYC2=${DATE2#????????}
     export GSISTAT2=gdas1.t${CYC2}z.gsistat.${DATE2}
      if [ ! -s "$GSISTAT2" ] ; then
      hpsstar get /hpssprod/runhistory/rh${DATE2%??????}/${DATE2%????}/${DATE2%??}/com_gfs_prod_gdas.${DATE2}.tar ./gdas1.t${CYC2}z.gsistat
      mv gdas1.t${CYC2}z.gsistat gdas1.t${CYC2}z.gsistat.${DATE2}
      fi
echo " ***** GSISTAT2=$GSISTAT2 "
sat2=`grep "o-g 01 rad"  gdas1.t${CYC2}z.gsistat.${DATE2} |awk '{print $4} '`
ins2=`grep "o-g 01 rad"  gdas1.t${CYC2}z.gsistat.${DATE2} |awk '{print $5}'`
#------------------------
#for cyc in 00    12   ; do
# for cyc in 00 06 12 18; do
#------------------------
 for sat in $save1; do 
 echo $sat
grep $sat1 ${rundir}/${GSISTAT1} |grep "o-g 01 rad  $ins1"  >x1
grep $sat2 ${rundir}/${GSISTAT2} |grep "o-g 01 rad  $ins2"  >x2

for sat in amsua amsub mhs airs sndr iasi ssmi hirs; do
#for sat in  amsua airs ; do

# --- looping on CDATE
#
output=$DATA/diff_${dump}_aqua.txt
###rm x1 x2
file=gsistat.${dump}.${CDATE}${cyc}

##grep $sat ${rundir}/${GSISTAT1} |grep "o-g 01 rad  aqua"  >x1
##grep $sat ${rundir}/${GSISTAT2} |grep "o-g 01 rad  aqua"  >x2
 

# 1
zero1=`eval grep $sat ${rundir}/${GSISTAT1}} |grep  "o-g 01 rad  aqua" |cut -c -36-46`
# 2
zero2=`eval grep $sat ${rundir}/${GSISTAT2}} |grep  "o-g 01 rad  aqua" |cut -c -36-46`

if [ $zero1 -gt 0 -a $zero2 -eq 0 ];then
 echo ${CDATE}${cyc} >>$output                 
 diff x1 x2 >> $output                 
fi
if [ $zero1 -eq 0 -a $zero2 -gt 0 ];then
 echo ${CDATE}${cyc} >>$output                 
 diff x1 x2 >> $output                 
fi





#------------------------
CDATE=` ${ndate_dir}/ndate +24 ${CDATE}${cyc} | cut -c1-8`
done 
#------------------------
done 
#------------------------
done 
#------------------------


exit
