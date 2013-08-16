#! /bin/ksh

#------------------------------------------------------------------
#
#  make_archive.sh
#
#  Archive the data files (*.ieee_d) to HPSS.
#
#  Note that only full days are archived, so if running for the
#  00, 06, or 12z cycles the previous day will be archived.  If 
#  PDATE = yyyymmdd18 then today (PDATE) will be last day archived.
#------------------------------------------------------------------
set -ax
export list=$listvar

#------------------------------------------------------------------
#  Determine last full day capable of archiving.
#------------------------------------------------------------------
CYCLE=`echo $PDATE|cut -c9-10`
if [[ ${CYCLE} = "18" ]]; then
   LASTARCH=$PDATE
else
   LASTARCH=`$NDATE -24 $PDATE`
fi

echo LASTARCH = $LASTARCH


#------------------------------------------------------------------
#  Determine the last archived date for this source.
#------------------------------------------------------------------
shell=ksh
. /usrx/local/Modules/default/init/${shell}
`module load hpss`

# 
# Need better reference here!
HPSSDIR="/NCEPDEV/hpssuser/g01/wx20es/nbns/stats/wopr"
#
#

TDATE=$LASTARCH
TDAY=`echo $TDATE|cut -c1-8`
tar_cnt=0
cntr=0
while [[ -d ${TANKDIR}/radmon.$TDAY && $tar_cnt < 5 && $cntr < 31 ]]; do

   tar_cnt=$( htar -tf ${HPSSDIR}/radmon.${TDAY}.tar | wc -l )
   echo $tar_cnt

   if [[ $tar_cnt < 5 ]] then
      echo adding $TDAY to list:
      tar_list="$tar_list $TDAY"
   fi

   TDATE=`$NDATE -24 $TDATE`
   TDAY=`echo $TDATE|cut -c1-8`

   ((cntr=cntr+1)) 
done

echo tar_list = $tar_list

#------------------------------------------------------------------
#  Archive tar_list to hpss and the $ARCHIVE_DIR
#------------------------------------------------------------------

for tar_date in ${tar_list}; do

   htar -cvf ${HPSSDIR}/radmon.${tar_date}.tar ${TANKDIR}/radmon.${tar_date}

   if [[ $MY_MACHINE = "wcoss" ]]; then
      cp -r ${TANKDIR}/radmon.${tar_date} ${ARCHIVE_DIR}/${SUFFIX}/radmon.${tar_date}      
   fi

done

#------------------------------------------------------------------
#  Remove any directories in $ARCHIVE_DIR in excess of 60 
#------------------------------------------------------------------

if [[ $MY_MACHINE = "wcoss" ]]; then

   total=`ls -d1 ${ARCHIVE_DIR}/${SUFFIX}/radmon.* | wc -l`
   ((extra=total-61)) 

   if [[ $extra > 0 ]]; then
#      `ls -d1 ${ARCHIVE_DIR}/${SUFFIX}/radmon.* | head -n $extra | xargs rm -rf`
       `ls -d1 ${ARCHIVE_DIR}/${SUFFIX}/radmon.* | head -n +${extra} | xargs`
   fi

fi


exit
 
