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
if [[ $SUFFIX = "wopr" ]]; then
   HPSSDIR="/NCEPDEV/hpssuser/g01/wx20es/nbns/stats/wopr"
else
   HPSSDIR="/NCEPDEV/hpssuser/g01/wx20es/nbns/stats/regional/nrx"
fi

HTAR="/usrx/local/hpss/htar"
#
#

TDATE=$LASTARCH
TDAY=`echo $TDATE|cut -c1-8`
tar_cnt=0
cntr=0
while [[ -d ${TANKDIR}/radmon.$TDAY && $tar_cnt < 5 && $cntr < 31 ]]; do

   tar_cnt=$( $HTAR -tf ${HPSSDIR}/radmon.${TDAY}.tar | wc -l )
   echo "tar_cnt = $tar_cnt"

   if [[ $tar_cnt < 5 ]] then
      echo "adding $TDAY to list"
      tar_list="$tar_list $TDAY"
   fi

   TDATE=`$NDATE -24 $TDATE`
   TDAY=`echo $TDATE|cut -c1-8`

   ((cntr=cntr+1)) 
done

#echo "tar_list = $tar_list"

#------------------------------------------------------------------
#  Archive tar_list to hpss and the $ARCHIVE_DIR
#------------------------------------------------------------------

for tar_date in ${tar_list}; do

   $HTAR -cvf ${HPSSDIR}/radmon.${tar_date}.tar ${TANKDIR}/radmon.${tar_date}

done


if [[ $MY_MACHINE = "wcoss" ]]; then

   #------------------------------------------------------------------
   #  Determine the last date stored on /sss for this source.
   #------------------------------------------------------------------
   ADATE=$LASTARCH
   ADAY=`echo $ADATE|cut -c1-8`
   cntr=0
   while [[ -d ${TANKDIR}/radmon.$ADAY && $cntr < 31 ]]; do

      if [[ ! -d ${ARCHIVE_DIR}/radmon.${ADAY} ]]; then
#         echo "adding $ADAY to list"
         arch_list="$arch_list $ADAY"
      fi

      ADATE=`$NDATE -24 $ADATE`
      ADAY=`echo $ADATE|cut -c1-8`

      ((cntr=cntr+1)) 
   done

   #------------------------------------------------------------------
   #  Copy directories to the ${ARCHIVE_DIR}
   #------------------------------------------------------------------
   for arch_date in ${arch_list}; do
      cp -r ${TANKDIR}/radmon.${arch_date} ${ARCHIVE_DIR}/radmon.${arch_date}      
   done

   #------------------------------------------------------------------
   #  Remove any directories in $ARCHIVE_DIR in excess of 60 
   #------------------------------------------------------------------
   total=`ls -d1 ${ARCHIVE_DIR}/radmon.* | wc -l`
   ((extra=total-61)) 

   if [[ $extra -gt 0 ]]; then
      `ls -d1 ${ARCHIVE_DIR}/radmon.* | head -n $extra | xargs rm -rf`
   fi

   #------------------------------------------------------------------
   #  Remove any directories in $TANKDIR in excess of 60
   #------------------------------------------------------------------
   total=`ls -d1 ${TANKDIR}/radmon.* | wc -l`
   ((extra=total-61))

   if [[ $extra -gt 0 ]]; then
      `ls -d1 ${TANKDIR}/radmon.* | head -n $extra | xargs rm -rf`
   fi


   #------------------------------------------------------------------
   #  Copy the data_map.xml file to the $TOP_ARCHIVE_DIR 
   #------------------------------------------------------------------
   if [[ -e ${RADMON_PARM}/data_map.xml ]]; then
      cp -f ${RADMON_PARM}/data_map.xml ${TOP_ARCHIVE_DIR}/.
   fi 
 
fi


exit
 
