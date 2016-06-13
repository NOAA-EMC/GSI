#! /bin/ksh

#------------------------------------------------------------------
#
#  nu_make_archive.sh
#
#  Archive the data files (*.ieee_d) to HPSS.
#
#  Note that only full days are archived, so if running for the
#  00, 06, or 12z cycles the previous day will be archived.  If 
#  PDATE = yyyymmdd18 then today (PDATE) will be last day archived.
#------------------------------------------------------------------
set -ax
export list=$listvar

#--------------------------------------------------------------------
# Run config files to load environment variables,
# set default plot conditions
#--------------------------------------------------------------------
##TESTING BRACKET BEGIN
#PDATE=2015101412
#SUFFIX=GDAS

#this_dir=`pwd`
#top_parm=${this_dir}/../../parm
#
#export MINMON_VERSION=${MINMON_VERSION:-${top_parm}/MinMon.ver}
#if [[ -s ${MINMON_VERSION} ]]; then
#   . ${MINMON_VERSION}
#else
#   echo "Unable to source ${MINMON_VERSION} file"
#   exit 2
#fi

#export MINMON_CONFIG=${MINMON_CONFIG:-${top_parm}/MinMon_config}
#
#if [[ -s ${MINMON_CONFIG} ]]; then
#   . ${MINMON_CONFIG}
#else
#   echo "Unable to source ${MINMON_CONFIG}"
#   exit
#fi
#
#MINMON_USER_SETTINGS=${MINMON_USER_SETTINGS:-${top_parm}/MinMon_user_settings}
#if [[ -s ${MINMON_USER_SETTINGS} ]]; then
#   . ${MINMON_USER_SETTINGS}
#else
#   echo "Unable to source ${MINMON_USER_SETTINGS}"
#   exit
#fi
##TESTING BRACKET END


#------------------------------------------------------------------
#  Determine last full day capable of archiving.
#------------------------------------------------------------------
CYCLE=`echo $PDATE|cut -c9-10`
if [[ ${CYCLE} = "18" ]]; then
   LASTARCH=$PDATE
else
   LASTARCH=`$NDATE -24 $PDATE`
fi

LAST_DAY=`echo $LASTARCH|cut -c1-8`
echo LAST_DAY = $LAST_DAY

#------------------------------------------------------------------
#  Determine the last archived date for this source.
#------------------------------------------------------------------
shell=ksh
. /usrx/local/Modules/default/init/${shell}
`module load hpss`

## 
## Need better reference here!
hpss2yr="/NCEPDEV/emc-da/2year"
if [[ $MINMON_SUFFIX = "GDAS" ]]; then
   HPSSDIR="${hpss2yr}/${LOGNAME}/nbns/stats/GDAS"
elif [[ $MINMON_SUFFIX = "GFS" ]]; then
   HPSSDIR="${hpss2yr}/${LOGNAME}/nbns/stats/GFS"
elif [[ $MINMON_SUFFIX = "4devb" ]]; then 
   HPSSDIR="${hpss2yr}/${LOGNAME}/nbns/stats/4devb"
elif [[ $MINMON_SUFFIX = "NDAS" ]]; then
   HPSSDIR="${hpss2yr}/${LOGNAME}/nbns/stats/regional/NDAS"
elif [[ $MINMON_SUFFIX = "RAP" ]]; then
   HPSSDIR="${hpss2yr}/${LOGNAME}/nbns/stats/regional/RAP"
fi

HTAR="/usrx/local/hpss/htar"
M_TANKDIR=${M_TANKverf}/stats/${MINMON_SUFFIX}
TDATE=$LASTARCH
TDAY=`echo $TDATE|cut -c1-8`
tar_cnt=0
cntr=0
max_list=0
while [[ -d ${M_TANKDIR}/minmon.$TDAY && $max_list -lt 5 && $cntr -lt 31 ]]; do

   tar_cnt=$( $HTAR -tf ${HPSSDIR}/minmon.${TDAY}.tar | wc -l )
   echo "tar_cnt = $tar_cnt"

   if [[ $tar_cnt -lt 5 ]] then
      echo "adding $TDAY to list"
      tar_list="$tar_list $TDAY"
      ((max_list=max_list+1))
   fi

   TDATE=`$NDATE -24 $TDATE`
   TDAY=`echo $TDATE|cut -c1-8`

   ((cntr=cntr+1)) 
done

echo "tar_list = $tar_list"

#------------------------------------------------------------------
#  Archive tar_list to hpss and the $ARCHIVE_DIR
#------------------------------------------------------------------

for tar_date in ${tar_list}; do

   $HTAR -cvf ${HPSSDIR}/minmon.${tar_date}.tar ${M_TANKDIR}/minmon.${tar_date}

done



#------------------------------------------------------------------
#------------------------------------------------------------------
#  Add new directories to the prod machine
#------------------------------------------------------------------
#------------------------------------------------------------------
if [[ $MY_MACHINE = "wcoss" ]]; then

   #------------------------------------------------------------------
   #  Generate lists of directories locally and on the prod machine
   #------------------------------------------------------------------
   PROD=`cat /etc/prod`
   dev_dirs=`ls -d1 $M_TANKDIR/minmon*`
   prod_dirs=`ssh $LOGNAME@$PROD "ls -d1 $M_TANKDIR/minmon*"`

   #------------------------------------------------------------------
   #  Throw out anything later than LASTARCH in dev_dirs so we don't
   #  end up with a partial directory on the prod machine.   
   #------------------------------------------------------------------
   nu_devdirs=""
   for tdir in ${dev_dirs}; do
      if [[ $tdir != "info" ]]; then
         test_date=`echo $tdir | gawk -F. '{print $NF}'`
         echo test_date=$test_date
         if [[ $test_date -le $LAST_DAY ]]; then
            nu_devdirs="$nu_devdirs $tdir"
         fi
      fi   
   done
   echo nu_devdirs = $nu_devdirs
   dev_dirs=$nu_devdirs

   #------------------------------------------------------------------
   #  Compare dev_dirs and prod_dirs.  Generate a list of any missing 
   #  directories in prod_dirs.
   #------------------------------------------------------------------
   xfer_list=""
   for tdir in ${dev_dirs}; do
      test=`echo $prod_dirs | grep $tdir`
      if [[ $test = "" ]]; then
        xfer_list="$xfer_list $tdir"
      fi
   done   
   echo "xfer_list = $xfer_list"
 
   #------------------------------------------------------------------
   #  For all entries in the xfer_list tar the directory, copy it to
   #  the prod machine, unpack it, rm both copies of the tar file 
   #------------------------------------------------------------------
   home=`pwd`
   cd $M_TANKDIR

   for tdir in ${xfer_list}; do
#      tarfile="$tdir.tar"
      mmdir=`echo $tdir | gawk -F/ '{print $NF}'`
      tarfile=${mmdir}.tar
   
      tar -cvf ${tarfile} ${mmdir}
      scp ./$tarfile ${LOGNAME}@${PROD}:${M_TANKDIR}/$tarfile

      ssh ${LOGNAME}@${PROD} "cd $M_TANKDIR && tar -xvf ./$tarfile && rm -f ./$tarfile"
      rm ./$tarfile
   done 
   cd $home

   #------------------------------------------------------------------
   #  Remove any directories in $M_TANKDIR in excess of 120
   #------------------------------------------------------------------
   total=`ls -d1 ${M_TANKDIR}/minmon.* | wc -l`
   ((extra=total-121))

   if [[ $extra -gt 0 ]]; then
      `ls -d1 ${M_TANKDIR}/minmon.* | head -n $extra | xargs rm -rf`
   fi

fi

exit
 
