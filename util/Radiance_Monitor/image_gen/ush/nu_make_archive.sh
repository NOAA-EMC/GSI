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
#PDATE=2015101312
#SUFFIX=wopr
#RAD_AREA=glb
#this_dir=`pwd`
#top_parm=${this_dir}/../../parm
#
#export RADMON_VERSION=${RADMON_VERSION:-${top_parm}/radmon.ver}
#if [[ -s ${RADMON_VERSION} ]]; then
#   . ${RADMON_VERSION}
#else
#   echo "Unable to source ${RADMON_VERSION} file"
#   exit 2
#fi
#
#export RADMON_CONFIG=${RADMON_CONFIG:-${top_parm}/RadMon_config}
#
#if [[ -s ${RADMON_CONFIG} ]]; then
#   . ${RADMON_CONFIG}
#else
#   echo "Unable to source ${RADMON_CONFIG}"
#   exit
#fi
#
#if [[ -s ${RADMON_USER_SETTINGS} ]]; then
#   . ${RADMON_USER_SETTINGS}
#else
#   echo "Unable to source ${RADMON_USER_SETTINGS}"
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

# 
# Need better reference here!
if [[ $SUFFIX = "wopr" ]]; then
   HPSSDIR="/2year/NCEPDEV/emc-da/Edward.Safford/nbns/stats/wopr"
elif [[ $SUFFIX = "pr4dev" ]]; then 
   HPSSDIR="/2year/NCEPDEV/emc-da/Edward.Safford/nbns/stats/pr4dev"
elif [[ $SUFFIX = "4devb" ]]; then 
   HPSSDIR="/2year/NCEPDEV/emc-da/Edward.Safford/nbns/stats/4devb"
elif [[ $SUFFIX = "nrx" ]]; then
   HPSSDIR="/2year/NCEPDEV/emc-da/Edward.Safford/nbns/stats/regional/nrx"
fi

HTAR="/usrx/local/hpss/htar"

TDATE=$LASTARCH
TDAY=`echo $TDATE|cut -c1-8`
tar_cnt=0
cntr=0
max_list=0
while [[ -d ${TANKDIR}/radmon.$TDAY && $max_list -lt 5 && $cntr -lt 31 ]]; do

   tar_cnt=$( $HTAR -tf ${HPSSDIR}/radmon.${TDAY}.tar | wc -l )
   echo "tar_cnt = $tar_cnt"

   if [[ $tar_cnt < 5 ]] then
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

   $HTAR -cvf ${HPSSDIR}/radmon.${tar_date}.tar ${TANKDIR}/radmon.${tar_date}

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
   dev_dirs=`ls $TANKverf`
   prod_dirs=`ssh $LOGNAME@$PROD "ls $TANKverf"`

   #------------------------------------------------------------------
   #  Throw out anything later than LASTARCH in dev_dirs so we don't
   #  end up with a partial directory on the prod machine.   
   #------------------------------------------------------------------
   nu_devdirs=""
   for tdir in ${dev_dirs}; do
      if [[ $tdir != "info" ]]; then
         test_date=`echo $tdir | gawk -F. '{print $2}'`
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
   cd $TANKverf

   for tdir in ${xfer_list}; do
      tarfile="$tdir.tar"
      
      tar -cvf $tarfile $tdir    
      scp ./$tarfile ${LOGNAME}@${PROD}:${TANKverf}/$tarfile

      ssh ${LOGNAME}@${PROD} "cd $TANKverf && tar -xvf ./$tarfile && rm -f ./$tarfile"
      rm ./$tarfile
   done 
   cd $home

   #------------------------------------------------------------------
   #  Remove any directories in $TANKDIR in excess of 120
   #------------------------------------------------------------------
   total=`ls -d1 ${TANKDIR}/radmon.* | wc -l`
   ((extra=total-121))

   if [[ $extra -gt 0 ]]; then
      `ls -d1 ${TANKDIR}/radmon.* | head -n $extra | xargs rm -rf`
   fi

fi

exit
 
