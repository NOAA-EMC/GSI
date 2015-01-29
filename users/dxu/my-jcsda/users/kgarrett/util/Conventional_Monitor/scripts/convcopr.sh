#!/bin/ksh

#------------------------------------------------------------------
#
#  convcopr.sh
#
#  This script copies the necessary data files to the working
#  directory and calls 2 child scripts to process the Cmon data.
#  
#  Note:  This file should be renamed to take the copr out of the
#         picture.
#
#------------------------------------------------------------------
set -ax
#export list=$list0

echo STMP=$STMP


#------------------------------------------------------------------
# Set up working directory.
export DATDIRL=${STMP}/datconv_$SUFFIX
rm -rf $DATDIRL
mkdir -p $DATDIRL
cd $DATDIRL

#tmpdir=${STMP}/conv${SUFFIX}.$PDATE
#export DATDIRL=/stmpp1/${LOGNAME}/datconv_$SUFFIX
#rm -rf $tmpdir
#mkdir -p $tmpdir
#cd $tmpdir


#--------------------------------------------------------------------
#   Set environment variables to export to subsequent scripts
export nreal_ps=17
export nreal_q=18
export nreal_t=17
export nreal_uv=21


#--------------------------------------------------------------------
# Copy data files file to local data directory.  Untar cnvstat file.  
# Change DATDIR definition
#
GDATE=`$NDATE -6 $PDATE`
gsdate=`echo $GDATE|cut -c1-8`
GCYA=`echo $GDATE|cut -c9-10`


$NCP $cnvstat $DATDIRL/cnvstat.$PDATE
$NCP $pgrbanl $DATDIRL/pgbanl.$PDATE
$NCP $pgrbf06 $DATDIRL/pgbf06.$GDATE

tar -xvf $DATDIRL/cnvstat.$PDATE
$UNCOMPRESS ${DATDIRL}/*.${Z}

rm ${DATDIRL}/cnvstat.$PDATE

export DATDIR=$DATDIRL

if [[ $MY_MACHINE = "wcoss" ]]; then

   #---------------------------------------------------------------
   #  operational GDAS still supports grib files; Russ's parallels
   #  only produce grib2 files.  This is probably the future.
   #
   if [[ $SUFFIX = "copr" ]]; then
      ${SCRIPTS}/grib2ctl.pl pgbanl.$PDATE > anal.ctl
      gribmap -i anal.ctl -0
      ${SCRIPTS}/grib2ctl.pl -verf pgbf06.$GDATE > guess.ctl
      gribmap -i guess.ctl
   else
      ${SCRIPTS}/g2ctl.pl -0 pgbanl.$PDATE > anal.ctl
      gribmap -0 -i anal.ctl
      ${SCRIPTS}/g2ctl.pl pgbf06.$GDATE > guess.ctl
      gribmap -i guess.ctl
   fi 
fi


#------------------------------------------------------------------
#   Clean up $tmpdir  Submit plot jobs.
#
#cd $tmpdir
#cd ../
#rm -rf $tmpdir


jobname=cmon_horz_hist_${SUFFIX}
run_script=${SCRIPTS}/horz_hist.sh
log_file=${LOGDIR}/horz_hist_${SUFFIX}.log
rm -f $log_file

if [[ $MY_MACHINE = "wcoss" ]]; then
   $SUB -P $PROJECT -q $JOB_QUEUE -o ${log_file} -R affinity[core] -M 50 -W 0:30 -J ${jobname} ${run_script}
fi


jobname=cmon_time_vert_${SUFFIX}
run_script=${SCRIPTS}/time_vert.sh
log_file=${LOGDIR}/time_vert_${SUFFIX}.log
rm -f $log_file

if [[ $MY_MACHINE = "wcoss" ]]; then
   $SUB -P $PROJECT -q $JOB_QUEUE -o ${log_file} -R affinity[core] -M 50 -W 0:10 -J ${jobname} ${run_script}
fi


#------------------------------------------------------------------------
#  archive "copr" data
#
#  This could use a switch to make this non-default; users probably won't
#  need this as a default feature.
#
if [[ $SUFFIX = "copr" ]]; then
   rc=`${SCRIPTS}/archive.sh`

   #---------------------------------------------------------------------
   #  tar up all files from a given day
   #  
   time_tank=${TANKDIR}/time_vert
   horz_anl_tank=${TANKDIR}/horz_hist/anl
   horz_ges_tank=${TANKDIR}/horz_hist/ges

   if [[ ${GCYA} = "18" ]]; then
      cd ${time_tank}
      tar -cvf time_vert.${gsdate}.tar *.${gsdate}*

      cd ${horz_anl_tank}
      tar -cvf horz_hist_anl.${gsdate}.tar *.${gsdate}*

      cd ${horz_ges_tank}
      tar -cvf horz_hist_ges.${gsdate}.tar *.${gsdate}*
   fi
fi

exit
