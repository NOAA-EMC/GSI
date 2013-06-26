#!/bin/sh

#--------------------------------------------------------------------
#
#  Plot_comp.sh
#
#  This script plots the requested comparision plots for the specified
#  suffix (data sources).  Data may be plotted from either global or 
#  regional sources.
#
#  Supported plots include:
#    plot_fs_obsnum_comp.sh
#
#  Note:  this does not generate any data files (*.ieee_d).  Those 
#  must be already created for this script to function correctly.  
#
#--------------------------------------------------------------------

function usage {
  echo "Usage:  Plot_comp.sh suffix"
  echo "            File name for CkPlt_glbl.sh may be full or relative path"
  echo "            Date in YYYYMMDDHH for plot"
  echo "            Suffix1 data source identifier that corresponds to data"
  echo "              in the $TANKDIR/stats directory/suffix1"
  echo "            Suffix2 data source identifier that corresponds to data"
  echo "              in the $TANKDIR/stats directory/suffix1"
  echo "            [Suffix3] optional data source identifier that corresponds to data"
  echo "              in the $TANKDIR/stats directory/suffix1"
}


set -ax
echo start CkPlt_comp.sh

nargs=$#
if [[ $nargs -lt 3 ]]; then
   usage
   exit 1
fi

this_file=`basename $0`
this_dir=`dirname $0`

export PDATE=$1
export SUFFIX1=$2
export SUFFIX2=$3
export SUFFIX3=$4
export SUFFIX=$SUFFIX1

#--------------------------------------------------------------------
# Set environment variables
#--------------------------------------------------------------------

top_parm=${this_dir}/../../parm

if [[ -s ${top_parm}/RadMon_config ]]; then
   . ${top_parm}/RadMon_config
else
   echo "Unable to source ${top_parm}/RadMon_config"
   exit 1
fi

if [[ -s ${top_parm}/RadMon_user_settings ]]; then
   . ${top_parm}/RadMon_user_settings
else
   echo "Unable to source ${top_parm}/RadMon_user_settings"
   exit 2
fi

. ${RADMON_IMAGE_GEN}/parm/plot_rad_conf

#--------------------------------------------------------------------
# Source necessary configuration files
#--------------------------------------------------------------------
data="ges"

echo $SUFFIX2, $SUFFIX3, $last_plot

if [[ $RAD_AREA == "glb" ]]; then
   . ${RADMON_IMAGE_GEN}/parm/glbl_comp_conf
elif [[ $RAD_AREA == "rgn" ]]; then
   . ${RADMON_IMAGE_GEN}/parm/rgnl_comp_conf
fi

mkdir -p $LOGDIR

if [[ -d $PLOT_WORK_DIR ]]; then
   rm -rf $PLOT_WORK_DIR
fi
mkdir $PLOT_WORK_DIR

#--------------------------------------------------------------
# Set up SUFFIX, TANKDIR and IMGNDIR for this plot.
#--------------------------------------------------------------
echo ${TANKDIR}
echo ${IMGNDIR}

export TANKDIR1=${TANKDIR}/${SUFFIX1}
export IMGNDIR1=${IMGNDIR}/${SUFFIX1}
prodate1=`${SCRIPTS}/find_last_cycle.pl ${TANKDIR1}`

export TANKDIR2=${TANKDIR}/${SUFFIX2}
export IMGNDIR2=${IMGNDIR}/${SUFFIX2}
prodate2=`${SCRIPTS}/find_last_cycle.pl ${TANKDIR2}`

#-------------------------------------------------------------------
#  SUFFIX3 may or may not exist (plots can include 2 or 3 different
#  data sources.  The absence of SUFFIX3 is not an error condition.
#-------------------------------------------------------------------
suff3=`echo ${#SUFFIX3}`
if [[ $suff3 -gt 0 ]]; then
   export TANKDIR3=${TANKDIR}/${SUFFIX3}
   export IMGNDIR3=${IMGNDIR}/${SUFFIX3}
   prodate3=`${SCRIPTS}/find_last_cycle.pl ${TANKDIR3}`
fi

#--------------------------------------------------------------
# Get date of cycle to process.  Make sure all sources have
# data for the desired cycle.  Exit if any have not processed
# the requested cycle.
#--------------------------------------------------------------

#export PDATE=`$NDATE +06 $last_plot`  

abort_run=0
if [[ ${prodate1} -lt $PDATE ]]; then
   echo $SUFFIX1 processing date $prodate1 is not up to $PDATE
   abort_run=1
fi
if [[ ${prodate2} -lt $PDATE ]]; then
   echo $SUFFIX2 processing date $prodate2 is not up to $PDATE
   abort_run=1
fi
if [[ $suff3 -gt 0 ]]; then
   if [[ ${prodate3} -lt $PDATE ]]; then
      echo $SUFFIX3 processing date $prodate3 is not up to $PDATE
      abort_run=1
   fi
fi

if [[ $abort_run -eq 1 ]]; then
   exit 3
fi


#-------------------------------------------------------------
#  If USE_STATIC_SATYPE == 0 then assemble the SATYPE list from
#  available data files in $TANKDIR1/angle
#  If USE_STATIC_SATYPE == 1 then load SATYPE from the SATYPE.txt
#  file.
#-------------------------------------------------------------
if [[ $USE_STATIC_SATYPE -eq 0 ]]; then
  
   PDY=`echo $PDATE|cut -c1-8` 
   if [[ -d ${TANKDIR1}/radmon.${PDY} ]]; then
      test_list=`ls ${TANKDIR1}/radmon.${PDY}/angle.*${PDATE}.ieee_d*`
   else
      test_list=`ls $TANKDIR1/angle/*.${PDATE}.ieee_d*`
   fi

   for test in ${test_list}; do
      this_file=`basename $test`
      tmp=`echo "$this_file" | cut -d. -f2`
      echo $tmp
      SATYPE_LIST="$SATYPE_LIST $tmp"
   done

   SATYPE=$SATYPE_LIST

else

   STATIC_SATYPE_FILE=${TANKDIR}/radmon.${PDY}/gdas_radmon_satype.txt

   #-------------------------------------------------------------
   #  Load the SATYPE list from the STATIC_SATYPE_FILE or exit
   #  if unable to locate it.
   #-------------------------------------------------------------
   if [[ -s $STATIC_SATYPE_FILE ]]; then
      SATYPE=""
      SATYPE=`cat ${STATIC_SATYPE_FILE}`
   else
      echo "Unable to locate $STATIC_SATYPE_FILE, must exit."
      exit
   fi
fi

echo $SATYPE



#------------------------------------------------------------------
# Export variables and submit plot script
#------------------------------------------------------------------
export listvar=PARM,RAD_AREA,PDATE,NDATE,TANKDIR1,TANKDIR2,TANKDIR3,IMGNDIR1,IMGNDIR2,IMGNDIR3,LOADLQ,EXEDIR,LOGDIR,SCRIPTS,GSCRIPTS,STNMAP,GRADS,USER,PTMP_USER,STMP_USER,USER_CLASS,SUB,SUFFIX,SUFFIX1,SUFFIX2,SUFFIX3,SATYPE,NCP,PLOT_WORK_DIR,ACCOUNT,COMPRESS,UNCOMPRESS,Z,listvar


#------------------------------------------------------------------
# submit plot script
#------------------------------------------------------------------
plotfile=${SCRIPTS}/plot_fs_obsnum_comp.sh
cmdfile=${PLOT_WORK_DIR}/cmdfile_comp_plot_${SUFFIX1}
jobname=plot_comp_${SUFFIX1}
logfile=${LOGDIR}/plot_comp.log
rm -f $logfile
rm -f $cmdfile

for type in ${SATYPE}; do
   echo ${plotfile} ${type} ${data} >> $cmdfile
done

ntasks=`cat $cmdfile|wc -l`
((nprocs=(ntasks+1)/2))

if [[ $MY_MACHINE = "ccs" ]]; then
   $SUB -a $ACCOUNT -e $listvar -j $jobname -u $USER -t 0:10:00 -o $logfile -p $ntasks/1/N -q dev -g $USER_CLASS  /usr/bin/poe -cmdfile $cmdfile -pgmmodel mpmd -ilevel 2 -labelio yes -stdoutmode ordered
elif [[ $MY_MACHINE = "wcoss" ]]; then
   $SUB -q dev -n 1,$ntasks -o ${logfile} -W 0:20 -J ${jobname} <$cmdfile
elif [[ $MY_MACHINE = "zeus" ]]; then
   $SUB -A $ACCOUNT -l procs=1,walltime=0:20:00 -N $jobname -v $listvar -j oe -o $logfile $cmdfile
fi



echo end CkPlt_comp.sh

exit
