#!/bin/sh

#--------------------------------------------------------------------
#
#  CkPlt_comp.sh
#
#  This script plots the requested comparision plots for the specified
#  suffix (data source).  Data may be plotted from either global or 
#  regional sources.
#
#  The entry for the suffix in the ../../parm/data_map.xml file should
#  include entries for the desired comparision source(s).
#
#  Supported plots include:
#    plot_fs_obsnum_comp.sh
#
#  Note:  this does not generate any data files (*.ieee_d).  Those 
#  must be already created for this script to function correctly.
#
#--------------------------------------------------------------------

function usage {
  echo "Usage:  CkPlt_comp.sh suffix"
  echo "            File name for CkPlt_glbl.sh may be full or relative path"
  echo "            Suffix is data source identifier that matches data in "
  echo "              the $TANKDIR/stats directory."
}


set -ax
echo start CkPlt_comp.sh

nargs=$#
if [[ $nargs -ne 1 ]]; then
   usage
   exit 1
fi

this_file=`basename $0`
this_dir=`dirname $0`

SUFFIX1=$1
SUFFIX=$SUFFIX1

#--------------------------------------------------------------------
# Set environment variables
#--------------------------------------------------------------------

top_parm=${this_dir}/../../parm

if [[ -s ${top_parm}/RadMon_config ]]; then
   . ${top_parm}/RadMon_config
else
   echo "Unable to source ${top_parm}/RadMon_config"
   exit
fi


. ${RADMON_IMAGE_GEN}/parm/plot_rad_conf

#--------------------------------------------------------------------
# Load necessary configuration parmeters from data_map file. 
#--------------------------------------------------------------------
SUFFIX2=`${SCRIPTS}/query_data_map.pl ${DATA_MAP} ${SUFFIX1} comp_suffix2`
SUFFIX3=`${SCRIPTS}/query_data_map.pl ${DATA_MAP} ${SUFFIX1} comp_suffix3`
last_plot=`${SCRIPTS}/query_data_map.pl ${DATA_MAP} ${SUFFIX1} comp_plotdate`
area=`${SCRIPTS}/query_data_map.pl ${DATA_MAP} ${SUFFIX1} area`
ACCOUNT=`${SCRIPTS}/query_data_map.pl ${DATA_MAP} ${SUFFIX1} account`
USER_CLASS=`${SCRIPTS}/query_data_map.pl ${DATA_MAP} ${SUFFIX1} user_class`

data="ges"

echo $SUFFIX2, $SUFFIX3, $last_plot

if [[ $area == "glb" ]]; then
   . ${RADMON_IMAGE_GEN}/parm/glbl_comp_conf
elif [[ $area == "rgn" ]]; then
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
prodate1=`${SCRIPTS}/query_data_map.pl ${DATA_MAP} ${SUFFIX1} prodate`

suff2=`echo ${#SUFFIX2}`
if [[ $suff2 -gt 0 ]]; then
   export TANKDIR2=${TANKDIR}/${SUFFIX2}
   export IMGNDIR2=${IMGNDIR}/${SUFFIX2}
   prodate2=`${SCRIPTS}/query_data_map.pl ${DATA_MAP} ${SUFFIX2} prodate`
else
   echo "unable to locate prodate field for source $SUFFIX2 in ${DATA_MAP} file"
   exit
fi

#-------------------------------------------------------------------
#  SUFFIX3 may or may not exist (plots can include 2 or 3 different
#  data sources.  The absence of SUFFIX3 is not an error condition.
#-------------------------------------------------------------------
suff3=`echo ${#SUFFIX3}`
if [[ $suff3 -gt 0 ]]; then
   export TANKDIR3=${TANKDIR}/${SUFFIX3}
   export IMGNDIR3=${IMGNDIR}/${SUFFIX3}
   prodate3=`${SCRIPTS}/query_data_map.pl ${DATA_MAP} ${SUFFIX3} prodate`
fi

#--------------------------------------------------------------
# Get date of cycle to process.  Make sure all sources have
# data for the desired cycle.  Exit if any have not processed
# the requested cycle.
#--------------------------------------------------------------

export PDATE=`$NDATE +06 $last_plot`  

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
   exit
fi

#-------------------------------------------------------------
#  Get the SATYPE for SUFFIX
#
export USE_STATIC_SATYPE=`${SCRIPTS}/query_data_map.pl ${DATA_MAP} ${SUFFIX1} static_satype`

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
   TANKDIR_INFO=${TANKDIR1}/info
   STATIC_SATYPE_FILE=${TANKDIR_INFO}/SATYPE.txt

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
export listvar=PARM,RAD_AREA,PDATE,NDATE,TANKDIR1,TANKDIR2,TANKDIR3,IMGNDIR1,IMGNDIR2,IMGNDIR3,LOADLQ,LLQ,WEB_SVR,WEB_USER,WEBDIR,EXEDIR,LOGDIR,SCRIPTS,GSCRIPTS,STNMAP,GRADS,USER,PTMP_USER,STMP_USER,USER_CLASS,SUB,SUFFIX,SUFFIX1,SUFFIX2,SUFFIX3,SATYPE,NCP,PLOT_WORK_DIR,ACCOUNT,COMPRESS,UNCOMPRESS,Z,listvar


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
   $SUB -q transfer -n $ntasks -o ${logfile} -W 0:20 -J ${jobname} <$cmdfile
elif [[ $MY_MACHINE = "zeus" ]]; then
   $SUB -A $ACCOUNT -l procs=1,walltime=0:20:00 -N $jobname -v $listvar -j oe -o $logfile $cmdfile
fi



#------------------------------------------------------------------
#  Update comp plot time
#------------------------------------------------------------------
rc=`${SCRIPTS}/update_data_map.pl ${DATA_MAP} ${SUFFIX} comp_plotdate ${PDATE}`
if [[ $rc -ne 0 ]]; then
   echo "error updating ${DATA_MAP}, return code = $rc"
fi

echo end CkPlt_comp.sh

exit
