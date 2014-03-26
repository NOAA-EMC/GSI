#!/bin/sh

#--------------------------------------------------------------------
#
#  PlotComp.sh
#
#  This script plots the requested comparision plots for the specified
#  suffix (data sources).  Data may be plotted from either global or 
#  regional sources.
#
#  Supported plots include:
#    plot_fs_obsnum_comp.sh (?)
#    plot_comp.sh
#
#  Note:  this does not generate any data files (*.ieee_d).  Those 
#  must be already created for this script to function correctly.  
#
#--------------------------------------------------------------------

function usage {
  echo "Usage:  PlotComp.sh start_date end_date suffix1 suffix2"
  echo "            File name for PlotComp.sh may be full or relative path"
  echo "            Start Date for plot range (format: yyyymmddhh)"
  echo "            End Date for plot range (format yyyymmddhh)"
  echo "            Suffix1 data source identifier that corresponds to data"
  echo "              in the $TANKDIR/stats directory/suffix1"
  echo "            Suffix2 data source identifier that corresponds to data"
  echo "              in the $TANKDIR/stats directory/suffix2"
}


set -ax
echo start PlotComp.sh

nargs=$#
if [[ $nargs -ne 4 ]]; then
   usage
   exit 1
fi

this_file=`basename $0`
this_dir=`dirname $0`

export SDATE=$1
export EDATE=$2
export SUFFIX1=$3
export SUFFIX2=$4
export SUFFIX=$SUFFIX1

#--------------------------------------------------------------------
# Set environment variables
#--------------------------------------------------------------------

top_parm=${this_dir}/../../parm
export RADMON_CONFIG=${RADMON_CONFIG:-${top_parm}/RadMon_config}

if [[ -s ${RADMON_CONFIG} ]]; then
   . ${RADMON_CONFIG}
else
   echo "Unable to source ${RADMON_CONFIG}"
   exit 2
fi

if [[ -s ${RADMON_USER_SETTINGS} ]]; then
   . ${RADMON_USER_SETTINGS}
else
   echo "Unable to source ${RADMON_USER_SETTINGS}"
   exit 3
fi

. ${IG_PARM}/plot_rad_conf

#--------------------------------------------------------------------
#  Check setting of RUN_ONLY_ON_DEV and possible abort if on prod and
#  not permitted to run there.
#--------------------------------------------------------------------

if [[ RUN_ONLY_ON_DEV -eq 1 ]]; then
   is_prod=`${IG_SCRIPTS}/onprod.sh`
   if [[ $is_prod = 1 ]]; then
      exit 10
   fi
fi

#--------------------------------------------------------------------
# Source necessary configuration files
#--------------------------------------------------------------------
data="ges"

echo $SDATE $EDATE $SUFFIX1, $SUFFIX2

if [[ -d ${MY_TANKDIR}/stats/${SUFFIX1} ]]; then
   export RAD_AREA="glb"
elif [[ -d ${MY_TANKDIR}/stats/regional/${SUFFIX1} ]]; then
   export RAD_AREA="rgn"
else
   echo unable to locate $SUFFIX1 in $MY_TANKDIR tree
   exit 4
fi

echo rad_area = $RAD_AREA

mkdir -p $LOGdir

if [[ -d $PLOT_WORK_DIR ]]; then
   rm -rf $PLOT_WORK_DIR
fi
mkdir $PLOT_WORK_DIR

#--------------------------------------------------------------
# Set up SUFFIX, TANKDIR and IMGNDIR for this plot.
#--------------------------------------------------------------
export TANKDIR=${MY_TANKDIR}/stats
echo ${TANKDIR}
export IMGNDIR=${MY_TANKDIR}/imgn
echo ${IMGNDIR}

export TANKDIR1=${TANKDIR}/${SUFFIX1}
export IMGNDIR1=${IMGNDIR}/${SUFFIX1}
prodate1=`${IG_SCRIPTS}/find_cycle.pl 1 ${TANKDIR1}`
echo prodate1 = $prodate1

export TANKDIR2=${TANKDIR}/${SUFFIX2}
export IMGNDIR2=${IMGNDIR}/${SUFFIX2}
prodate2=`${IG_SCRIPTS}/find_cycle.pl 1 ${TANKDIR2}`
echo prodate2 = $prodate2

#-------------------------------------------------------------
#  Build the SATYPE list using SUFFIX1 
#-------------------------------------------------------------
  
PDY=`echo $EDATE|cut -c1-8` 
if [[ -d ${TANKDIR1}/radmon.${PDY} ]]; then
   test_list=`ls ${TANKDIR1}/radmon.${PDY}/time.*${EDATE}.ieee_d*`
else
   test_list=`ls $TANKDIR1/time/*.${EDATE}.ieee_d*`
fi

for test in ${test_list}; do
   this_file=`basename $test`
   tmp=`echo "$this_file" | cut -d. -f2`
   echo $tmp
   test_anl=`echo $this_file | grep "_anl"`
   if [[ $test_anl = "" ]]; then
      SATYPE_LIST="$SATYPE_LIST $tmp"
   fi
done

export SATYPE=$SATYPE_LIST
echo $SATYPE

. ${IG_SCRIPTS}/mk_comp_plots.sh

echo end PlotComp.sh

exit
