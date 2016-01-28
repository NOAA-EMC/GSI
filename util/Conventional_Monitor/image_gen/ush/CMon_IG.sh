#!/bin/sh

#--------------------------------------------------------------------
#
#  CMon_IG.sh 
#
#  This is the top level image generation script for the Conventional 
#  Data Monitor (Cmon) package.  
#
#--------------------------------------------------------------------

#--------------------------------------------------------------------
#  usage
#--------------------------------------------------------------------
function usage {
  echo " "
  echo "Usage:  CMon_IG.sh suffix [plot_date]"
  echo "            Suffix is data source identifier that matches data in "
  echo "              the $C_TANKDIR/stats directory."
  echo "            Plot_date, format YYYYMMDDHH is optional.  If included the plot"
  echo "              will be for the specified cycle, provided data files are available."
  echo "              If not included, the plot cycle will be for the latest cycle found"
  echo "              for this suffix."
}


#--------------------------------------------------------------------
#  CMon_IG.sh begins here
#--------------------------------------------------------------------

echo "Begin CMon_IG.sh"

nargs=$#
if [[ $nargs -lt 1 || $nargs -gt 2 ]]; then
   usage
   exit 1
fi


set -ax

this_file=`basename $0`
this_dir=`dirname $0`

export CMON_SUFFIX=$1
echo "CMON_SUFFIX = $CMON_SUFFIX"

export NUM_CYCLES=${NUM_CYCLES:-121}	# number of cycles in plot
export JOBNAME=${JOBNAME:-CMon_plt_${CMON_SUFFIX}}
export grib2=${grib2:-0}		# 1 = grib2 (true), 0 = grib

export GRADS=/apps/grads/2.0.1a/bin/grads
#--------------------------------------------------------------------
#  Set plot_time if it's included as an argument
#--------------------------------------------------------------------
plot_time=
if [[ $nargs -eq 2 ]]; then
   export plot_time=$2;
   echo "use plot_time = $plot_time"
fi


#--------------------------------------------------------------------
# Run config files to load environment variables,
# set default plot conditions
#--------------------------------------------------------------------
top_parm=${this_dir}/../../parm

cmon_version_file=${cmon_version:-${top_parm}/CMon.ver}
if [[ -s ${cmon_version_file} ]]; then
   . ${cmon_version_file}
   echo "able to source ${cmon_version_file}"
else
   echo "Unable to source ${cmon_version_file} file"
   exit 2
fi

cmon_config=${cmon_config:-${top_parm}/CMon_config}
if [[ -s ${cmon_config} ]]; then
   . ${cmon_config}
   echo "able to source ${cmon_config}"
else
   echo "Unable to source ${cmon_config} file"
   exit 3
fi

#--------------------------------------------------------------------
#  Check for my monitoring use.  Abort if running on prod machine.
#--------------------------------------------------------------------

if [[ RUN_ONLY_ON_DEV -eq 1 ]]; then
   is_prod=`${C_IG_SCRIPTS}/onprod.sh`
   if [[ $is_prod = 1 ]]; then
      exit 10
   fi
fi

jobname=CMon_ig_${CMON_SUFFIX}


#--------------------------------------------------------------------
#  Create LOGdir as needed
#--------------------------------------------------------------------
if [[ ! -d ${C_LOGDIR} ]]; then
   mkdir -p $C_LOGSDIR
fi


#--------------------------------------------------------------------
# Get date of cycle to process.  Exit if available data has already
# been plotted ($PDATE -gt $PRODATE).
#
# If plot_time has been specified via command line argument, then
# set PDATE to it.  Otherwise, determine the last cycle processed
# (into *.ieee_d files) and use that as the PDATE.
#--------------------------------------------------------------------
export PRODATE=`${C_IG_SCRIPTS}/find_cycle.pl 1 ${C_TANKDIR}`

if [[ $plot_time != "" ]]; then
   export PDATE=$plot_time
else
   export PDATE=$PRODATE
fi

echo "PRODATE, PDATE = $PRODATE, $PDATE"


#--------------------------------------------------------------------
# Check for running plot jobs and abort if found
#--------------------------------------------------------------------

if [[ $MY_MACHINE = "wcoss" ]]; then
   running=`bjobs -l | grep ${jobname} | wc -l`
else
   running=`showq -n -u ${LOGNAME} | grep ${jobname} | wc -l`
fi

echo "running = $running"
if [[ $running -ne 0 ]]; then
   echo "Plot jobs still running for $CMON_SUFFIX, must exit"
   exit 9 
fi


#--------------------------------------------------------------------
#  Create workdir and cd to it
#--------------------------------------------------------------------

export C_PLOT_WORKDIR=${C_PLOT_WORKDIR:-${C_STMP_USER}/plot_cmon_${CMON_SUFFIX}}
rm -rf $C_PLOT_WORKDIR
mkdir -p $C_PLOT_WORKDIR
cd $C_PLOT_WORKDIR


#--------------------------------------------------------------------
# Set the START_DATE for the plot
#--------------------------------------------------------------------

hrs=`expr $NUM_CYCLES \\* -6`
echo "hrs = $hrs"

export START_DATE=`$NDATE ${hrs} $PDATE`
echo "start_date, prodate, pdate = $START_DATE $PRODATE  $PDATE"



#------------------------------------------------------------------
#   Start image plotting jobs.
#------------------------------------------------------------------

${C_IG_SCRIPTS}/mk_horz_hist.sh

${C_IG_SCRIPTS}/mk_time_vert.sh


#------------------------------------------------------------------
#  Run the make_archive.sh script if $DO_ARCHIVE is switched on.
#------------------------------------------------------------------
#if [[ $DO_ARCHIVE = 1 ]]; then
#   ${IG_SCRIPTS}/make_archive.sh
#fi


#--------------------------------------------------------------------
# Clean up and exit
#cd $tmpdir
#cd ../
#rm -rf $tmpdir

echo "End CMon_IG.sh"
exit
