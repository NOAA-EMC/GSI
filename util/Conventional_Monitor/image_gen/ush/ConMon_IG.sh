#!/bin/bash

#--------------------------------------------------------------------
#
#  ConMon_IG.sh 
#
#  This is the top level image generation script for the Conventional 
#  Data Monitor (ConMon) package.  
#
#--------------------------------------------------------------------

#--------------------------------------------------------------------
#  usage
#--------------------------------------------------------------------
function usage {
  echo " "
  echo " "
  echo "Usage:  ConMon_IG.sh suffix [-p|--pdate pdate -r|--run gdas|gfs]"
  echo "            Suffix is the indentifier for this data source."
  echo " "
  echo "            -p | --pdate yyyymmddcc to specify the cycle to be plotted."
  echo "                 If unspecified pdate will be set using the "
  echo "                 C_IMGNDIR/last_plot_time file, and if that doesn't"
  echo "                 exist, then the last available date will be plotted."
  echo " "             
  echo "            -r | --run   the gdas|gfs run to be processed."
  echo "                 Use only if data in TANKdir stores both runs, gdas"
  echo "                 gdas is the default value."
  echo " "
}


#--------------------------------------------------------------------
#  CMon_IG.sh begins here
#--------------------------------------------------------------------

echo "Begin ConMon_IG.sh"


nargs=$#
if [[ $nargs -lt 1 || $nargs -gt 5 ]]; then
   usage
   exit 1
fi

set -ax

#-----------------------------------------------
#  Process command line arguments
#
export RUN=gdas

while [[ $# -ge 1 ]]
do
   key="$1"
   echo $key

   case $key in
      -p|--pdate)
         export PDATE="$2"
         shift # past argument
      ;;
      -r|--run)
         export RUN="$2"
         shift # past argument
      ;;
      *)
         #any unspecified key is CONMON_SUFFIX
         export CONMON_SUFFIX=$key
      ;;
   esac

   shift
done

this_file=`basename $0`
this_dir=`dirname $0`

echo "CONMON_SUFFIX = $CONMON_SUFFIX"
echo "PDATE         = $PDATE"
echo "RUN           = $RUN"

export NUM_CYCLES=${NUM_CYCLES:-121}			# number of cycles in plot
export JOBNAME=${JOBNAME:-ConMon_plt_${CONMON_SUFFIX}}
export grib2=${grib2:-1}				# 1 = grib2 (true), 0 = grib
							# should this move to config?

plot_time=${PDATE}

#--------------------------------------------------------------------
# Run config files to load environment variables,
# set default plot conditions
#--------------------------------------------------------------------
top_parm=${this_dir}/../../parm

conmon_version_file=${conmon_version:-${top_parm}/ConMon.ver}
if [[ -s ${conmon_version_file} ]]; then
   . ${conmon_version_file}
   echo "able to source ${conmon_version_file}"
else
   echo "Unable to source ${conmon_version_file} file"
   exit 2
fi

conmon_config=${conmon_config:-${top_parm}/ConMon_config}
if [[ -s ${conmon_config} ]]; then
   . ${conmon_config}
   echo "able to source ${conmon_config}"
else
   echo "Unable to source ${conmon_config} file"
   exit 3
fi


#--------------------------------------------------------------------
#  Create LOGdir as needed
#--------------------------------------------------------------------
export C_LOGDIR=${C_LOGDIR}/${RUN}/conmon
if [[ ! -d ${C_LOGDIR} ]]; then
   mkdir -p $C_LOGDIR
fi


#--------------------------------------------------------------------
# Get date of cycle to process.  Exit if available data has already
# been plotted ($PDATE -gt $last_cycle).
#
# PDATE can be set one of 3 ways.  This is the order of priority:
#
#   1.  Specified via command line argument
#   2.  Read from ${C_IMGNDIR}/last_plot_time file and advanced
#        one cycle.
#   3.  Using the last available cycle for which there is
#        data in ${C_TANKDIR}.
#
# If option 2 has been used the ${C_IMGNDIR}/last_plot_time file
# will be updated with ${PDATE} if the plot is able to run.
#--------------------------------------------------------------------

echo "C_IG_SCRIPTS = ${C_IG_SCRIPTS}"
echo "C_TANKDIR = ${C_TANKDIR}"

last_cycle=`${C_IG_SCRIPTS}/find_cycle.pl \
		--cyc 1 --dir ${C_TANKDIR} --run ${RUN}`

#if [[ $plot_time != "" ]]; then
#   export PDATE=$plot_time
#else
#   export PDATE=$last_cycle
#fi

if [[ ${PDATE} = "" ]]; then

   if [[ -e ${C_IMGNDIR}/last_plot_time ]]; then
      echo " USING last_plot_time"
      last_plot=`cat ${C_IMGNDIR}/last_plot_time`
      export PDATE=`$NDATE +6 ${last_plot}`
#      export PDATE=`cat ${C_IMGNDIR}/last_plot_time`
   else
      export PDATE=$last_cycle
   fi
fi


#--------------------------------------------------------------------
# Set the START_DATE for the plot
#--------------------------------------------------------------------
ncycles=`expr $NUM_CYCLES - 1`

hrs=`expr $ncycles \\* -6`
echo "hrs = $hrs"

export START_DATE=`$NDATE ${hrs} $PDATE`
echo "START_DATE, last_cycle, PDATE = $START_DATE $last_cycle  $PDATE"



#------------------------------------------------------------------
#   Start image plotting jobs.
#------------------------------------------------------------------
if [[ $PDATE -le ${last_cycle} ]]; then

   echo "ABLE to plot ${PDATE}, last processed date is ${last_cycle}"

   #--------------------------------------------------------------------
   #  Create workdir and cd to it
   #--------------------------------------------------------------------
   export C_PLOT_WORKDIR=${C_PLOT_WORKDIR:-${C_STMP_USER}/${CONMON_SUFFIX}/${RUN}/conmon}
   rm -rf $C_PLOT_WORKDIR
   mkdir -p $C_PLOT_WORKDIR
   cd $C_PLOT_WORKDIR

   #--------------------------------------------------------------------
   #  Run the two setup scripts
   #--------------------------------------------------------------------
#   ${C_IG_SCRIPTS}/mk_horz_hist.sh

#   ${C_IG_SCRIPTS}/mk_time_vert.sh

   #--------------------------------------------------------------------
   #  Update the last_plot_time file if found
   #--------------------------------------------------------------------
   if [[ -e ${C_IMGNDIR}/last_plot_time ]]; then
      echo "update last_plot_time file"  
      echo ${PDATE} > ${C_IMGNDIR}/last_plot_time
   fi

else
   echo "UNABLE to plot ${PDATE}, last processed date is ${last_cycle}"
   exit 4
fi


echo "End ConMon_IG.sh"
exit
