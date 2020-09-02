#!/bin/sh

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
  echo "Usage:  ConMon_IG.sh suffix [-p|--pdate pdate -r|--run gdas|gfs]"
  echo "            Suffix is the indentifier for this data source."
  echo "            -p | --pdate yyyymmddcc to specify the cycle to be processed"
  echo "              if unspecified the last available date will be processed"
  echo "            -r | --run   the gdas|gfs run to be processed"
  echo "              use only if data in TANKdir stores both runs, otherwise"
  echo "              gdas is assumed."
  echo " "
}


#--------------------------------------------------------------------
#  CMon_IG.sh begins here
#--------------------------------------------------------------------

echo "Begin ConMon_IG.sh"

set -ax

nargs=$#
if [[ $nargs -lt 1 || $nargs -gt 5 ]]; then
   usage
   exit 1
fi


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
# been plotted ($PDATE -gt $PRODATE).
#
# If plot_time has been specified via command line argument, then
# set PDATE to it.  Otherwise, determine the last cycle processed
# (into *.ieee_d files) and use that as the PDATE.
#--------------------------------------------------------------------

echo "C_IG_SCRIPTS = ${C_IG_SCRIPTS}"
echo "C_TANKDIR = ${C_TANKDIR}"

export PRODATE=`${C_IG_SCRIPTS}/find_cycle.pl \
		--cyc 1 --dir ${C_TANKDIR} --run ${RUN}`

if [[ $plot_time != "" ]]; then
   export PDATE=$plot_time
else
   export PDATE=$PRODATE
fi

echo "PRODATE, PDATE = $PRODATE, $PDATE"


#--------------------------------------------------------------------
#  Create workdir and cd to it
#--------------------------------------------------------------------
export C_PLOT_WORKDIR=${C_PLOT_WORKDIR:-${C_STMP_USER}/${CONMON_SUFFIX}/${RUN}/conmon}
rm -rf $C_PLOT_WORKDIR
mkdir -p $C_PLOT_WORKDIR
cd $C_PLOT_WORKDIR


#--------------------------------------------------------------------
# Set the START_DATE for the plot
#--------------------------------------------------------------------
ncycles=`expr $NUM_CYCLES - 1`

hrs=`expr $ncycles \\* -6`
echo "hrs = $hrs"

export START_DATE=`$NDATE ${hrs} $PDATE`
echo "start_date, prodate, pdate = $START_DATE $PRODATE  $PDATE"


#------------------------------------------------------------------
#   Start image plotting jobs.
#------------------------------------------------------------------

#${C_IG_SCRIPTS}/mk_horz_hist.sh

${C_IG_SCRIPTS}/mk_time_vert.sh


#--------------------------------------------------------------------
# Clean up and exit
#cd $C_PLOT_WORKDIR
#cd ../
#rm -rf $C_PLOT_WORKDIR

echo "End ConMon_IG.sh"
exit
