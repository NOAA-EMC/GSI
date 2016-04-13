#!/bin/sh

#####################################################################################
#  Function been tested:            Run RadMon data extraction for GDAS on wcoss 
#
#  Calling sequence:                ./test_radmon.sh Data_In Data_Out PDATE
#
#  Initial condition:               This script needs a bias correction and a radstat 
#				    file from the same cycle.  These must be in the 
# 				    Data_In directory.
#
#  Usage:                           The RadMon is a verf step job but can be run any 
#                                   time after the radstat file is generated. 
#
#  Data_In:                         Specified on command line.  This should be
#				    something like ${COMROOT}/gfs/prod where 
#				    $COMROOT is /com2 or /com.
#
#  Data_Out:                        Specified on command line. This is where the 
#				    radmon data files will be stored.
#
#  PDATA:			    Date to be processed in YYYYMMDDHH form, where HH
#				    is 00, 06, 12, or 18.
#
#  Result verification:             Output should be contained in directory $Data_Out/
#				    radmon.[YYYYMMDDHH] where YYYYMMDDHH = pdate.
#				    Within that directory there should be ieee_d files
#				    for each satellite instrument by angle, bcoef, 
#				    bcor, and time.  Zero length data files are a 
#                                   problem.
#####################################################################################

#--------------------------------------------------------------------
#  usage
#--------------------------------------------------------------------
function usage {
  echo "Usage:  test_radmon.sh Data_In Data_Out PDATE "
  echo "            Data_In :  directory containing abias and radstat file(s)"
  echo "            Data_Out:  directory for output data"
  echo "            PDATE   :  full YYYYMMDDHH cycle to run."
}

set -ax
echo start test_radmon.sh

module load prod_util/v1.0.2
module load prod_envir
module load PrgEnv-intel

#--------------------------------------------------------------------
#  test_radmon.sh begins here
#--------------------------------------------------------------------
nargs=$#
if [[ $nargs -ne 3 ]]; then
   usage
   exit 1
fi

export Data_In=$1
export Data_Out=$2
export PDATE=$3

radmon_shared_ver=2.0.2

#--------------------------------------------------------------------
#  SUFFIX is the identifying name for a given data set from either a
#  prod or parallel run.  For this driver we'll use radmon_test as 
#  default.
#--------------------------------------------------------------------
export SUFFIX=${SUFFIX:-"radmon_test"}
export envir="test"

export TANKverf=${PWD}/${SUFFIX}
export DATAROOT=/gpfs/hps/stmp/${LOGNAME}
export COMROOT=${COMROOTp1}
export SENDCOM=NO

export NWROOT=/nwprod2
export utilscript=${UTILROOT}/ush

#--------------------------------------------------------------------
#  Override HOMEgdas to point to the /nwprod dir within this package
#  in order to access fix, jobs, scripts in the JJob for testing.
#
#  This should be removed when handed to NCO?
#--------------------------------------------------------------------
this_dir=${PWD}
num_flds=`echo ${this_dir} | awk -F'/' '{print NF}'`
num_flds=`expr $num_flds - 1`
gdas_dir=`echo ${this_dir} | cut -d/ -f1-${num_flds}`

num_flds=`expr $num_flds - 1`
nwprod_dir=`echo ${this_dir} | cut -d/ -f1-${num_flds}`

export HOMEgdas=${HOMEgdas:-${gdas_dir}}
export HOMEradmon=${HOMEradmon:-${nwprod_dir}/radmon_shared.v${radmon_shared_ver}}

export jlogfile="jlogfile_${SUFFIX}"
export KEEPDATA=YES

export PDY=`echo ${PDATE}|cut -c1-8`
export cyc=`echo ${PDATE}|cut -c9-10`

export DATDIR=${Data_In}

#---------------------------------------------------------------
# Locate required files.             
#---------------------------------------------------------------
if [[ -d ${DATDIR}/gdas.$PDY ]]; then
   export DATDIR=${DATDIR}/gdas.${PDY}

   export biascr=$DATDIR/gdas1.t${cyc}z.abias  
   export radstat=$DATDIR/gdas1.t${cyc}z.radstat
else
   export biascr=$DATDIR/biascr.gdas.${PDATE}  
   export radstat=$DATDIR/radstat.gdas.${PDATE}
fi

#--------------------------------------------------------------------
# If data is available, export variables, and submit driver for
# radiance monitoring jobs.
#--------------------------------------------------------------------
data_available=0

if [[ -e ${radstat} ]]; then
   echo " radstat exists"                                         
fi 

if [[ -e ${biascr} ]]; then
   echo "biascr exists"
fi 

#------------------------------------------------------------------
#   Submit job 
#------------------------------------------------------------------
if [[ -e ${radstat} && -e ${biascr} ]]; then
   data_available=1

   jobname="test_radmon"
   echo "queue job $jobname"
   logfile="./radmon_test.${PDY}.${cyc}.log"
   project="GDAS-T2O"
#   job_queue="dev_shared"
   job_queue="dev"
   SUB=bsub

   echo "job is ${HOMEgdas}/jobs/JGDAS_VERFRAD"

#   $SUB -q ${job_queue} -P $project -o $logfile -M 100 -R affinity[core] -W 0:20 -J ${jobname} ${HOMEgdas}/jobs/JGDAS_VERFRAD
   $SUB -q ${job_queue} -P $project -o $logfile -M 100 -W 0:20 -J ${jobname} ${HOMEgdas}/jobs/JGDAS_VERFRAD

fi

#--------------------------------------------------------------------
# clean up and exit
#--------------------------------------------------------------------
exit_value=0
if [[ ${data_available} -ne 1 ]]; then
   exit_value=6
   echo No data available for ${SUFFIX}
else
   echo "normal operation, job submitted"
fi

echo end test_radmon.sh


module unload prod_util/v1.0.2
module unload prod_envir
module unload PrgEnv-intel

exit ${exit_value}

