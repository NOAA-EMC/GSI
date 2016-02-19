#!/bin/sh

#####################################################################################
#  Function been tested:            Run MinMon data extraction for GDAS on wcoss 
#
#  Calling sequence:                ./test_minmon.sh Data_Out PDATE
#
#  Initial condition:               This script needs a gsistat or fits2 file for the
#                                   specified cycle.  These must be in the Data_In
#                                   directory and are created by the GSI.
#
#  Usage:                           The MinMon is a verf step job but can be run any 
#                                   time after the gsistat file is generated. This 
#				    driver is designed to work with gsistats in
#				    /com/gfs/prod and is wired to use the gdas gsistat
#                                   files.  It may be altered to use gsistat from
#				    another source by overriding either the Data_In or
#				    gsistat defaults by setting environment variables.
#
#  Data_In:                         Defaults to /com/gfs/prod.  This may be 
#                                   overridden with an environment variable.   
#
#  Data_Out:                        Specified on command line. This is where the 
#                                   radmon data files will be stored.
#
#  PDATA:                           Date to be processed in YYYYMMDDHH form, where HH
#                                   is 00, 06, 12, or 18.
#
#  Result verification:             Output should be contained in directory $Data_Out/
#                                   minmon.[YYYYMMDDHH] where YYYYMMDDHH = pdate.
#                                   Within that directory there should be ieee_d files
#					[ add more here ]
#####################################################################################

#--------------------------------------------------------------------
#  usage
#--------------------------------------------------------------------
function usage {
  echo ""
  echo "Usage:  test_minmon.sh  Data_Out PDATE"
  echo "            Data_Out :  location used for storing extraced minmon data"
  echo "            PDATE    :  cycle to run in YYYYMMDDHH form"
  echo ""
}

#--------------------------------------------------------------------
#  test_minmon.sh begins here
#--------------------------------------------------------------------

module load prod_util/v1.0.2

nargs=$#
if [[ $nargs -ne 2 ]]; then
   usage
   exit 1
fi

set -ax
export Data_In=${Data_In:-/com/gfs/prod}
export Data_Out=$1
if [[ $Data_Out = "." ]]; then
   export Data_Out=${PWD}
fi
export PDATE=$2

minmon_shared_ver=1.0.0

#--------------------------------------------------------------------
#  MINMON_SUFFIX is the identifying name for a given data set from 
#  either a prod or parallel run.  For this driver we'll use 
#  minmon_test as default.
#--------------------------------------------------------------------
export MINMON_SUFFIX=${MINMON_SUFFIX:-"minmon_test"}
export envir="test"

export M_TANKverf=${Data_Out}
export DATAROOT=/stmpp1/${LOGNAME}
export SENDCOM=NO
export GLB_AREA=1

export COMROOT=/com
export NWROOT=/nwprod2

echo MINMON_SUFFIX = $MINMON_SUFFIX

#--------------------------------------------------------------------
#  Override HOMEgdas to point to the /nwprod dir within this package
#  in order to access fix, jobs, scripts in the JJob for testing.
#
#  This should be removed when handed to NCO?
#--------------------------------------------------------------------
this_file=`basename $0`
this_dir=${PWD}
num_flds=`echo ${this_dir} | awk -F'/' '{print NF}'`
num_flds=`expr $num_flds - 1`
gdas_dir=`echo ${this_dir} | cut -d/ -f1-${num_flds}`

num_flds=`expr $num_flds - 1`
nwprod_dir=`echo ${this_dir} | cut -d/ -f1-${num_flds}`

export HOMEgdas=${gdas_dir}
export HOMEminmon=${nwprod_dir}/minmon_shared.v${minmon_shared_ver}
export jlogfile="jlogfile_${MINMON_SUFFIX}"
export KEEPDATA=YES


export PDY=`echo ${PDATE}|cut -c1-8`
export cyc=`echo ${PDATE}|cut -c9-10`


#--------------------------------------------------------------------
#  locate required gsistat file
#--------------------------------------------------------------------
gsistat=${Data_In}/gdas.${PDY}/gdas1.t${cyc}z.gsistat


##########################################
#  expand M_TANKverf for this MINMON_SUFFIX
##########################################
NEWtank=${M_TANKverf}/${MINMON_SUFFIX}
export M_TANKverf=$NEWtank

echo "M_TANKverf = $M_TANKverf"
echo "PDY, cyc = $PDY, $cyc "

#--------------------------------------------------------------------
# If data is available submit the j-job.
#--------------------------------------------------------------------
data_available=0
if [[ -e $gsistat ]]; then
   data_available=1

   export m_jlogfile="${m_jlogfile}${MINMON_SUFFIX}.${PDY}.${cyc}.log"
   echo  "m_jlogfile = $m_jlogfile"
   jobname=minmon_de_${MINMON_SUFFIX}

   rm -f $m_jlogfile

   SUB="bsub"
   job_queue="dev_shared"
   project="GDAS-T2O"

   echo "SUB        = $SUB"
   echo "job_queue  = $job_queue"
   echo "project    = $project"
   echo "jobname    = $jobname" 

#   export PERL5LIB="/usrx/local/pm5/lib64/perl5:/usrx/local/pm5/share/perl5"
   $SUB -q $job_queue -P $project -o ${m_jlogfile} -M 50 -R affinity[core] -W 0:10 -J ${jobname} $HOMEgdas/jobs/JGDAS_VMINMON
fi

#--------------------------------------------------------------------
# clean up and exit
#--------------------------------------------------------------------
exit_value=0
if [[ ${data_available} -ne 1 ]]; then
   exit_value=6
   echo No data available for ${MINMON_SUFFIX}
else
   echo "normal operation, job submitted"
fi

module unload prod_util/v1.0.2
echo "end test_minmon.sh"

exit ${exit_value}
