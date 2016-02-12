#!/bin/sh

#####################################################################################
#  Function been tested:            Run RadMon data extraction for GDAS on wcoss 
#
#  Calling sequence:                ./test_radmon.sh Data_In Data_Out PDATE
#
#  Initial condition:               This script needs a bias correction and a radstat 
#				    file from the same cycle.  These must be in the 
# 				    indir directory.
#
#  Usage:                           {bsub < xxx.sh under what directory structure}
#
#  Data_In:                         Specified "indir" on command line.  This should be
#				    something like ${COMROOT}/gfs/prod where 
#				    $COMROOT is /com2 or /com.
#
#  Data_Out:                        Specified "outdir" on command line. 
#
#  Result verification:             Output should be contained in directory $outdir/
#				    radmon.[YYYYMMDDHH] where YYYYMMDDHH = pdate.
#				    Within that directory there should be ieee_d files
#				    for each satellite instrument by angle, bcoef, 
#				    bcor, and time.  Zero length files are a problem.
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
#  Eventually remove RUN_ENVIR argument but allow for it to possibly be
#  present as $2 to ensure backward compatibility.
#  
#  if $COMOUT is defined then assume we're in a parallel.
#--------------------------------------------------------------------
export SUFFIX="radmon_test"
export envir="test"

export TANKverf=${PWD}/${SUFFIX}
export DATAROOT=/stmpp1/${LOGNAME}
export COMROOT=/com
export SENDCOM=NO

export NWROOT=/nwprod2
#export RUN_ENVIR=""
#
#if [[ $RUN_ENVIR = "" ]]; then
#  export RUN_ENVIR="para"
#  if [[ $COMOUT = "" ]]; then
#     export RUN_ENVIR="dev"
#  fi
#fi

echo SUFFIX = $SUFFIX
#echo RUN_ENVIR = $RUN_ENVIR

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
export HOMEradmon=${nwprod_dir}/radmon_shared.v${radmon_shared_ver}
export jlogfile="jlogfile_${SUFFIX}"
export KEEPDATA=YES


#--------------------------------------------------------------------
# Set environment variables
#--------------------------------------------------------------------
#export RAD_AREA=glb
#export MAKE_CTL=${MAKE_CTL:-1}
#export MAKE_DATA=${MAKE_DATA:-1}

#if [[ $RUN_ENVIR = para || $RUN_ENVIR = prod ]]; then
#   this_dir=${VRFYRAD_DIR}
#fi


#mkdir -p $TANKverf
#mkdir -p $LOGdir


#------------------------------------------------------------------
#  define data file sources depending on $RUN_ENVIR
#
#  need to idenfity correct output location(s) for binary files
#------------------------------------------------------------------

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

#   export MP_SHARED_MEMORY=yes
#   export MEMORY_AFFINITY=MCM
#   export envir=prod
#   
#   export cyc=$cyc
#   export job=gdas_vrfyrad_${PDY}${cyc}
#   export SENDSMS=${SENDSMS:-NO}
#   export DATA_IN=${WORKverf_rad}
#   export DATA=${DATA:-${STMP_USER}/radmon}
#   export jlogfile=${WORKverf_rad}/jlogfile_${SUFFIX}
#
#   export VERBOSE=${VERBOSE:-YES}
#  

   #----------------------------------------------------------------------------
   #  Advance the satype file from previous day.
   #  If it isn't found then create one using the contents of the radstat file.
   #----------------------------------------------------------------------------
#   export satype_file=${TANKverf}/radmon.${PDY}/${SUFFIX}_radmon_satype.txt
#
#   if [[ $cyc = "00" ]]; then
#      echo "Making new day directory for 00 cycle"
#      mkdir -p ${TANKverf}/radmon.${PDY}
#      prev_day=`${NDATE} -06 $PDATE | cut -c1-8`
#      if [[ -s ${TANKverf}/radmon.${prev_day}/${SUFFIX}_radmon_satype.txt ]]; then
#         cp ${TANKverf}/radmon.${prev_day}/${SUFFIX}_radmon_satype.txt ${TANKverf}/radmon.${PDY}/.
#      fi
fi 

if [[ -e ${biascr} ]]; then
   echo "biascr exists"
fi 

if [[ -e ${radstat} && -e ${biascr} ]]; then
   data_available=1
fi

#    echo "TESTING for $satype_file"
#    if [[ -s ${satype_file} ]]; then
#      echo "${satype_file} is good to go"
#    else
#      echo "CREATING satype file"
#      radstat_satype=`tar -tvf $radstat | grep _ges | awk -F_ '{ print $2 "_" $3 }'`
#      echo $radstat_satype > ${satype_file}
#      echo "CREATED ${satype_file}"
#    fi

   
   #------------------------------------------------------------------
   #   Override the default base_file declaration if there is an  
   #   available base file for this source.
   #------------------------------------------------------------------
#   if [[ -s ${TANKverf}/info/radmon_base.tar.${Z} || -s ${TANKverf}/info/radmon_base.tar ]]; then
#      export base_file=${TANKverf}/info/radmon_base.tar 
#   fi

   #------------------------------------------------------------------
   #   Submit data processing jobs.
   #------------------------------------------------------------------
#   if [[ $MY_MACHINE = "wcoss" ]]; then
      jobname="test_radmon"
      echo "queue job $jobname"
      logfile="./radmon_test.${PDY}.${cyc}.log"
      project="GDAS-T2O"
      job_queue="dev_shared"
      SUB=bsub

      echo "job is ${HOMEgdas}/jobs/JGDAS_VERFRAD"

      $SUB -q ${job_queue} -P $project -o $logfile -M 100 -R affinity[core] -W 0:20 -J ${jobname} ${HOMEgdas}/jobs/JGDAS_VERFRAD

#   elif [[ $MY_MACHINE = "zeus" || $MY_MACHINE = "theia" ]]; then
#      $SUB -A $ACCOUNT -l procs=1,walltime=0:10:00 -N ${jobname} -V -o $LOGdir/data_extract.${PDY}.${cyc}.log -e $LOGdir/error_file.${PDY}.${cyc}.log $HOMEgdasradmon/jobs/JGDAS_VERFRAD
#   fi
  
#--------------------------------------------------------------------
# Clean up and exit
#--------------------------------------------------------------------
#cd $tmpdir
#cd ../
#rm -rf $tmpdir

exit_value=0
if [[ ${data_available} -ne 1 ]]; then
   exit_value=6
   echo No data available for ${SUFFIX}
else
   echo "driver reports normal operation, job submitted"
fi

echo end test_radmon.sh


module unload prod_util/v1.0.2

exit ${exit_value}

