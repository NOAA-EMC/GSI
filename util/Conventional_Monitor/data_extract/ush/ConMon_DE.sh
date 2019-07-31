#!/bin/sh

#--------------------------------------------------------------------
#
#  ConMon_DE.sh 
#
#  This is the top level data extractionscript for the Conventional 
#  Data Monitor (ConMon) package.  
#
#  C_DATDIR and C_GDATDIR (source directories for the cnvstat files) 
#  point to the operational data (GDAS).  They can be overriden 
#  either in your interactive shell or in a script in order to point 
#  to another source.
#--------------------------------------------------------------------

#--------------------------------------------------------------------
#  usage
#--------------------------------------------------------------------
function usage {
  echo "Usage:  ConMon_DE.sh suffix [pdate]"
  echo "            Suffix is the indentifier for this data source."
  echo "            Pdate is the full YYYYMMDDHH cycle to run.  This 
		    param is optional"
}

#--------------------------------------------------------------------
#  CMon_DE.sh begins here
#--------------------------------------------------------------------

nargs=$#
if [[ $nargs -lt 1 || $nargs -gt 2 ]]; then
   usage
   exit 1
fi

set -ax
echo "Begin ConMon_DE.sh"

this_file=`basename $0`
this_dir=`dirname $0`


export CMON_SUFFIX=$1

#--------------------------------------------------------------------
#  RUN_ENVIR:  can be either "dev" or "para".
#--------------------------------------------------------------------
#export RUN_ENVIR=$2		
export RUN_ENVIR=${RUN_ENVIR:-"dev"}

#--------------------------------------------------------------------
#  load modules
#--------------------------------------------------------------------
#. /usrx/local/Modules/3.2.9/init/ksh
#module use /nwprod2/modulefiles
#module load grib_util
#module load prod_util
#module load util_shared


if [[ $nargs -ge 1 ]]; then
   export PDATE=$2;
   echo "PDATE set to $PDATE"
fi

echo CMON_SUFFIX = $CMON_SUFFIX
echo RUN_ENVIR = $RUN_ENVIR

top_parm=${this_dir}/../../parm

cmon_version_file=${cmon_version:-${top_parm}/ConMon.ver}
if [[ -s ${cmon_version_file} ]]; then
   . ${cmon_version_file}
   echo "able to source ${cmon_version_file}"
else
   echo "Unable to source ${cmon_version_file} file"
   exit 2
fi

cmon_config=${cmon_config:-${top_parm}/ConMon_config}
if [[ -s ${cmon_config} ]]; then
   . ${cmon_config}
   echo "able to source ${cmon_config}"
else
   echo "Unable to source ${cmon_config} file"
   exit 3
fi


jobname=ConMon_de_${CMON_SUFFIX}

#--------------------------------------------------------------------
# Create any missing directories

echo "C_TANKDIR = ${C_TANKDIR}"
echo "C_LOGDIR  = ${C_LOGDIR}"
echo "C_IMGNDIR = ${C_IMGNDIR}"
if [[ ! -d ${C_TANKDIR} ]]; then
   mkdir -p ${C_TANKDIR}
fi
if [[ ! -d ${C_LOGDIR} ]]; then
   mkdir -p ${C_LOGDIR}
fi
if [[ ! -d ${C_IMGNDIR} ]]; then
   mkdir -p ${C_IMGNDIR}
fi


tmpdir=${WORKverf_cmon}/de_cmon_${CMON_SUFFIX}
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

#--------------------------------------------------------------------
# Check status of monitoring job.  Is it already running?  If so, exit
# this script and wait for job to finish.

if [[ $MY_MACHINE = "wcoss" ]]; then
   count=`bjobs -u ${LOGNAME} -p -r -J "${jobname}" | wc -l`
   if [[ $count -ne 0 ]] ; then
      echo "Previous cmon jobs are still running for ${CMON_SUFFIX}" 
      exit 5
   fi
fi

#--------------------------------------------------------------------
# Get date of cycle to process and/or previous cycle processed.
#
if [[ $PDATE = "" ]]; then
   GDATE=`${C_DE_SCRIPTS}/find_cycle.pl 1 ${C_TANKDIR}`
   PDATE=`$NDATE +06 $GDATE`
else
   GDATE=`$NDATE -06 $PDATE`
fi

echo GDATE = $GDATE

PDY=`echo $PDATE|cut -c1-8`
export CYC=`echo $PDATE|cut -c9-10`

export GCYC=`echo $GDATE|cut -c9-10`
export PDYm6h=`echo $GDATE|cut -c1-8`
echo PDYm6h = $PDYm6h


export CNVSTAT_LOCATION=${CNVSTAT_LOCATION:-/gpfs/hps/nco/ops/com/gfs/prod}
export C_DATDIR=${C_DATDIR:-${CNVSTAT_LOCATION}/gdas.$PDY}
export C_GDATDIR=${C_GDATDIR:-${CNVSTAT_LOCATION}/gdas.$PDYm6h}

export C_COMIN=${C_DATDIR}
export C_COMINm6h=${C_GDATDIR}

export DATA_IN=${WORKverf_cmon}
export CMON_WORK_DIR=${CMON_WORK_DIR:-${C_STMP_USER}/cmon_${CMON_SUFFIX}}
pid=$$
export jobid=cmon_DE_${CMON_SUFFIX}.${pid}

#--------------------------------------------------------------------
# If data is available, export variables, and submit driver for
# plot jobs.
#
# Modification here is for prhw14 and prhs13 parallels which only
# generate grib2 files for the analysis and forecast files.  The 
# operational GDAS creates grib and grib2 files.  The Cmon package
# was originally designed to use grib files, but it's clear that
# grib2 will be the only standard with the next major release of 
# GSI. 

export grib2=${grib2:-0}
export cnvstat="${C_DATDIR}/gdas.t${CYC}z.cnvstat"
if [[ ! -s ${cnvstat} ]]; then
   export cnvstat=${C_DATDIR}/cnvstat.gdas.${PDATE}
fi

export pgrbf00="${C_DATDIR}/gdas.t${CYC}z.pgrbf00"
if [[ ! -s ${pgrbf00} ]]; then
   export pgrbf00=${C_DATDIR}/pgbanl.gdas.${PDATE}
fi

export pgrbf06="${C_GDATDIR}/gdas.t${GCYC}z.pgrbf06"
if [[ ! -s ${pgrbf06} ]]; then
   export pgrbf06=${C_DATDIR}/pgbf06.gdas.${GDATE}
fi

exit_value=0
if [ -s $cnvstat  -a -s $pgrbf00 -a -s $pgrbf06 ]; then
   #------------------------------------------------------------------
   #   Submit data extraction job.
   #------------------------------------------------------------------
   if [ -s $pgrbf06 ]; then

      if [[ $MY_MACHINE = "wcoss" ]]; then
        $SUB -q $JOB_QUEUE -P $PROJECT -o $C_LOGDIR/DE.${PDY}.${CYC}.log -M 500 -R affinity[core] -W 0:25 -J ${jobname} -cwd $PWD ${HOMEgdascmon}/jobs/JGDAS_VCMON

      elif [[ $MY_MACHINE = "theia" ]]; then
         $SUB -A $ACCOUNT --ntasks=1 --time=00:20:00 \
		-p service -J ${jobname} -o $C_LOGDIR/DE.${PDY}.${CYC}.log \
		$HOMEgdascmon/jobs/JGDAS_VCMON
      fi

   else
      echo data not available, missing $pgrbf06 file
      exit_value=6
   fi
else
   echo data not available -- missing $cnvstat and/or $pgrbf00 files
   exit_value=7
fi


#--------------------------------------------------------------------
# Clean up and exit
#cd $tmpdir
#cd ../
#rm -rf $tmpdir

echo "End ConMon_DE.sh"
exit ${exit_value}
