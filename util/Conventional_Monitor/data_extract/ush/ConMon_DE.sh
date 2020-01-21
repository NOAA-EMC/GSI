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
  echo "Usage:  ConMon_DE.sh suffix [-p|--pdate pdate -r|--run gdas|gfs]"
  echo "            Suffix is the indentifier for this data source."
  echo "            -p | --pdate yyyymmddcc to specify the cycle to be processed"
  echo "              if unspecified the last available date will be processed"
  echo "            -r | --run   the gdas|gfs run to be processed"
  echo "              use only if data in TANKdir stores both runs, otherwise"
  echo "	      gdas is assumed."
  echo " "
}


#--------------------------------------------------------------------
#  ConMon_DE.sh begins here
#--------------------------------------------------------------------
set -ax
echo "Begin ConMon_DE.sh"

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


#--------------------------------------------------------------------
#  RUN_ENVIR:  can be "dev", "para", or "prod".
#--------------------------------------------------------------------
export RUN_ENVIR=${RUN_ENVIR:-"prod"}


echo CONMON_SUFFIX = $CONMON_SUFFIX
echo RUN_ENVIR = $RUN_ENVIR
export NET=${CONMON_SUFFIX}

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


jobname=ConMon_DE_${CONMON_SUFFIX}

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

#
#  Make tmpdir and work space conform to *Mon standard
#
#tmpdir=${WORKverf_conmon}/DE_conmon_${CONMON_SUFFIX}
#rm -rf $tmpdir
#mkdir -p $tmpdir
#cd $tmpdir

#--------------------------------------------------------------------
# Check status of monitoring job.  Is it already running?  If so, exit
# this script and wait for job to finish.

#  If I add a pid to the working dir name then more than one can run
#  at the same time.

#if [[ $MY_MACHINE = "wcoss" ]]; then
#   count=`bjobs -u ${LOGNAME} -p -r -J "${jobname}" | wc -l`
#   if [[ $count -ne 0 ]] ; then
#      echo "Previous conmon jobs are still running for ${CONMON_SUFFIX}" 
#      exit 5
#   fi
#fi

#--------------------------------------------------------------------
# Get date of cycle to process and/or previous cycle processed.
#
if [[ $PDATE = "" ]]; then
   GDATE=`${C_DE_SCRIPTS}/find_cycle.pl --cyc 1 --dir ${C_TANKDIR} --run $RUN `
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


export CNVSTAT_LOCATION=${CNVSTAT_LOCATION:-/gpfs/dell1/nco/ops/com/gfs/${RUN_ENVIR}}
#export CNVSTAT_LOCATION=${CNVSTAT_LOCATION:-${COMROOTp3}/gfs/${RUN_ENVIR}}
export C_DATDIR=${C_DATDIR:-${CNVSTAT_LOCATION}/${RUN}.${PDY}}
export C_GDATDIR=${C_GDATDIR:-${CNVSTAT_LOCATION}/${RUN}.${PDYm6h}}

export C_COMIN=${C_DATDIR}
export C_COMINm6h=${C_GDATDIR}

#export DATA_IN=${WORKverf_conmon}
export CONMON_WORK_DIR=${CONMON_WORK_DIR:-${C_STMP_USER}/${CONMON_SUFFIX}}/${RUN}/conmon
pid=$$
export jobid=DE_${PDATE}.${pid}

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

export grib2=${grib2:-1}

#----------------------------------------------------
#  Note to self:
# investigate swapping C_DATDIR for $COMROOTp3
#
export cnvstat="${C_DATDIR}/${CYC}/gdas.t${CYC}z.cnvstat"
if [[ ! -s ${cnvstat} ]]; then
   export cnvstat="${C_DATDIR}/gdas.t${CYC}z.cnvstat"
fi

#---------------
# analysis file
#
export pgrbf00="${C_DATDIR}/${CYC}/gdas.t${CYC}z.pgrb2.1p00.anl"
if [[ ! -s ${pgrbf00} ]]; then
   export pgrbf00="${C_DATDIR}/gdas.t${CYC}z.pgrbf00"
fi

#---------------
# guess file
#
export pgrbf06="${C_GDATDIR}/${GCYC}/gdas.t${GCYC}z.pgrb2.1p00.f006"
if [[ ! -s ${pgrbf06} ]]; then
   export pgrbf06="${C_GDATDIR}/gdas.t${GCYC}z.pgrbf06"
fi

exit_value=0
if [ -s $cnvstat  -a -s $pgrbf00 -a -s $pgrbf06 ]; then
   #------------------------------------------------------------------
   #   Submit data extraction job.
   #------------------------------------------------------------------
   if [ -s $pgrbf06 ]; then

      echo "Ok to proceed with DE"
      logdir=${C_LOGDIR}/${RUN}/conmon
      if [[ ! -d ${logdir} ]]; then
         mkdir -p ${logdir}
      fi

      logfile=${logdir}/DE.${PDY}.${CYC}.log
      if [[ -e ${logfile} ]]; then
         rm -f ${logfile}
      fi

      if [[ $MY_MACHINE = "wcoss" ]]; then
        $SUB -q $JOB_QUEUE -P $PROJECT -o $C_LOGDIR/DE.${PDY}.${CYC}.log -M 500 -R affinity[core] -W 0:25 -J ${jobname} -cwd $PWD ${HOMEgdas_conmon}/jobs/JGDAS_CONMON

      elif [[ $MY_MACHINE = "wcoss_d" ]]; then
        $SUB -q $JOB_QUEUE -P $PROJECT -o ${logfile} -M 200 \
		-R affinity[core] -W 0:25 -J ${jobname} \
		-cwd $PWD ${HOMEgdas_conmon}/jobs/JGDAS_CONMON

      elif [[ $MY_MACHINE = "hera" ]]; then
         $SUB -A $ACCOUNT --ntasks=1 --time=00:20:00 \
		-p service -J ${jobname} -o $C_LOGDIR/DE.${PDY}.${CYC}.log \
		${HOMEgdas_conmon}/jobs/JGDAS_CONMON
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
