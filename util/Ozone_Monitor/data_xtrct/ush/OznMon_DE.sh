#!/bin/bash

#--------------------------------------------------------------------
#  usage
#--------------------------------------------------------------------
function usage {
  echo "Usage:  OznMon_DE.sh suffix [pdate]"
  echo "            Suffix is the indentifier for this data source."
  echo "            -p | --pdate yyyymmddcc to specify the cycle to be processed"
  echo "              if unspecified the last available date will be processed"
  echo "            -r | --run   the gdas|gfs run to be processed"
  echo "              use only if data in TANKdir stores both runs"
  echo "            -s | --ostat location of directory to oznstat files"
  echo " "
}

#--------------------------------------------------------------------
#  OznMon_DE.sh begins here
#--------------------------------------------------------------------
set -ax

nargs=$#
if [[ $nargs -lt 1 || $nargs -gt 7 ]]; then
   usage
   exit 1
fi

#-----------------------------------------------
#  Process command line arguments
#
while [[ $# -ge 1 ]]
do
   key="$1"
   echo $key

   case $key in
      -p|--pdate)
         pdate="$2"
         shift # past argument
      ;;
      -r|--run)
         export RUN="$2"
         shift # past argument
      ;;
      -o|--ostat)
         oznstat_dir="$2"
         shift # past argument
      ;;
      *)
         #any unspecified key is OZNMON_SUFFIX
         export OZNMON_SUFFIX=$key
      ;;
   esac

   shift
done

this_file=`basename $0`
this_dir=`dirname $0`

echo "OZNMON_SUFFIX = $OZNMON_SUFFIX"
echo "RUN           = $RUN"
echo "pdate         = $pdate"

top_parm=${this_dir}/../../parm

oznmon_user_settings=${oznmon_user_settings:-${top_parm}/OznMon_user_settings}
if [[ -s ${oznmon_user_settings} ]]; then
   . ${oznmon_user_settings}
   echo "able to source ${oznmon_user_settings}"
else
   echo "Unable to source ${oznmon_user_settings} file"
   exit 4
fi


oznmon_config=${oznmon_config:-${top_parm}/OznMon_config}
if [[ -s ${oznmon_config} ]]; then
   . ${oznmon_config}
   echo "able to source ${oznmon_config}"
else
   echo "Unable to source ${oznmon_config} file"
   exit 3
fi

#-------------------------------------------
#  J-Job needs these assignments to override 
#  operational defaults.
#
export OZN_TANKDIR=$OZN_STATS_TANKDIR
export DATAROOT=${STMP_USER}

if [[ -e ${OZN_TANKDIR}/info/gdas_oznmon_satype.txt ]]; then
   export satype_file=${satype_file:-${OZN_TANKDIR}/info/gdas_oznmon_satype.txt}
fi

#--------------------------------------------------------------
#  Determine next cycle
#    If pdate wasn't an argument then call find_cycle.pl
#    to determine the last processed cycle, and set pdate to
#    the next cycle
#--------------------------------------------------------------
if [[ ${#pdate} -le 0 ]]; then  
   echo "pdate not specified:  setting pdate using last cycle"
   if [[ -d ${OZN_DE_SCRIPTS} ]]; then
      echo "good:  $OZN_DE_SCRIPTS"
   else
      echo "badd:  $OZN_DE_SCRIPTS"
   fi

   echo "OZN_STATS_TANKDIR = $OZN_STATS_TANKDIR"

   date=`${OZN_DE_SCRIPTS}/find_cycle.pl -run gdas -cyc 1 -dir ${OZN_STATS_TANKDIR}`

   echo "date = $date"
   pdate=`$NDATE +6 $date`
else
   echo "pdate was specified:  $pdate"
fi

export PDATE=${pdate}
export PDY=`echo $PDATE|cut -c1-8`
export cyc=`echo $PDATE|cut -c9-10`

mdate=`$NDATE -24 $PDATE`
PDYm1=`echo ${mdate}|cut -c1-8`
cycm1=`echo ${mdate}|cut -c9-10`
echo "PDY, cyc, PDYm1 = ${PDY}, ${cyc} ${PDYm1}"

export TANKverf_ozn=${OZN_TANKDIR}/${RUN}.${PDY}/${cyc}/oznmon
export TANKverf_oznM1=${OZN_TANKDIR}/${RUN}.${PDYm1}/${cycm1}/oznmon

pid=${pid:-$$}


echo "OZN_LOGdir = $OZN_LOGdir"
if [[ ! -d ${OZN_LOGdir} ]]; then
   mkdir -p ${OZN_LOGdir}
fi

#-------------------------------------------------------------
#  define job, jobid for submitted job
#
export job=${job:-oznmon_de_${OZNMON_SUFFIX}}
export jobid=${jobid:-${job}.${cyc}.${pid}}

#-------------------------------------------------------------
#  note:  COMROOT is defined in module prod_envir so in order 
#  to log to a jlogfile that has to be overridden.
#
export COMROOT=${PTMP_USER}

#-------------------------------------------------------------
#  Set COM_IN to default or input value.
#
if [ ${#oznstat_dir} -gt 0 ]; then
   com_in=${oznstat_dir}
else
   com_in=${COM_IN:-/gpfs/dell1/nco/ops/com/gfs/prod}
fi

export COM_IN=${com_in}
export COMROOT=${COMROOT:-/${PTMP_USER}}


#-------------------------------------------------------------
#
#  Note:  J-job's default location for the oznstat file is
#         /com2/gfs/prod/gdas.yyyymmdd/gdas1.hhz.oznstat
#  The directory containing the oznstat file can overriden or
#    the gsistat file can be directly overriden by exporting 
#    a value for $oznstat.
#
echo "MY_MACHINE = $MY_MACHINE"
echo "SUB        = $SUB"
echo "JOB_QUEUE  = $JOB_QUEUE"
echo "jobname    = $jobname" 
echo "job        = $job" 
echo "envir      = $envir"
echo "gdas_oznmon_ver   = $gdas_oznmon_ver"
echo "shared_oznmon_ver = $shared_oznmon_ver"
echo "ACCOUNT    = $ACCOUNT"

jobfile=${jobfile:-${HOMEgdas_ozn}/jobs/JGDAS_ATMOS_VERFOZN}
echo "jobfile = $jobfile"

#-------------------------------------------------------------
#  Note:  regional model use is not yet implemented.
#
#if [[ $GLB_AREA -eq 0 ]]; then
#   jobfile=${jobfile:-${HOMEnam}/jobs/JNAM_VERFOZN}
#else
#   jobfile=${jobfile:-${HOMEgdas_ozn}/jobs/JGDAS_ATMOS_VERFOZN}
#   echo "jobfile = $jobfile"
#fi


#---------------------------------------------------------------
#  expand OZN_WORK_DIR to make unique for this cycle time
#
export OZN_WORK_DIR=${OZN_WORK_DIR}/DE.${PDY}.${cyc}
if [[ -e $OZN_WORK_DIR ]]; then
   rm -rf ${OZN_WORK_DIR}
fi
mkdir -p ${OZN_WORK_DIR}
cd ${OZN_WORK_DIR}

echo "jobfile = $jobfile"
echo "out:  $OZN_LOGdir/DE.$PDY.$cyc.log"
echo "err:  $OZN_LOGdir/DE.$PDY.$cyc.err"

if [[ $MY_MACHINE = "hera" ]]; then
   $SUB --account=${ACCOUNT} --time=05 -J ${job} -D . \
        -o ${OZN_LOGdir}/DE.${PDY}.${cyc}.log \
	--ntasks=1 --mem=5g \
	${jobfile}
	
elif [[ $MY_MACHINE = "wcoss_d" ]]; then

   $SUB -q $JOB_QUEUE -P $PROJECT -M 400 -R affinity[core] \
        -o ${OZN_LOGdir}/DE.${PDY}.${cyc}.log \
        -e ${OZN_LOGdir}/DE.${PDY}.${cyc}.err \
        -W 0:05 -J ${job} -cwd ${PWD} $jobfile

elif [[ $MY_MACHINE = "wcoss_c" ]]; then

  $SUB -q $JOB_QUEUE -P $PROJECT -o ${OZN_LOGdir}/DE.${PDY}.${cyc}.log \
        -e ${OZN_LOGdir}/DE.${PDY}.${cyc}.err \
        -R "select[mem>100] rusage[mem=100]" \
        -M 100 -W 0:05 -J ${job} -cwd ${PWD} $jobfile

fi


echo "end OznMon_DE.sh"
