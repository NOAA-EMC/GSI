#!/bin/sh

#--------------------------------------------------------------------
#  RadMon_DE_rgn.sh
#
#  Extract data for regional source.  
#--------------------------------------------------------------------
echo start RadMon_DE_rgn.sh



#--------------------------------------------------------------------
#  useage function
#--------------------------------------------------------------------
function usage {
  echo "Usage:  RadMon_DE_rgn.sh suffix "
  echo ""
  echo '            Suffix or $NET value is the indentifier for this data source.'
  echo ""
  echo "            -p|--pdate   Full YYYYMMDDHH cycle to run.  This param is optional.  If not"
  echo '               supplied, $TANKverf will be checked and the next cycle time will be run'
  echo ""
  echo '            -r|--run  $run value for this source.  "nam" is the default.'
  echo ""
  echo "            -s|--radstat  Location of directory tree containing the radstat files."
  echo '               This should point to the directory containing the $run.$pdy/*radstat* files.'
  echo ""
  echo "            -i|--int  Cycle interval in hours.  Default is 1."
  echo ""
}


#------------------------------------------------------------------------------
# set_hr_tm() assigns the rgnTM and rgnHH vars which are file name components
#             in the rapid refresh scheme the nam uses.
#------------------------------------------------------------------------------
function set_hr_tm(){

   case $hr in  
      00) rgnHH=t00z
          rgnTM=tm00;;
      01) rgnHH=t06z
          rgnTM=tm05;;
      02) rgnHH=t06z
          rgnTM=tm04;;
      03) rgnHH=t06z
          rgnTM=tm03;;
      04) rgnHH=t06z
          rgnTM=tm02;;
      05) rgnHH=t06z
          rgnTM=tm01;;
      06) rgnHH=t06z
          rgnTM=tm00;;
      07) rgnHH=t12z
          rgnTM=tm05;;
      08) rgnHH=t12z
          rgnTM=tm04;;
      09) rgnHH=t12z
          rgnTM=tm03;;
      10) rgnHH=t12z
          rgnTM=tm02;;
      11) rgnHH=t12z
          rgnTM=tm01;;
      12) rgnHH=t12z
          rgnTM=tm00;;
      13) rgnHH=t18z
          rgnTM=tm05;;
      14) rgnHH=t18z
          rgnTM=tm04;;
      15) rgnHH=t18z
          rgnTM=tm03;;
      16) rgnHH=t18z
          rgnTM=tm02;;
      17) rgnHH=t18z
          rgnTM=tm01;;
      18) rgnHH=t18z	
          rgnTM=tm00;;  
      19) rgnHH=t00z    # This is where the day changes.  
          rgnTM=tm05
          incr=1;;
      20) rgnHH=t00z
          rgnTM=tm04
          incr=1;;
      21) rgnHH=t00z
          rgnTM=tm03
          incr=1;;
      22) rgnHH=t00z
          rgnTM=tm02
          incr=1;;
      23) rgnHH=t00z
          rgnTM=tm01
          incr=1;;
   esac

}


#--------------------------------------------------------------------
#  VrfyRad_rgnl.sh begins here
#--------------------------------------------------------------------
nargs=$#
if [[ $nargs -lt 1 || $nags -gt 9 ]]; then
   usage
   exit 1
fi

this_file=`basename $0`
this_dir=`dirname $0`

#-----------------------------------------------------------
#  Prrocess command line arguments.
#
radstat_location=/gpfs/dell1/nco/ops/com/nam/prod
cycle_interval=1
run=nam

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
         run="$2"
         shift
      ;;
      -i|--int)  # cycle interval
         cycle_interval="$2"
         shift # past argument
      ;;
      -s|--radstat)
         radstat_location="$2"
         shift # past argument
      ;;
      *)
         #any unspecified key is CONMON_SUFFIX
         export RADMON_SUFFIX=$key
      ;;
   esac

   shift
done

echo "RADMON_SUFFIX    = ${RADMON_SUFFIX}"
echo "pdate            = ${pdate}"
echo "run              = ${run}"
echo "radstat_location = ${radstat_location}"
echo "cycle_interval   = ${cycle_interval}"

set -ax

export CYCLE_INTERVAL=${cycle_interval}
export NET=${RADMON_SUFFIX}
export RUN=${run}

export RAD_AREA=rgn


top_parm=${this_dir}/../../parm

radmon_config=${radmon_config:-${top_parm}/RadMon_config}
if [[ ! -e ${radmon_config} ]]; then
   echo "Unable to source ${radmon_config} file"
   exit 2
fi

. ${radmon_config}
if [[ $? -ne 0 ]]; then
   echo "Error detected while sourcing ${radmon_config} file"
   exit $?
fi


radmon_user_settings=${radmon_user_settings:-${top_parm}/RadMon_user_settings}
if [[ ! -e ${radmon_user_settings} ]]; then
   echo "Unable to source ${radmon_user_settings} file"
   exit 3
fi

. ${radmon_user_settings}
if [[ $? -ne 0 ]]; then
   echo "Unable to source ${radmon_user_settings} file"
   exit $?
fi

#---------------------------------------------
#  Create $TANKverf and/or $LOGdir if needed.
#
if [[ ! -d ${TANKverf} ]]; then
   mkdir -p $TANKverf
fi

if [[ ! -d ${LOGdir} ]]; then
   mkdir -p $LOGdir
fi


jobname=${jobname:-RadMon_DE_${RADMON_SUFFIX}}

#--------------------------------------------------------------------
# Determine date of cycle to process.  
#
#   1.  Use the specified date from the command line, if provided.
#
#   2.  Else determine the last processed date, add $CYCLE_INTERVAL
#       hrs to determine the next cycle.  
#--------------------------------------------------------------------

if [[ ${pdate} = "" ]]; then
   ldate=`${DE_SCRIPTS}/find_cycle.pl --cyc 1 --dir ${TANKverf}`

   if [[ ${#ldate} -ne 10 ]]; then
      echo "ERROR:  Unable to locate any previous cycle's data files"
      echo "        Check location of ${TANKVERF}.  If OK then try to"
      echo "        re-run this script using the -p|--pdate option."
      exit 4
   fi



   pdate=`${NDATE} +${CYCLE_INTERVAL} $ldate`
   echo "pdate set to ${pdate}"
fi


#--------------------------------------------------------------------------
#  Locate the correct radstat file
#
#  Note:  namrr is peculiar in that the day rolls over with the
#         t00z.radstat.tm06 file, so cycle times of 18-23 hrs will be 
#         found in the t18z.radstat.tm00-06, which is in tomorrow's
#         directory, or nam.$PDY+24 hrs.
#
pdy=`echo $pdate|cut -c1-8`
cyc=`echo $pdate|cut -c9-10`

hr=${cyc}

rgnHH=''
rgnTM=''
incr=0

set_hr_tm 

echo "incr  = ${incr}"
echo "rgnHH, rgnTM = ${rgnHH}, ${rgnTM}"

#------------------------------------------------------------------
#  If processing on of the last 5 cycles for the day, look for 
#  them in the next day's directory.
#------------------------------------------------------------------
if [[ $rgnHH = "t00z" && $rgnTM != "tm00" ]]; then
   echo " option 1"
   pdate06=`${NDATE} +6 $pdate`
   pdy06=`echo ${pdate06} | cut -c1-8`
   export radstat=${radstat_location}/${run}.${pdy06}/${RADMON_SUFFIX}.${rgnHH}.radstat.${rgnTM}
   export biascr=${radstat_location}/${run}.${pdy06}/${RADMON_SUFFIX}.${rgnHH}.satbias.${rgnTM}
else
   echo " option 2"
   export radstat=${radstat_location}/${run}.${pdy}/${RADMON_SUFFIX}.${rgnHH}.radstat.${rgnTM}
   export biascr=${radstat_location}/${run}.${pdy}/${RADMON_SUFFIX}.${rgnHH}.satbias.${rgnTM}
fi

if [[ ! -e ${radstat} ]]; then
   echo "Unable to locate radstat file ${radstat}"
   exit 5
fi
if [[ ! -e ${biascr} ]]; then
   echo "Unable to locate biascr file ${satbias}"
   exit 6
fi

echo "radstat: ${radstat}"
echo "biascr: ${satbias}"



export PDATE=${pdate}
export PDY=`echo $PDATE|cut -c1-8`
export cyc=`echo $PDATE|cut -c9-10`
export job=${RADMON_SUFFIX}_vrfyrad_${PDY}${cyc}

export DATA=${DATA:-${STMP_USER}/${RADMON_SUFFIX}/${run}/radmon/DE_${PDATE}}  # this should be WORKDIR
cd ${STMP_USER}
rm -rf ${DATA}
mkdir -p ${DATA}

logfile=$LOGdir/DE.${PDY}.${cyc}.log

job=$HOMEnam/jobs/JNAM_VERFRAD

if [[ $MY_MACHINE = "wcoss_d" ]]; then
   $SUB -q $JOB_QUEUE -P $PROJECT -M 40 -R affinity[core] -o ${logfile} \
        -W 0:05 -J ${jobname} -cwd ${PWD} ${job}

elif [[ $MY_MACHINE = "wcoss_c" ]]; then
   $SUB -q $JOB_QUEUE -P $PROJECT -M 40 -o ${logfile} -W 0:10 \
        -J ${jobname} -cwd ${PWD} ${job}

elif [[ $MY_MACHINE = "hera" ]]; then
   $SUB -A $ACCOUNT -l procs=1,walltime=0:05:00 -N ${jobname} -V \
        -j oe -o ${logfile} ${job}

elif [[ $MY_MACHINE = "wcoss2" ]]; then
   $SUB -q $JOB_QUEUE -A $ACCOUNT -o ${logfile} -e ${LOGdir}/DE.${PDY}.${cyc}.err \
        -V -l select=1:mem=5000M -l walltime=20:00 -N ${jobname} ${job}
fi


echo end RadMon_DE_rgn.sh
exit ${exit_value}
