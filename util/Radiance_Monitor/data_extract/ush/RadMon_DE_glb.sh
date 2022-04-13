#!/bin/sh

#--------------------------------------------------------------------
#  RadMon_DE_glbl.sh
#
#  Data extraction script for global (GDAS) radiance diagnostic data.
#
#  This script verifies data is available and submits the
#  JGDAS_ATMOS_VERFRAD job, which performs the data extraction and
#  validation checks. 
#--------------------------------------------------------------------
echo start VrfyRad_glbl.sh


#--------------------------------------------------------------------
#  usage
#--------------------------------------------------------------------
function usage {
  echo "Usage:  RadMon_DE_glbl.sh suffix [-p|--pdate -r|--run -s|--radstat]"
  echo ""
  echo "            suffix is the indentifier for this data source."
  echo "              This is usually the same as the NET value."
  echo ""
  echo "            -p|--pdate is the full YYYYMMDDHH cycle to run.  If not specified"
  echo "              the TANKverf directory will be used to determine the next cycle time"
  echo ""
  echo "            -r|--run is gdas|gfs.  gdas is the default if not specified."
  echo ""
  echo "            -s|--radstat is the location of the directory containing the radstat"
  echo '              files.  Note that this is the parent directory to the ${run}.${pdy} directory'  
}

#--------------------------------------------------------------------
#  RadMon_DE_glbl begins here.
#--------------------------------------------------------------------
exit_value=0

nargs=$#
echo "nargs = $nargs"
if [[ $nargs -lt 1 || $nargs -gt 7 ]]; then
   usage
   exit 1
fi

#-----------------------------------------------------------
#  Set default values and process command line arguments.
#
run=gdas
radstat_location=/gpfs/dell1/nco/ops/com/gfs/prod

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
         run="$2"
         shift # past argument
      ;;
      -s|--radstat)
         radstat_location="$2"
         shift # past argument
      ;;
      *)
         #any unspecified key is RADMON_SUFFIX
         export RADMON_SUFFIX=$key
      ;;
   esac

   shift
done

export RADSTAT_LOCATION=$radstat_location
export RUN=$run

echo "RADMON_SUFFIX    = $RADMON_SUFFIX"
echo "RUN              = $RUN"
echo "PDATE            = $PDATE"
echo "RADSTAT_LOCATION = $RADSTAT_LOCATION"

set -ax

this_file=`basename $0`
this_dir=`dirname $0`


#--------------------------------------------------------------------
# Source config and user_settings files
#--------------------------------------------------------------------

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


#-------------------------------------------------------
#  Create log and TANK directories if they don't exist
#-------------------------------------------------------
if [[ ! -d ${TANKverf} ]]; then
   mkdir -p $TANKverf
fi
if [[ ! -d ${LOGdir} ]]; then
   mkdir -p $LOGdir
fi


#---------------------------------------------------------------
# Get date of cycle to process.  If it hasn't been specified 
# in the arugments get the last cycle processed and add 6 hrs.
#---------------------------------------------------------------
if [[ $PDATE = "" ]]; then
   idate=`${DE_SCRIPTS}/nu_find_cycle.pl --run $RUN --cyc 1 --dir ${TANKverf}`

   if [[ ${#idate} -ne 10 ]]; then
      echo "ERROR:  Unable to locate any previous cycle's data files"
      echo "        Please re-run this script with a specified starting cycle as the last argument"
      exit 4
   fi

   export PDATE=`${NDATE} +06 $idate`
fi

echo "PDATE = $PDATE"

pdy=`echo $PDATE|cut -c1-8`
cyc=`echo $PDATE|cut -c9-10`
export PDY=${pdy}
export cyc=${cyc}

#---------------------------------------------------------------
# Locate radstat and biascr files.             
#---------------------------------------------------------------
component=atmos

if [[ -d ${RADSTAT_LOCATION}/gdas.${pdy}/${cyc}/${component} ]]; then
   echo "looking for radstat in default location"

   datadir=${RADSTAT_LOCATION}/gdas.${pdy}/${cyc}/${component}
   export biascr=${datadir}/gdas.t${cyc}z.abias  
   export radstat=${datadir}/gdas.t${cyc}z.radstat

elif [[ -d ${RADSTAT_LOCATION}/gdas.${pdy}/${cyc} ]]; then
   echo "looking for radstat in secondary location"

   export datadir=${RADSTAT_LOCATION}/gdas.${pdy}/${cyc}
   export biascr=${datadir}/gdas.t${cyc}z.abias  
   export radstat=${datadir}/gdas.t${cyc}z.radstat

elif [[ -d ${RADSTAT_LOCATION} ]]; then
   echo "looking for radstat in tertiary location"

   export datadir=${RADSTAT_LOCATION}
   export biascr=${datadir}/gdas.t${cyc}z.abias  
   export radstat=${datadir}/gdas.t${cyc}z.radstat

else
   echo "unable to locate expected subdirectories in ${RADSTAT_LOCATION}"
   exit 5
fi


#--------------------------------------------------------------------
# If data is available, export variables, and submit J-job.
#--------------------------------------------------------------------

if [[ -e ${radstat} && -e ${biascr} ]]; then
   echo "FOUND radstat and biascr -- we are good to go"

   export jlogfile=${WORKverf_rad}/jlogfile_${RADMON_SUFFIX}

   export VERBOSE=${VERBOSE:-YES}
   prev_day=`${NDATE} -06 $PDATE | cut -c1-8`
   prev_cyc=`${NDATE} -06 $PDATE | cut -c9-10`

   if [[ -e ${TANKverf}/info/${run}_radmon_satype.txt ]]; then
      export satype_file=${TANKverf}/info/${run}_radmon_satype.txt 
   fi
   export TANKverf_rad=${TANKverf_rad:-${TANKverf}/${RUN}.${pdy}/${cyc}/${MONITOR}} 
   export TANKverf_radM1=${TANKverf_radM1:-${TANKverf}/${RUN}.${prev_day}/${prev_cyc}/${MONITOR}}

   if [[ $cyc = "00" ]]; then
      echo "Making new day directory for 00 cycle"
      mkdir -p ${TANKverf_rad}
   fi 

   
   #------------------------------------------------------------------
   #   Override the default base_file declaration if there is an  
   #   available base file for this source.
   #------------------------------------------------------------------
   if [[ -s ${TANKverf}/info/radmon_base.tar.${Z} || -s ${TANKverf}/info/radmon_base.tar ]]; then
      export base_file=${TANKverf}/info/radmon_base.tar 
   fi

   export RAD_DATA_IN=${RAD_DATA_IN:-${DATAROOT}/DE.${pdy}${cyc}}

   if [[ -d ${RAD_DATA_IN} ]]; then
      rm -rf ${RAD_DATA_IN}
      mkdir -p ${RAD_DATA_IN}
   fi

   job=${HOMEgdas}/jobs/JGDAS_ATMOS_VERFRAD  
   jobname=RadMon_DE_${RADMON_SUFFIX}
   logfile=${LOGdir}/DE.${pdy}.${cyc}.log
   if [[ -e ${logfile} ]]; then
     rm -f ${logfile}
   fi


   if [[ $MY_MACHINE = "wcoss_d" ]]; then
      $SUB -q $JOB_QUEUE -P $PROJECT -o ${logfile} \
           -M 5000 -R affinity[core] -W 0:20 -J ${jobname} -cwd ${PWD} ${job}

   elif [[ $MY_MACHINE = "wcoss_c" ]]; then
      $SUB -q $JOB_QUEUE -P $PROJECT -o ${logfile} \
           -M 5000 -W 0:20 -J ${jobname} -cwd ${PWD} ${job}

   elif [[ $MY_MACHINE = "hera" ]]; then
      $SUB --account=${ACCOUNT} --time=10 -J ${jobname} -D . \
        -o ${logfile} --ntasks=1 --mem=5g ${job} 

   elif [[ $MY_MACHINE = "wcoss2" ]]; then
      $SUB -q $JOB_QUEUE -A $ACCOUNT -o ${logfile} -e ${LOGdir}/DE.${pdy}.${cyc}.err \
        -V -l select=1:mem=5000M -l walltime=20:00 -N ${jobname} ${job}
   fi

else  # radstat and/or biascr not found

   if [[ ! -e ${radstat} ]]; then
      echo "${radstat} RADSTAT FILE NOT FOUND"
   fi
   if [[ ! -e ${biascr} ]]; then
      echo "${biascr} BIASCR FILE NOT FOUND"
   fi
   exit 6
fi


echo end VrfyRad_glbl.sh


exit ${exit_value}

