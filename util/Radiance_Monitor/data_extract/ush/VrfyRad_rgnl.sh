#!/bin/sh

#--------------------------------------------------------------------
#  VrfyRad_rgnl
#
#  Extract data for regional source.  
#--------------------------------------------------------------------
set -ax
echo start VrfyRad_rgnl.sh



#--------------------------------------------------------------------
#  useage function
#--------------------------------------------------------------------
function usage {
  echo "Usage:  VrfyRad_rgnl.sh suffix [pdate] "
  echo "            File name for VrfyRad_rgnl.sh can be full or relative path"
  echo "            Suffix is the indentifier for this data source."
  echo "            Pdate is the full YYYYMMDDHH cycle to run.  This param is optional"
}


#--------------------------------------------------------------------
#  VrfyRad_rgnl.sh begins here
#--------------------------------------------------------------------
nargs=$#
if [[ $nargs -lt 1 || $nags -gt 3 ]]; then
   usage
   exit 1
fi

. /usrx/local/Modules/3.2.9/init/sh
module load /nwprod2/modulefiles/prod_util/v1.0.2

this_file=`basename $0`
this_dir=`dirname $0`

#--------------------------------------------------------------------
#  Eventually remove RUN_ENVIR argument but allow for it to possibly be
#  present as $2 to ensure backward compatibility.
#
#  if $COMOUT is defined then assume we're in a parallel.
#--------------------------------------------------------------------
export RADMON_SUFFIX=$1
export RUN_ENVIR=""
increment=1          

if [[ $nargs -ge 2 ]]; then
   if [[ $2 = "dev" || $2 = "para" ]]; then
      export RUN_ENVIR=$2;
   else
      export PDATE=$2;
   fi

   if [[ $nargs -eq 3 ]]; then
      export PDATE=$3;
   fi
fi

if [[ $RUN_ENVIR = "" ]]; then
  export RUN_ENVIR="para"
  if [[ $COMOUT = "" ]]; then
     export RUN_ENVIR="dev"
  fi
fi

echo RADMON_SUFFIX = $RADMON_SUFFIX
echo RUN_ENVIR = $RUN_ENVIR


#--------------------------------------------------------------------
# Set environment variables
#--------------------------------------------------------------------
export RAD_AREA=rgn
export MAKE_CTL=${MAKE_CTL:-1}
export MAKE_DATA=${MAKE_DATE:-1}

if [[ ${RUN_ENVIR} = para || ${RUN_ENVIR} = prod ]]; then
   this_dir=${VRFYRAD_DIR}
fi


top_parm=${this_dir}/../../parm
export RADMON_VERSION=${RADMON_VERSION:-${top_parm}/radmon.ver}
if [[ -s ${RADMON_VERSION} ]]; then
   . ${RADMON_VERSION}
else
   echo "Unable to source ${RADMON_VERSION} file"
   exit 2
fi

export RADMON_CONFIG=${RADMON_CONFIG:-${top_parm}/RadMon_config}

if [[ -s ${RADMON_CONFIG} ]]; then
   . ${RADMON_CONFIG}
else
   echo "Unable to source ${RADMON_CONFIG} file"
   exit 2 
fi
if [[ -s ${RADMON_USER_SETTINGS} ]]; then
   . ${RADMON_USER_SETTINGS}
else
   echo "Unable to source ${RADMON_USER_SETTINGS} file"
   exit 3 
fi

. ${DE_PARM}/data_extract_config


#--------------------------------------------------------------------
#  Check setting of RUN_ONLY_ON_DEV and possible abort if on prod and
#  not permitted to run there.
#--------------------------------------------------------------------

if [[ RUN_ONLY_ON_DEV -eq 1 ]]; then
   is_prod=`${DE_SCRIPTS}/onprod.sh`
   if [[ $is_prod = 1 ]]; then
      exit 10
   fi
fi


#--------------------------------------------------------------------

mkdir -p $TANKverf
mkdir -p $LOGdir

jobname=$DATA_EXTRACT_JOBNAME

#--------------------------------------------------------------------
# Check status of monitoring job.  Are any earlier verf jobs still
# running?  If so, exit this script and wait for job to finish.
#
# If we're good to go, clean out the $LOADLQ directory and proceed.
#--------------------------------------------------------------------

if [[ ${RUN_ENVIR} = dev ]]; then
   if [[ $MY_MACHINE = "wcoss" ]]; then
      total=`bjobs -l | grep ${jobname} | wc -l`
   elif [[ $MY_MACHINE = "zeus" || $MY_MACHINE = "theia" ]]; then
      total=0
      line=`qstat -u ${LOGNAME} | grep ${jobname}`
      test=`echo $line | gawk '{print $10}'`

      total=`echo $line | grep ${jobname} | wc -l`
      if [[ $test = "C" && $total -eq "1" ]]; then
         total=0
      fi
   fi

   if [[ $total -gt 0 ]]; then
      exit 4
   fi
fi

tmpdir=${WORKverf_rad}/check_rad${RADMON_SUFFIX}
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

export REGIONAL_RR=${REGIONAL_RR:-0} 		#  regional rapid refresh flag
echo "REGIONAL_RR = ${REGIONAL_RR}"

#------------------------------------------------------------------
#  define data file sources depending on $RUN_ENVIR
#
#  need to idenfity correct output location(s) for binary files
#------------------------------------------------------------------
if [[ $RUN_ENVIR = dev ]]; then

   #--------------------------------------------------------------------
   # Get date of cycle to process.  Use the specified date from the 
   #   command line, if provided.
   #
   #   If no date was provided then determine the last processed date
   #   ($pdate) and add 06 hrs to determine the next cycle ($qdate).  
   #   Also run the find_ndas_radstat.pl script to determine the 
   #   earliest date for which a radstat file exists ($fdate).  
   #   Sometimes there are breaks in radstat file availability.  If the 
   #   next cycle is for a date less than the next available date, then 
   #   we have to use the next available date.
   #--------------------------------------------------------------------
   export DATDIR=${PTMP_USER}/regional
   export com=${RADSTAT_LOCATION}

   if [[ $PDATE = "" ]]; then
      pdate=`${DE_SCRIPTS}/find_cycle.pl 1 ${TANKverf}`

      if [[ ${#pdate} -ne 10 ]]; then
         echo "ERROR:  Unable to locate any previous cycle's data files"
         echo "        Re-run this script with a specified starting cycle"
         exit 5
      fi

      # --------------------------------------------------------------------
      #  CYCLE_INTERVAL comes from the ../../parm/RadMon_user_settings file
      # --------------------------------------------------------------------
      if [[ $REGIONAL_RR -eq 1 ]]; then
         # if there is no $pdate we may or may not increment the $pdate
         # depending on the rgnHH and rgnTM settings
         # get PDY, rgnHH, rgnTM      

         new_day=0
         hr=`echo $pdate|cut -c9-10`
         if [[ $hr = "00" || $hr = "06" || $hr = "12" || $hr = "18" ]]; then
            rgnHH=`${DE_SCRIPTS}/rr_get_tHHz.pl $pdate ${TANKverf}` 
            
            if [[ $hr = "00" && $rgnHH = "t00z" ]]; then
               increment=0
               rgnHH="t06z"
               rgnTM="tm06"
            elif [[ $hr = "06" && $rgnHH = "t06z" ]]; then
               increment=0
               rgnHH="t12z"
               rgnTM="tm06"
            elif [[ $hr = "12" && $rgnHH = "t12z" ]]; then
               increment=0
               rgnHH="t18z"
               rgnTM="tm06"
            elif [[ $hr = "18" && $rgnHH = "t18z" ]]; then
               increment=0
               rgnHH="t00z"
               rgnTM="tm06"
            fi
         fi

         if [[ $increment -eq 1 ]]; then          
            qdate=`${NDATE} +${CYCLE_INTERVAL} $pdate`	# namrr is peculiar in that that
#         elif [[ $new_day -eq 1 ]]; then		# day rolls over with the
#            qdate=`${NDATE} +24 $pdate`			# t00z.radstat.tm06 file
#            echo "ADVANCE from $pdate to $qdate"	# t18z.radstat.tm00 was yesterday
         else 
            qdate=$pdate
         fi 

      else
         qdate=`${NDATE} +${CYCLE_INTERVAL} $pdate`
      fi

      if [[ $REGIONAL_RR -eq 0 ]]; then
         fdate=`${DE_SCRIPTS}/find_ndas_radstat.pl 0 $com`
         echo $fdate

         if [[ $qdate -ge $fdate ]]; then
            export PDATE=$qdate
         else 
            export PDATE=$fdate
         fi
      else		# REGIONAL_RR 
         export PDATE=$qdate
      fi
   fi 

   sdate=`echo $PDATE|cut -c1-8`
   export CYA=`echo $PDATE|cut -c9-10`

   #---------------------------------------------------------------
   # Locate required files.
   #---------------------------------------------------------------
   echo $PDATE

   if [[ $REGIONAL_RR -eq 1 ]]; then
      echo increment = $increment

      if [[ $increment -eq 1 ]]; then   
         PDY00=`echo $PDATE | cut -c 1-8` 
         HH00=`echo $PDATE | cut -c 9-10`
         case $HH00 in  
            00) export rgnHH=t00z
                export rgnTM=tm00;;
            01) export rgnHH=t06z
                export rgnTM=tm05;;
            02) export rgnHH=t06z
                export rgnTM=tm04;;
            03) export rgnHH=t06z
                export rgnTM=tm03;;
            04) export rgnHH=t06z
                export rgnTM=tm02;;
            05) export rgnHH=t06z
                export rgnTM=tm01;;
            06) export rgnHH=t06z
                export rgnTM=tm00;;
            07) export rgnHH=t12z
                export rgnTM=tm05;;
            08) export rgnHH=t12z
                export rgnTM=tm04;;
            09) export rgnHH=t12z
                export rgnTM=tm03;;
            10) export rgnHH=t12z
                export rgnTM=tm02;;
            11) export rgnHH=t12z
                export rgnTM=tm01;;
            12) export rgnHH=t12z
                export rgnTM=tm00;;
            13) export rgnHH=t18z
                export rgnTM=tm05;;
            14) export rgnHH=t18z
                export rgnTM=tm04;;
            15) export rgnHH=t18z
                export rgnTM=tm03;;
            16) export rgnHH=t18z
                export rgnTM=tm02;;
            17) export rgnHH=t18z
                export rgnTM=tm01;;
            18) export rgnHH=t18z	# day changes here?!
                export rgnTM=tm00;;
            19) export rgnHH=t00z
                export rgnTM=tm05;;
            20) export rgnHH=t00z
                export rgnTM=tm04;;
            21) export rgnHH=t00z
                export rgnTM=tm03;;
            22) export rgnHH=t00z
                export rgnTM=tm02;;
            23) export rgnHH=t00z
                export rgnTM=tm01;;
         esac
      fi

      echo "PDATE = $PDATE"
      echo "DATDIR = $DATDIR"
      echo "com    = $com"
      echo "rgnHH  = $rgnHH"
      echo "rgnTM  = $rgnTM"
      /bin/sh ${DE_SCRIPTS}/getbestnamrr_radstat.sh ${PDATE} ${DATDIR} ${com} ${rgnHH} ${rgnTM}

   else

      /bin/sh ${DE_SCRIPTS}/getbestndas_radstat.sh ${PDATE} ${DATDIR} ${com}
   fi

   echo RADSTAT = $radstat
   echo BIASCR  = $biascr



elif [[ ${RUN_ENVIR} = para ]]; then
   #  need to change this logic in line with glbl version
   #  can't default to ndas 

   #---------------------------------------------------------------
   # Locate required files.
   #---------------------------------------------------------------
   
   export DATDIR=${PTMP_USER}/regional
   export com=`dirname ${COMOUT}`
   export PDATE=${CDATE}

   sdate=`echo ${PDATE}|cut -c1-8`
   export CYA=`echo ${PDATE}|cut -c9-10`

   /bin/sh ${DE_SCRIPTS}/getbestndas_radstat.sh $PDATE $DATDIR $com

else
   echo RUN_ENVIR = $RUN_ENVIR
   exit 1
fi


export biascr=$DATDIR/satbias.${PDATE}
export radstat=$DATDIR/radstat.${PDATE}

#--------------------------------------------------------------------
# If data is available, export variables, and submit driver for
# radiance monitoring jobs.
#--------------------------------------------------------------------

data_available=0

if [ -s $radstat -a -s $biascr ]; then
   data_available=1

   export MP_SHARED_MEMORY=yes
   export MEMORY_AFFINITY=MCM
   export envir=prod

   export PDY=`echo $PDATE|cut -c1-8`
   export cyc=`echo $PDATE|cut -c9-10`

   export job=${RADMON_SUFFIX}_vrfyrad_${PDY}${cyc}
   export SENDSMS=${SENDSMS:-NO}
   export DATA_IN=${WORKverf_rad}
   export DATA=${DATA:-${STMP_USER}/radmon_de_${RADMON_SUFFIX}}
   cd ${STMP_USER}
   rm -rf ${DATA}
   mkdir ${DATA}
   export jlogfile=${WORKverf_rad}/jlogfile_${RADMON_SUFFIX}

   export VERBOSE=${VERBOSE:-YES}

  
   #----------------------------------------------------------------------------
   #  Advance the satype file from previous day.
   #  If it isn't found then create one using the contents of the radstat file.
   #----------------------------------------------------------------------------
   my_satype_file=${TANKverf}/radmon.${PDY}/${RADMON_SUFFIX}_radmon_satype.txt

   #  logic here needs work.
   #  point TANKverf_rad to radmon.${next_day} for all of the t00z cycles
   #  maybe always try to make the directory just to be sure its there.

   if [[ $REGIONAL_RR -eq 1 ]]; then
      if [[ $cyc -ge 18 ]]; then
         echo "Making new day directory for 18 cycle"
         next_day=`${NDATE} +06 $PDATE | cut -c1-8`

         export TANKverf_rad=${TANKverf}/radmon.${next_day}
#         if [[ ! -d ${TANKverf_rad} ]]; then
#            mkdir -p ${TANKverf_rad}
#         fi

#         prev_day=`${NDATE} -06 $PDATE | cut -c1-8`
         if [[ -s ${TANKverf}/radmon.${PDY}/${RADMON_SUFFIX}_radmon_satype.txt ]]; then
            cp ${TANKverf}/radmon.${PDY}/${RADMON_SUFFIX}_radmon_satype.txt ${TANKverf}/radmon.${next_day}/.
         fi
       fi
    else
      if [[ $cyc = "00" ]]; then
         echo "Making new day directory for 00 cycle"
         mkdir -p ${TANKverf}/radmon.${PDY}
         prev_day=`${NDATE} -06 $PDATE | cut -c1-8`
         if [[ -s ${TANKverf}/radmon.${prev_day}/${RADMON_SUFFIX}_radmon_satype.txt ]]; then
            cp ${TANKverf}/radmon.${prev_day}/${RADMON_SUFFIX}_radmon_satype.txt ${TANKverf}/radmon.${PDY}/.
         fi
       fi
    fi
    echo "TESTING for $my_satype_file"
    if [[ -s ${my_satype_file} ]]; then
      echo "${my_satype_file} is good to go"
    else
      echo "CREATING satype file"
      radstat_satype=`tar -tvf $radstat | grep _ges | awk -F_ '{ print $2 "_" $3 }'`
      echo $radstat_satype > ${my_satype_file}
      echo "CREATED ${my_satype_file}"
    fi

 
   #------------------------------------------------------------------
   #   Override the default base_file declaration if there is an
   #   available base file for this source.
   #------------------------------------------------------------------
   if [[ -s ${TANKverf}/info/radmon_base.tar.${Z} || -s ${TANKverf}/info/radmon_base.tar ]]; then
      export base_file=${TANKverf}/info/radmon_base.tar
   fi


   #------------------------------------------------------------------
   #   Submit data processing jobs.

   logfile=$LOGdir/data_extract.${RADMON_SUFFIX}.${PDY}.${cyc}.log

   if [[ $MY_MACHINE = "wcoss" ]]; then
      $SUB -q $JOB_QUEUE -P $PROJECT -M 40 -R affinity[core] -o ${logfile} \
           -W 0:10 -J ${jobname} -cwd ${PWD} $HOMEnam/jobs/JNAM_VERFRAD
   elif [[ $MY_MACHINE = "cray" ]]; then
      $SUB -q $JOB_QUEUE -P $PROJECT -M 40 -o ${logfile} -W 0:10 \
           -J ${jobname} -cwd ${PWD} $HOMEnam/jobs/JNAM_VERFRAD
   elif [[ $MY_MACHINE = "zeus" || $MY_MACHINE = "theia"  ]]; then
      $SUB -A $ACCOUNT -l procs=1,walltime=0:05:00 -N ${jobname} -V \
           -j oe -o ${logfile} ${HOMEnam}/jobs/JNAM_VERFRAD
   fi

fi

#--------------------------------------------------------------------
# Clean up and exit
#--------------------------------------------------------------------
#cd $tmpdir
#cd ../
#rm -rf $tmpdir

exit_value=0
if [[ ${data_available} -ne 1 ]]; then
   echo No data available for ${RADMON_SUFFIX}
   exit_value=6
fi

module unload /nwprod2/modulefiles/prod_util/v1.0.2

echo end VrfyRad_rgn.sh
exit ${exit_value}
