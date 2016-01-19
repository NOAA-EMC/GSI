#!/bin/sh
set -xa

#--------------------------------------------------
#
#  mk_time_vert.sh
#
#--------------------------------------------------

echo "--> mk_time_vert.sh"

   export nregion=10

   echo "CMON_SUFFIX = $CMON_SUFFIX"
   echo "C_TANKDIR   = $C_TANKDIR"
   echo "PDATE       = $PDATE"

   export PDY=`echo ${PDATE}|cut -c1-8`
   export CYC=`echo ${PDATE}|cut -c9-10`



   #--------------------------------------------
   #  submit time ps plots
   #--------------------------------------------
   jobname="${JOBNAME}_time_ps"
   logfile="${C_LOGDIR}/plot_time_ps_${CMON_SUFFIX}.${PDY}.${CYC}.log"
   errfile="${C_LOGDIR}/plot_time_ps_${CMON_SUFFIX}.${PDY}.${CYC}.err"
   pltfile="${C_IG_SCRIPTS}/plot_time_ps.sh"
   rm -f $logfile
   rm -f $errfile

   #  wcoss is yet untested
   if [[ $MY_MACHINE == "wcoss" ]]; then
      echo "job for wcoss goes here"
      $SUB -q $JOB_QUEUE -P $PROJECT -o ${logfile} -R affinity[core] -M 100 -W 0:50 -J $jobname $pltfile
   elif [[ $MY_MACHINE == "theia" ]]; then
      echo "ACCOUNT = $ACCOUNT"
      echo "jobname = $jobname"
      ${SUB} -A ${ACCOUNT} -l procs=1,walltime=0:15:00 -N ${jobname} -V -o ${logfile} -e ${errfile} ${pltfile}
   fi

   #--------------------------------------------
   #  submit time q plots
   #--------------------------------------------
   for type in q t uv u v; do
      jobname="${JOBNAME}_time_${type}"
      logfile="${C_LOGDIR}/plot_time_${type}_${CMON_SUFFIX}.${PDY}.${CYC}.log"
      errfile="${C_LOGDIR}/plot_time_${type}_${CMON_SUFFIX}.${PDY}.${CYC}.err"
      pltfile="${C_IG_SCRIPTS}/plot_time.sh "
      export TYPE=${type}
      rm -f $logfile
      rm -f $errfile

      #  wcoss is yet untested
      if [[ $MY_MACHINE == "wcoss" ]]; then
         echo "job for wcoss goes here"
         $SUB -q $JOB_QUEUE -P $PROJECT -o ${logfile} -R affinity[core] -M 100 -W 0:50 -J $jobname $pltfile
      elif [[ $MY_MACHINE == "theia" ]]; then
         echo "ACCOUNT = $ACCOUNT"
         echo "jobname = $jobname"

         if [[ ${type} == "uv" || ${type} == "u" || ${type} == "v" ]]; then
            walltime="walltime=0:22:00"
         else
            walltime="walltime=0:10:00"
         fi
 
         ${SUB} -A ${ACCOUNT} -l procs=1,${walltime} -N ${jobname} -V -o ${logfile} -e ${errfile} ${pltfile}

      fi
   done


   #--------------------------------------------
   #  submit vertical plots
   #--------------------------------------------
   for type in q t uv u v; do
      jobname="${JOBNAME}_vert_${type}"
      logfile="${C_LOGDIR}/plot_vert_${type}_${CMON_SUFFIX}.${PDY}.${CYC}.log"
      errfile="${C_LOGDIR}/plot_vert_${type}_${CMON_SUFFIX}.${PDY}.${CYC}.err"
      pltfile="${C_IG_SCRIPTS}/plot_vert.sh "
      export TYPE=${type}
      rm -f $logfile
      rm -f $errfile

      #  wcoss is yet untested
      if [[ $MY_MACHINE == "wcoss" ]]; then
         echo "job for wcoss goes here"
         $SUB -q $JOB_QUEUE -P $PROJECT -o ${logfile} -R affinity[core] -M 100 -W 0:50 -J $jobname $pltfile
      elif [[ $MY_MACHINE == "theia" ]]; then
         echo "ACCOUNT = $ACCOUNT"
         echo "jobname = $jobname"

         if [[ ${type} == "uv" || ${type} == "u" || ${type} == "v" ]]; then
            walltime="walltime=0:22:00"
         else
            walltime="walltime=0:10:00"
         fi
 
         ${SUB} -A ${ACCOUNT} -l procs=1,${walltime} -N ${jobname} -V -o ${logfile} -e ${errfile} ${pltfile}

      fi
   done



echo "<-- mk_time_vert.sh"

exit
