#!/bin/sh
set -xa

#--------------------------------------------------
#
#  mk_time_vert.sh
#
#--------------------------------------------------

echo "--> mk_time_vert.sh"

   export nregion=10

   echo "CONMON_SUFFIX = $CONMON_SUFFIX"
   echo "C_TANKDIR   = $C_TANKDIR"
   echo "PDATE       = $PDATE"

   export PDY=`echo ${PDATE}|cut -c1-8`
   export CYC=`echo ${PDATE}|cut -c9-10`



   #--------------------------------------------
   #  submit time ps plots
   #--------------------------------------------
   jobname="${JOBNAME}_time_ps"
   logfile="${C_LOGDIR}/plot_time_ps_${CONMON_SUFFIX}.${PDY}.${CYC}.log"
   errfile="${C_LOGDIR}/plot_time_ps_${CONMON_SUFFIX}.${PDY}.${CYC}.err"
   pltfile="${C_IG_SCRIPTS}/plot_time_ps.sh"
   rm -f $logfile
   rm -f $errfile

   if [[ $MY_MACHINE == "wcoss_d" || ${MY_MACHINE} = "wcoss_c" ]]; then
      $SUB -q ${JOB_QUEUE} -P ${PROJECT} -o ${logfile} -R affinity[core] \
		-M 100 -W 0:50 -J ${jobname} -cwd ${PWD} ${pltfile}

   elif [[ $MY_MACHINE == "hera" ]]; then
      ${SUB} -A ${ACCOUNT} --ntasks=1 --time=00:15:00 \
                -p service -J ${jobname} -o ${logfile} ${pltfile}
   fi

   #--------------------------------------------
   #  submit time plots
   #--------------------------------------------
   for type in q t uv; do
      jobname="${JOBNAME}_time_${type}"
      logfile="${C_LOGDIR}/plot_time_${type}_${CONMON_SUFFIX}.${PDY}.${CYC}.log"
      errfile="${C_LOGDIR}/plot_time_${type}_${CONMON_SUFFIX}.${PDY}.${CYC}.err"
      pltfile="${C_IG_SCRIPTS}/plot_time.sh "
      export TYPE=${type}
      rm -f $logfile
      rm -f $errfile

      if [[ $MY_MACHINE == "wcoss_d" || ${MY_MACHINE} = "wcoss_c" ]]; then

         if [[ ${type} == "uv" || ${type} == "u" || ${type} == "v" ]]; then
            walltime="02:30"
         else
            walltime="00:50"
         fi

         $SUB -q ${JOB_QUEUE} -P ${PROJECT} -o ${logfile} -R affinity[core] \
		-M 100 -W ${walltime} -J ${jobname} -cwd ${PWD} ${pltfile}

      elif [[ $MY_MACHINE == "hera" ]]; then
         if [[ ${type} == "uv" || ${type} == "u" || ${type} == "v" ]]; then
            walltime="02:30:00"
         else
            walltime="00:40:00"
         fi
 
         ${SUB} -A ${ACCOUNT} --ntasks=1 --time=${walltime} \
                -p service -J ${jobname} -o ${logfile} ${pltfile}

      fi
   done


   #--------------------------------------------
   #  submit vertical plots
   #--------------------------------------------
   for type in q t uv u v; do

      jobname="${JOBNAME}_vert_${type}"
      logfile="${C_LOGDIR}/plot_vert_${type}_${CONMON_SUFFIX}.${PDY}.${CYC}.log"
      errfile="${C_LOGDIR}/plot_vert_${type}_${CONMON_SUFFIX}.${PDY}.${CYC}.err"
      pltfile="${C_IG_SCRIPTS}/plot_vert.sh "
      export TYPE=${type}
      rm -f $logfile
      rm -f $errfile

      if [[ $MY_MACHINE == "wcoss_d" || ${MY_MACHINE} = "wcoss_c" ]]; then
         $SUB -q ${JOB_QUEUE} -P ${PROJECT} -o ${logfile} -R affinity[core] \
	      -M 100 -W 1:30 -J ${jobname} -cwd ${PWD} ${pltfile}

      elif [[ $MY_MACHINE == "hera" ]]; then
         if [[ ${type} == "uv" || ${type} == "u" || ${type} == "v" ]]; then
            walltime="00:50:00"
         else
            walltime="00:30:00"
         fi
 
         ${SUB} -A ${ACCOUNT} --ntasks=1 --time=${walltime} \
                -p service -J ${jobname} -o ${logfile} ${pltfile}

      fi
   done


echo "<-- mk_time_vert.sh"

exit
