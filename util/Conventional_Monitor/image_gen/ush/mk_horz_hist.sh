#!/bin/sh

#------------------------------------------------------------------
#
#  mk_horz_hist.sh
#
#------------------------------------------------------------------
set -ax


   echo "--> mk_horz_hist.sh"

   rc=0
  
   echo "CONMON_SUFFIX = $CONMON_SUFFIX"
   echo "C_TANKDIR   = $C_TANKDIR"

   export PDY=`echo ${PDATE}|cut -c1-8`
   export CYC=`echo ${PDATE}|cut -c9-10`

   export hint=10    ##(mb) the plot pressure interval press+-hint


   #----------------------------------------------------------
   # The list of data types, based on convinfo.txt file
   #----------------------------------------------------------
   export ps_TYPE=" ps120_00 ps180_00 ps181_00 ps183_00 ps187_00 "

   export q_TYPE=" q120_00 q130_00 q132_00 q133_00 q134_00 q135_00 q180_00 q181_00 q182_00 q183_00 q187_00 "

   export t_TYPE=" t120_00 t130_00 t131_00 t132_00 t133_00 t134_00 t135_00 t180_00 t181_00 t182_00 t183_00 t187_00 "

   export uv_TYPE=" uv220_00 uv221_00 uv223_00 uv224_00 uv228_00 uv229_00 uv230_00 uv231_00 uv232_00 uv233_00 uv234_00 uv235_00 uv242_00 uv243_00 uv243_55 uv243_56 uv245_257 uv245_259 uv245_270 uv246_257 uv246_257 uv246_270 uv247_257 uv247_259 uv247_270 uv248_00 uv249_00 uv250_00 uv251_00 uv252_00 uv253_00 uv253_55 uv253_56 uv254_00 uv254_55 uv254_56 uv255_00 uv256_00 uv257_00 uv258_00 uv280_00 uv281_00 uv282_00 uv284_00 uv287_00"


   export nreal_ps=${nreal_ps:-17}
   export nreal_q=${nreal_q:-18}
   export nreal_t=${nreal_t:-22}
   export nreal_uv=${nreal_uv:-21}


   #------------------------------
   # submit the plot_hist job
   #------------------------------

   jobname="${JOBNAME}_hist"
   plot_hist="${C_IG_SCRIPTS}/plot_hist.sh"
   logfile="${C_LOGDIR}/plothist_${CONMON_SUFFIX}.${PDY}.${CYC}.log"
   errfile="${C_LOGDIR}/plothist_${CONMON_SUFFIX}.${PDY}.${CYC}.err"
   rm -f $logfile
   rm -f $errfile

   if [[ ${MY_MACHINE} = "wcoss_d" || ${MY_MACHINE} = "wcoss_c" ]]; then
      ${SUB} -q ${JOB_QUEUE} -P ${PROJECT} -o ${logfile} -M 100 \
   	   -R affinity[core] -W 0:20 -J ${jobname} -cwd ${PWD} ${plot_hist}

   elif [[ $MY_MACHINE = "hera" ]]; then
      ${SUB} -A ${ACCOUNT} --ntasks=1 --time=00:20:00 \
		-p service -J ${jobname} -o ${logfile} ${plot_hist}
   fi


   #------------------------------
   # submit the plot_horz job
   #------------------------------

   jobname="${JOBNAME}_horz"
   plot_horz="${C_IG_SCRIPTS}/plot_horz.sh"
   logfile="${C_LOGDIR}/plothorz_${CONMON_SUFFIX}.${PDY}.${CYC}.log"
   errfile="${C_LOGDIR}/plothorz_${CONMON_SUFFIX}.${PDY}.${CYC}.err"
   rm -f $logfile
   rm -f $errfile

   if [[ $MY_MACHINE = "wcoss_d" || ${MY_MACHINE} = "wcoss_c" ]]; then
      $SUB -q $JOB_QUEUE -P $PROJECT -o ${logfile} -M 300 \
   	   -R affinity[core] -W 0:20 -J ${jobname} -cwd ${PWD} ${plot_horz}

   elif [[ $MY_MACHINE = "hera" ]]; then
      ${SUB} -A ${ACCOUNT} --ntasks=1 --time=00:20:00 \
		-p service -J ${jobname} -o ${logfile} ${plot_horz}
   fi


   #------------------------------
   # submit the plot_horz_uv job
   #------------------------------

   jobname="${JOBNAME}_horz_uv"
   plot_horz_uv="${C_IG_SCRIPTS}/plot_horz_uv.sh"
   logfile="${C_LOGDIR}/plothorz_uv_${CONMON_SUFFIX}.${PDY}.${CYC}.log"
   errfile="${C_LOGDIR}/plothorz_uv_${CONMON_SUFFIX}.${PDY}.${CYC}.err"
   rm -f $logfile
   rm -f $errfile

   if [[ $MY_MACHINE = "wcoss_d" || $MY_MACHINE = "wcoss_c" ]]; then
      $SUB -q $JOB_QUEUE -P $PROJECT -o ${logfile} -M 300 \
	   -R affinity[core] -W 0:20 -J ${jobname} ${plot_horz_uv}

   elif [[ $MY_MACHINE = "hera" ]]; then
      ${SUB} -A ${ACCOUNT} --ntasks=1 --time=00:20:00 \
	     -p service -J ${jobname} -o ${logfile} ${plot_horz_uv}
   fi


echo "<-- mk_horz_hist.sh"
exit ${rc}

