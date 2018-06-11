#!/bin/ksh

#------------------------------------------------------------------
#  mk_summary.sh
#
#   - set up working directory
#   - build the cmdfile script
#   - submit plot job
#------------------------------------------------------------------

echo "begin mk_summary.sh"
set -ax

export string=ges


#------------------------------------------------------------------
# Define working directory for summary plots
#
tmpdir=${WORKDIR}/summary
export WORKDIR=$tmpdir

rm -rf ${WORKDIR}
mkdir -p ${WORKDIR}
cd ${WORKDIR}

#------------------------------------------------------------------
#  Expand $OZN_IMGN_TANKDIR for summary
#
export OZN_IMGN_TANKDIR=${OZN_IMGN_TANKDIR}/summary
if [[ ! -d ${OZN_IMGN_TANKDIR} ]]; then
   mkdir -p ${OZN_IMGN_TANKDIR}
fi

#------------------------------------------------------------------
# Loop over sat types and create entry in cmdfile for each.
#

if [[ ${string} = "ges" ]]; then

   suffix=a
   list="count omg cpen"

   cmdfile=cmdfile_psummary
   rm -f $cmdfile

>$cmdfile
   for type in ${SATYPE}; do
      if [[ $type != "omi_aura" && $type != "gome_metop-a" && $type != "gome_metop-b" ]]; then
         echo "${OZN_IG_SCRIPTS}/plot_summary.sh $type" >> $cmdfile
      fi
   done
   chmod a+x $cmdfile

   job=${OZNMON_SUFFIX}_ozn_psummary
   o_logfile=${OZN_LOGdir}/plot_summary.${PDATE}
   if [[ -e ${o_logfile} ]]; then
      rm -f ${o_logfile}
   fi

   logf=${OZN_LOGdir}/IG.${PDY}.${cyc}.summary.log
   if [[ -e $logf ]]; then
      rm -f $logf
   fi

   errf=${OZN_LOGdir}/IG.${PDY}.${cyc}.summary.err
   if [[ -e $errf ]]; then
      rm -f $errf
   fi

   if [[ ${MY_MACHINE} = "wcoss" ]]; then

      $SUB -q ${JOB_QUEUE} -P ${PROJECT} -M 50 -R affinity[core] \
           -o ${logf} -e ${errf} -W 0:05 -J ${job} -cwd ${WORKDIR} ${WORKDIR}/${cmdfile}

   elif [[ ${MY_MACHINE} = "theia" ]]; then
     
      $SUB -A ${ACCOUNT} -l procs=1,walltime=0:05:00 -N ${job} -V \
         -o ${logf} -e ${errf} ${cmdfile}

   elif [[ ${MY_MACHINE} = "cray" ]]; then

      $SUB -q ${JOB_QUEUE} -P ${PROJECT} -o ${logf} -e ${errf} \
           -R "select[mem>100] rusage[mem=100]" \
           -M 100 -W 0:05 -J ${job} -cwd ${WORKDIR} ${WORKDIR}/${cmdfile}
   fi

fi

echo "end mk_summary.sh"
exit
