#!/bin/ksh -l

#------------------------------------------------------------------
#  mk_time.sh
#
#   - set up working directory
#   - build the cmdfile script
#   - submit plot job
#------------------------------------------------------------------

echo "begin mk_time.sh"
set -ax

export data_source="ges anl"


#------------------------------------------------------------------
# Define working directory for time plots
#
tmpdir=${WORKDIR}/time
tmpdir_sum=${WORKDIR}/summary
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

export WORKDIR=$tmpdir

#------------------------------------------------------------------
#  Expand $OZN_IMGN_TANKDIR for time
#
export OZN_IMGN_TANKDIR=${OZN_IMGN_TANKDIR}/time
if [[ ! -d ${OZN_IMGN_TANKDIR} ]]; then
   mkdir -p ${OZN_IMGN_TANKDIR}
fi

#------------------------------------------------------------------
# Loop over sat types & create entry in cmdfile for each.
#

for dsrc in ${data_source}; do
   suffix=a
   if [[ $dsrc = "ges" ]]; then
      list="cnt omg cpen"
   else
      list="cnt oma cpen"
   fi

   cmdfile=cmdfile_${dsrc}_ptime
   rm -f $cmdfile
   ctr=0

>$cmdfile
   for type in ${SATYPE}; do
      if [[ ${MY_MACHINE} = "hera" ]]; then
         echo "${ctr} ${OZN_IG_SCRIPTS}/plot_time.sh $type $suffix '$list' $dsrc" >> $cmdfile
         ((ctr=ctr+1))
      else
         echo "${OZN_IG_SCRIPTS}/plot_time.sh $type $suffix '$list' $dsrc" >> $cmdfile
      fi
   done
   chmod a+x $cmdfile

   job=${OZNMON_SUFFIX}_ozn_${dsrc}_ptime

   logf=${OZN_LOGdir}/IG.${PDY}.${cyc}.${dsrc}.time.log
   if [[ -e $logf ]]; then
      rm -f $logf
   fi

   errf=${OZN_LOGdir}/IG.${PDY}.${cyc}.${dsrc}.time.err
   if [[ -e $errf ]]; then
      rm -f $errf
   fi

   if [[ ${MY_MACHINE} = "wcoss" ]]; then

      $SUB -q ${JOB_QUEUE} -P ${PROJECT} -M 50 -R affinity[core] \
           -o ${logf} -e ${errf} -W 0:05 -J ${job} -cwd ${WORKDIR} ${WORKDIR}/${cmdfile}

   elif [[ ${MY_MACHINE} = "hera" ]]; then

      $SUB --account ${ACCOUNT} -n $ctr  -o ${logf} -D . -J ${job} --time=10 \
           --wrap "srun -l --multi-prog ${cmdfile}"

   elif [[ ${MY_MACHINE} = "cray" ]]; then

      $SUB -q ${JOB_QUEUE} -P ${PROJECT} -o ${logf} -e ${errf} \
           -R "select[mem>100] rusage[mem=100]" \
           -M 100 -W 0:05 -J ${job} -cwd ${WORKDIR} ${WORKDIR}/${cmdfile}

   elif [[ ${MY_MACHINE} = "wcoss_d" ]]; then

      $SUB -q ${JOB_QUEUE} -P ${PROJECT} -M 50 -R affinity[core] \
           -o ${logf} -e ${errf} -W 0:05 -J ${job} -cwd ${WORKDIR} ${WORKDIR}/${cmdfile}

   fi


done

echo "end mk_time.sh"
exit
