#!/bin/ksh -l

#------------------------------------------------------------------
#  mk_horiz.sh
#
#   - set up working directory
#   - build the cmdfile script
#   - submit plot job
#------------------------------------------------------------------

echo "begin mk_horiz.sh"
set -ax

echo "GRADS = $GRADS"
echo "STNMAP = $STNMAP"


#------------------------------------------------------------------
# Define working directory for horiz plots
#
tmpdir=${WORKDIR}/horiz
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

export WORKDIR=$tmpdir

#------------------------------------------------------------------
#  Expand $OZN_IMGN_TANKDIR for horiz
#
export OZN_IMGN_TANKDIR=${OZN_IMGN_TANKDIR}/horiz
if [[ ! -d ${OZN_IMGN_TANKDIR} ]]; then
   mkdir -p ${OZN_IMGN_TANKDIR}
fi

#------------------------------------------------------------------
# Loop over sat types & create entry in cmdfile for each.
#
suffix=a

data_source="ges anl"

for dsrc in ${data_source}; do

   cmdfile=cmdfile_${dsrc}_phoriz
   rm -f $cmdfile
>$cmdfile

   ctr=0
   for type in ${SATYPE}; do

      if [[ ${dsrc} = "ges" ]]; then
         list="obs ges obsges"
      else
         list="obs anl obsanl"
      fi

      if [[ ${MY_MACHINE} = "hera" ]]; then
         echo "$ctr ${OZN_IG_SCRIPTS}/plot_horiz.sh $type $suffix '$list' $dsrc" >> $cmdfile
      else
         echo "${OZN_IG_SCRIPTS}/plot_horiz.sh $type $suffix '$list' $dsrc" >> $cmdfile
      fi
      ((ctr=ctr+1))
   done

   chmod a+x $cmdfile

   job=${OZNMON_SUFFIX}_ozn_${dsrc}_phoriz
   o_logfile=${OZN_LOGdir}/plot_horiz.${dsrc}.${PDATE}

   logf=${OZN_LOGdir}/IG.${PDY}.${cyc}.${dsrc}.horiz.log
   if [[ -e $logf ]]; then
     rm -f $logf
   fi

   errf=${OZN_LOGdir}/IG.${PDY}.${cyc}.${dsrc}.horiz.err
   if [[ -e $errf ]]; then
      rm -f $errf
   fi


   if [[ ${MY_MACHINE} = "wcoss" ]]; then

      $SUB -q ${JOB_QUEUE} -P ${PROJECT} -M 50 -R affinity[core] \
           -o ${logf} -e ${errf} -W 0:05 -J ${job} -cwd ${WORKDIR} \
	   ${WORKDIR}/${cmdfile}

   elif [[ ${MY_MACHINE} = "hera" ]]; then

      $SUB --account ${ACCOUNT} -n $ctr  -o ${logf} -D . -J ${job} \
           --time=10 --wrap "srun -l --multi-prog ${cmdfile}"

   elif [[ ${MY_MACHINE} = "cray" ]]; then

      $SUB -q ${JOB_QUEUE} -P ${PROJECT} -o ${logf} -e ${errf} \
           -R "select[mem>100] rusage[mem=100]" \
           -M 100 -W 0:05 -J ${job} -cwd ${WORKDIR} ${WORKDIR}/${cmdfile}

   elif [[ ${MY_MACHINE} = "wcoss_d" ]]; then

      $SUB -q ${JOB_QUEUE} -P ${PROJECT} -M 50 -R affinity[core] \
           -o ${logf} -e ${errf} -W 0:05 -J ${job} -cwd ${WORKDIR} \
	   ${WORKDIR}/${cmdfile}


   fi
 
done

echo "end mk_horiz.sh"
exit
