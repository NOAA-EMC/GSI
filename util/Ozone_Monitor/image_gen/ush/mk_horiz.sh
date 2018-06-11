#!/bin/ksh

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

cmdfile=cmdfile_phoriz
rm -f $cmdfile


>$cmdfile
for type in ${SATYPE}; do
#   if [[ $type == 'omi_aura' || $type == 'gome_metop-a' ]] ; then
#      list="obs ges obsges sza fovn"
#   else
      list="obs ges obsges"
#   fi
   echo "${OZN_IG_SCRIPTS}/plot_horiz.sh $type $suffix '$list'" >> $cmdfile
done
chmod a+x $cmdfile

job=${OZNMON_SUFFIX}_ozn_phoriz
o_logfile=${OZN_LOGdir}/plot_horiz.${PDATE}

logf=${OZN_LOGdir}/IG.${PDY}.${cyc}.horiz.log
errf=${OZN_LOGdir}/IG.${PDY}.${cyc}.horiz.err

if [[ -e $logf ]]; then
  rm -f $logf
fi

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
 

echo "end mk_horiz.sh"
exit
