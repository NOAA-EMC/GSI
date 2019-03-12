#!/bin/ksh

#------------------------------------------------------------------
#  mk_time.sh
#
#   - set up working directory
#   - build the cmdfile script
#   - submit plot job
#------------------------------------------------------------------

echo "begin mk_time.sh"
set -ax

export string="ges"


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
suffix=a
list="cnt omg cpen"

cmdfile=cmdfile_ptime
rm -f $cmdfile

>$cmdfile
for type in ${SATYPE}; do
   echo "${OZN_IG_SCRIPTS}/plot_time.sh $type $suffix '$list'" >> $cmdfile
done
chmod a+x $cmdfile

job=${OZNMON_SUFFIX}_ozn_ptime

logf=${OZN_LOGdir}/IG.${PDY}.${cyc}.time.log
if [[ -e $logf ]]; then
   rm -f $logf
fi

errf=${OZN_LOGdir}/IG.${PDY}.${cyc}.time.err
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


echo "end mk_time.sh"
exit
