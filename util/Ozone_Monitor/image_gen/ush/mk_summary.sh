#!/bin/ksh -l

#------------------------------------------------------------------
#  mk_summary.sh
#
#   - set up working directory
#   - build the cmdfile script
#   - submit plot job
#------------------------------------------------------------------

echo "begin mk_summary.sh"
set -ax

export process_type="ges anl"


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

for ptype in ${process_type}; do

   if [[ ${ptype} = "ges" ]]; then
      list="count omg cpen"
   else
      list="count oma cpen"
   fi

   suffix=a

   cmdfile=cmdfile_${ptype}_psummary
   rm -f $cmdfile

   ctr=0
>$cmdfile
   for type in ${SATYPE}; do

      #--------------------------------------------------------------------
      #  Note:  This if statement is a a bandaide.  The better solution is  
      #         to poll the ctl file for the source, extract the number of
      #         levels, and only include those instruments where nlev > 1.
      #         Alternately that information could be added to the satype
      #         table, or use the GFS obstype table maybe.  Either way I 
      #         need to fix this fast and engineer a better solution after
      #         some consideration, so it's bandaide now, and elegant
      #         solution in the next release.
      #
      #         Summary plots are dimensioned on the x axis by number of 
      #         levels, so when nlev = 1 the plot script doesn't work.
      #
      if [[ $type != "omi_aura" && $type != "gome_metop-a" && \
	    $type != "gome_metop-b" && $type != "ompstc8_npp" ]]; then
         if [[ ${MY_MACHINE} = "hera" ]]; then
            echo "${ctr} ${OZN_IG_SCRIPTS}/plot_summary.sh $type $ptype" >> $cmdfile
         else
            echo "${OZN_IG_SCRIPTS}/plot_summary.sh $type $ptype" >> $cmdfile
         fi
         ((ctr=ctr+1))
      fi
   done
   chmod a+x $cmdfile

   job=${OZNMON_SUFFIX}_ozn_${ptype}_psummary
   o_logfile=${OZN_LOGdir}/plot_summary.${ptype}.${PDATE}
   if [[ -e ${o_logfile} ]]; then
      rm -f ${o_logfile}
   fi

   logf=${OZN_LOGdir}/IG.${PDY}.${cyc}.${ptype}.summary.log
   if [[ -e $logf ]]; then
      rm -f $logf
   fi

   errf=${OZN_LOGdir}/IG.${PDY}.${cyc}.${ptype}.summary.err
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
           -o ${logf} -e ${errf} -W 0:05 -J ${job} \
	   -cwd ${WORKDIR} ${WORKDIR}/${cmdfile}

   fi

done

echo "end mk_summary.sh"
exit
