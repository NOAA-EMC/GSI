#!/bin/sh

set -ax

suffix=GFS
RUN=gdas
CONMON_SUFFIX=${suffix}

this_file=`basename $0`
this_dir=`dirname $0`

top_parm=${this_dir}/../../parm

conmon_config=${conmon_config:-${top_parm}/ConMon_config}
if [[ -s ${conmon_config} ]]; then
   . ${conmon_config}
   echo "able to source ${conmon_config}"
else
   echo "Unable to source ${conmon_config} file"
   exit 3
fi


package=ProdGSI

scripts=${this_dir}
if [[ $scripts == "." ]]; then
   scripts=`pwd`
fi
echo "scripts = $scripts"


export DO_DATA_RPT=1
export DO_DIAG_RPT=1

export DO_ARCHIVE=1
export JOB_QUEUE=dev_shared
#export NUM_CYCLES=120
export NUM_CYCLES=30
cycle_interval=6

#export MAIL_CC="russ.treadon@noaa.gov, john.derber@noaa.gov, andrew.collard@noaa.gov"
export MAIL_CC="edward.c.safford@gmail.com"


data_map=${scripts}/data_map.xml

imgdate=`${scripts}/query_data_map.pl ${data_map} ${CONMON_SUFFIX}_${RUN} imgdate`
idate=`$NDATE +${cycle_interval} $imgdate`

prodate=`${scripts}/find_cycle.pl --cyc 1 --dir ${C_TANKDIR} --run ${RUN}`
echo "imgdate, idate, prodate = $imgdate, $idate, $prodate"

if [[ $idate -le $prodate ]]; then

   logdir=${C_LOGDIR}/${RUN}/conmon

   echo "logdir = $logdir"
   if [[ ! -d ${logdir} ]]; then
      mkdir -p ${logdir}
   fi

   echo " firing ConMon_IG.sh"
   ${scripts}/ConMon_IG.sh ${CONMON_SUFFIX} --pdate ${idate} --run ${RUN} \
	1>${logdir}/ConMon_IG.log \
	2>${logdir}/ConMon_IG.err

   rc=`${scripts}/update_data_map.pl ${data_map} ${CONMON_SUFFIX}_${RUN} imgdate ${idate}`

fi


exit
