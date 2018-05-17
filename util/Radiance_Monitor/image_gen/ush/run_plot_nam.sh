#!/bin/sh

echo starting run_plot_nam.sh

idev=`cat /etc/dev | cut -c1`
iprod=`cat /etc/prod | cut -c1`

export PROJECT=NDAS-T2O
export RADSTAT_LOCATION=/com/nam/prod
export DO_ARCHIVE=1
export DO_DATA_RPT=1
export DO_DIAG_RPT=1
export JOB_QUEUE=dev_shared
export MAIL_TO="edward.safford@noaa.gov"
#export MAIL_CC="russ.treadon"
#export MAIL_CC="edward.c.safford@gmail.com"

export RADMON_SUFFIX=nam
export TANK_USE_RUN=0
export RUN=""
export RAD_AREA=rgn
export CYCLE_INTERVAL=1
export USE_STATIC_SATYPE=1
export REGIONAL_RR=1

package=ProdGSI/util/Radiance_Monitor
scripts=/gpfs/${idev}d2/emc/da/noscrub/Edward.Safford/${package}/image_gen/ush

NDATE=/nwprod/util/exec/ndate
data_map=/gpfs/${idev}d2/emc/da/noscrub/Edward.Safford/${package}/parm/data_map.xml

tankdir=/gpfs/${idev}d2/emc/da/noscrub/Edward.Safford/nbns/stats/regional/nam
imgdate=`${scripts}/query_data_map.pl ${data_map} nam imgdate`

idate=`$NDATE +${CYCLE_INTERVAL} $imgdate`

prodate=`${scripts}/find_cycle.pl --cyc 1 --dir ${tankdir}`

echo idate, prodate = $idate, $prodate

if [[ $idate -le $prodate ]]; then

   ${scripts}/CkPlt_rgnl.sh nam $idate  1>/ptmpp1/Edward.Safford/logs/${RADMON_SUFFIX}/radmon/CkPlt_nam.log 2>/ptmpp1/Edward.Safford/logs/${RADMON_SUFFIX}/radmon/CkPlt_nam.err

   rc=`${scripts}/update_data_map.pl ${data_map} nam imgdate ${idate}`

  scp ${data_map} gyre:/gpfs/td2/emc/da/noscrub/Edward.Safford/${package}/parm/data_map.xml
fi


echo exiting run_plot_nam.sh

exit
