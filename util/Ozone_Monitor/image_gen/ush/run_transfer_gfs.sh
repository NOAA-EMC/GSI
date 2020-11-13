#!/bin/sh

package=ProdGSI/util/Ozone_Monitor
#package=OznMon

ozn_suffix=GFS
run=gdas

ch=`hostname  | cut -c1`

#export WEB_DIR=/home/people/emc/www/htdocs/gmb/gdas/es_ozn/pngs

scripts=/gpfs/dell2/emc/modeling/noscrub/Edward.Safford/${package}/image_gen/ush

${scripts}/OznMon_Transfer.sh ${ozn_suffix} --run ${run}

exit
