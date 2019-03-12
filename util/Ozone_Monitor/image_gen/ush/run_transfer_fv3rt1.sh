#!/bin/sh

package=ProdGSI/util/Ozone_Monitor
#package=OznMon

ozn_suffix=fv3rt1
run=gdas

ch=`hostname  | cut -c1`

export WEB_DIR=/home/people/emc/www/htdocs/gmb/gdas/es_ozn/pngs

scripts=/gpfs/${ch}d2/emc/da/noscrub/Edward.Safford/${package}/image_gen/ush

${scripts}/OznMon_Transfer.sh ${ozn_suffix} --run ${run}

exit
