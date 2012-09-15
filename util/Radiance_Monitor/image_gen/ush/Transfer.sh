#!/bin/sh

function usage {
  echo "Usage:  Transfer.sh suffix"
  echo "            File name for Transfer.sh may be full or relative path"
  echo "            Suffix is data source identifier that matches data in "
  echo "              the $TANKDIR/stats directory."
}

set -ax
echo start Transfer.sh

nargs=$#
if [[ $nargs -ne 1 ]]; then
   usage
   exit 1
fi

SUFFIX=$1
this_file=`basename $0`
this_dir=`dirname $0`

top_parm=${this_dir}/../../parm

if [[ -s ${top_parm}/RadMon_config ]]; then
   . ${top_parm}/RadMon_config
else
   echo "Unable to source ${top_parm}/RadMon_config"
   exit
fi

. ${RADMON_IMAGE_GEN}/parm/plot_rad_conf


#--------------------------------------------------------------------
# Get the area (glb/rgn) for this suffix
#--------------------------------------------------------------------
area=`${SCRIPTS}/query_data_map.pl ${DATA_MAP} ${SUFFIX} area`

if [[ $area == "glb" ]]; then
  . ${RADMON_IMAGE_GEN}/parm/glbl_conf
elif [[ $area == "rgn" ]]; then
  . ${RADMON_IMAGE_GEN}/parm/rgnl_conf
fi

log_file=${LOGSverf_rad}/Transfer_${SUFFIX}.log
err_file=${LOGSverf_rad}/Transfer_${SUFFIX}.err


/usrx/local/bin/rsync -ave ssh --exclude *.ctl*  ${IMGNDIR}/ \
   esafford@rzdm.ncep.noaa.gov:${WEBDIR}/

echo end Transfer.sh
exit
