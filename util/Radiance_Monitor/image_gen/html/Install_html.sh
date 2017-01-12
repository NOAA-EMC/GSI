#!/bin/sh
set -ax

#--------------------------------------------------------------------
#--------------------------------------------------------------------
#  Install_html.sh
#
#  Given a suffix and a global/regional flag as inputs, build the
#  html necessary for a radiance monitor web site and tranfer it to
#  the server.
#--------------------------------------------------------------------
#--------------------------------------------------------------------

function usage {
  echo "Usage:  Install_html.sh suffix area"
  echo "            Suffix is data source identifier that matches data in "
  echo "              the $TANKDIR/stats directory."
  echo "            area is either 'glb' or 'rgn' (global or regional)"
}

echo "BEGIN Install_html.sh"
echo ""

nargs=$#
if [[ $nargs -ne 2 ]]; then
   usage
   exit 2
fi

RADMON_SUFFIX=$1
echo RADMON_SUFFIX = $RADMON_SUFFIX
export RAD_AREA=$2

this_file=`basename $0`
this_dir=`dirname $0`

top_parm=${this_dir}/../../parm

if [[ -s ${top_parm}/RadMon_config ]]; then
   . ${top_parm}/RadMon_config
else
   echo "ERROR:  Unable to source ${top_parm}/RadMon_config"
   exit
fi

if [[ -s ${top_parm}/RadMon_user_settings ]]; then
   . ${top_parm}/RadMon_user_settings
else
   echo "ERROR:  Unable to source ${top_parm}/RadMon_user_settings"
   exit
fi


#--------------------------------------------------------------
#  source plot_rad_conf to get WEB_SVR, WEB_USER, WEBDIR
#
. ${RADMON_IMAGE_GEN}/parm/plot_rad_conf


#--------------------------------------------------------------
#  call the appropriate child script for glb or rgn
#
if [[ $RAD_AREA == "glb" ]]; then 
   ${RADMON_IMAGE_GEN}/html/install_glb.sh $RADMON_SUFFIX 
else 
   ${RADMON_IMAGE_GEN}/html/install_rgn.sh $RADMON_SUFFIX 
fi


echo "END Install_html.sh"

exit
