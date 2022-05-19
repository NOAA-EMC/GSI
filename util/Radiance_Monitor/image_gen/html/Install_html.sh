#!/bin/sh

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

export RADMON_SUFFIX=$1
echo RADMON_SUFFIX = $RADMON_SUFFIX
export RAD_AREA=$2

this_file=`basename $0`
this_dir=`dirname $0`

top_parm=${this_dir}/../../parm

radmon_config=${radmon_config:-${top_parm}/RadMon_config}
if [[ ! -e ${radmon_config} ]]; then
   echo "Unable to source ${radmon_config}"
   exit 2
fi

. ${radmon_config}
if [[ $? -ne 0 ]]; then
   echo "Error detected while sourcing ${radmon_config} file"
   exit $?
fi


radmon_user_settings=${radmon_user_settings:-${top_parm}/RadMon_user_settings}
if [[ ! -e ${radmon_user_settings} ]]; then
   echo "Unable to locate ${radmon_user_settings} file"
   exit 4
fi

. ${radmon_user_settings}
if [[ $? -ne 0 ]]; then
   echo "Error detected while sourcing ${radmon_user_settings} file"
   exit $?
fi

#--------------------------------------------------------------
#  call the appropriate child script for glb or rgn
#
if [[ $RAD_AREA == "glb" ]]; then 
   ${RADMON_IMAGE_GEN}/html/install_glb.sh 
else 
   ${RADMON_IMAGE_GEN}/html/install_rgn.sh
fi


echo "END Install_html.sh"

exit
