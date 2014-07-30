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

top_parm=${this_dir}/../parm

if [[ -s ${top_parm}/conv_conf ]]; then
   . ${top_parm}/conv_conf
else
   echo "Unable to source ${top_parm}/conv_conf"
   exit
fi

if [[ ${IMGNDIR} != "/" ]]; then
   if [[ $MY_MACHINE = "wcoss" ]]; then
      /usr/bin/rsync -ave ssh --exclude *.ctl*  ${IMGNDIR} \
         ${WEBUSER}@${WEBSVR}:${WEBDIR}/
   fi
fi
echo end Transfer.sh
exit
