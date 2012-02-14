#!/bin/ksh

#------------------------------------------------------------------
#
# mk_bcoef_plots.sh
#
# submit the plot jobs to make the bcoef images.
#
#------------------------------------------------------------------

set -ax
date
export list=$listvar

imgndir="${IMGNDIR}/bcoef"
tankdir="${TANKDIR}/bcoef"

if [[ ! -d ${imgndir} ]]; then
   mkdir -p ${imgndir}
fi

#-------------------------------------------------------------------
#  Locate/update the control files.  If no ctl file is available
#  report a warning to the log file.  If there is a ctl file in
#  $tankdir then copy it to $imgndir.
#
allmissing=1
for type in ${SATYPE}; do

   # warn if no ctl file(s) available at all
   if [[ ! -s ${imgndir}/${type}.ctl.Z && ! -s ${imgndir}/${type}.ctl &&
         ! -s ${tankdir}/${type}.ctl.Z && ! -s ${tankdir}/${type}.ctl ]]; then
      echo WARNING:  unable to locate ${type}.ctl
   fi

   if [[ -s ${tankdir}/${type}.ctl.Z || -s ${tankdir}/${type}.ctl  ]]; then
      $NCP ${tankdir}/${type}.ctl* ${imgndir}/.
      allmissing=0
   fi

   if [[ -s ${imgndir}/${type}.ctl.Z || -s ${imgndir}/${type}.ctl ]]; then
      allmissing=0
   fi
done

if [[ $allmissing = 1 ]]; then
   echo ERROR:  Unable to plot.  All control files are missing.
   exit
fi


#-------------------------------------------------------------------
#   Update the time definition (tdef) line in the angle control
#   files.
#
#   Note that the logic for the tdef in time series is backwards
#   from angle series.  Time tdefs start at -720 from PDATE.  For
#   angle series the tdef = $PDATE and the script works backwards.
#   Some consistency on this point would be great.

start_date=`$NDATE -720 $PDATE`

for type in ${SATYPE}; do
   if [[ -s ${imgndir}/${type}.ctl.Z ]]; then
     uncompress ${imgndir}/${type}.ctl.Z
   fi
   ${SCRIPTS}/update_ctl_tdef.sh ${imgndir}/${type}.ctl ${start_date}
   compress ${imgndir}/${type}.ctl
done


#-------------------------------------------------------------------
# submit plot job
#

rm $LOGDIR/plot_bcoef.log
$SUB -a $ACOUNT -e $listvar -j plot_${SUFFIX}_bcoef -u $USER -q dev  -g ${USER_CLASS} -t 1:00:00 -o $LOGDIR/plot_bcoef.log $SCRIPTS/plot_bcoef.sh


exit
