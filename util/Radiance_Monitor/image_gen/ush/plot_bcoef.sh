#! /bin/ksh

#------------------------------------------------------------------
#
#  plot_bcoef.sh
#
#------------------------------------------------------------------

set -ax
export list=$listvar



#------------------------------------------------------------------
# Set environment variables.
tmpdir=${PLOT_WORK_DIR}/plot_bcoef_${SUFFIX}.$PDATE
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

plot_bcoef=plot_bcoef.gs



#------------------------------------------------------------------
#   Set dates
bdate=`$NDATE -720 $PDATE`
edate=$PDATE
bdate0=`echo $bdate|cut -c1-8`
edate0=`echo $edate|cut -c1-8`


#--------------------------------------------------------------------
# Copy executable and control files to $tmpdir

imgdef=`echo ${#IMGNDIR}`
if [[ $imgdef -gt 0 ]]; then
  ctldir=$IMGNDIR/bcoef
else
  ctldir=$TANKDIR/bcoef
fi

echo ctldir = $ctldir



#--------------------------------------------------------------------
# Create plots and place on server (rzdm).

for type in ${SATYPE}; do

   $NCP $ctldir/${type}.ctl* ./
   uncompress ${type}.ctl.Z

   cdate=$bdate
   while [[ $cdate -le $edate ]]; do
      $NCP $TANKDIR/bcoef/${type}.${cdate}.ieee_d* ./
      adate=`$NDATE +6 $cdate`
      cdate=$adate
   done
   uncompress *.ieee_d.Z

   list="mean atmpath clw lapse2 lapse"
   for var in $list; do
cat << EOF > ${type}_${var}.gs
'open ${type}.ctl'
'run ${GSCRIPTS}/${plot_bcoef} ${type} ${var} x1100 y850'
'quit'
EOF
      timex $GRADS -bpc "run ${tmpdir}/${type}_${var}.gs"
   done 


   ssh -l ${WEB_USER} ${WEB_SVR} "mkdir -p ${WEBDIR}/bcoef"
   for var in $list; do
      scp ${type}.${var}*.png    ${WEB_USER}@${WEB_SVR}:${WEBDIR}/bcoef
   done

   for var in $list; do
      rm -f ${type}.${var}*.png
   done

   rm -f ${type}.ieee_d
   rm -f ${type}.ctl

done



#--------------------------------------------------------------------
# Clean $tmpdir.  Submit done job.

cd $tmpdir
cd ../
rm -rf $tmpdir


exit

