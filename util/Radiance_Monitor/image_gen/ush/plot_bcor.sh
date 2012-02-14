#! /bin/ksh

#------------------------------------------------------------------
#
#  plot_bcor.sh
#
#------------------------------------------------------------------

set -ax
export list=$listvars

SATYPE2=$1
PVAR=$2
PTYPE=$3


#------------------------------------------------------------------
# Set environment variables.

tmpdir=${PLOT_WORK_DIR}/plot_bcor_${SUFFIX}_${SATYPE2}.$PDATE.${PVAR}
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

plot_bcor_count=plot_bcor_count.${RAD_AREA}.gs
plot_bcor_sep=plot_bcor_sep.${RAD_AREA}.gs


#------------------------------------------------------------------
#   Set dates

bdate=`$NDATE -720 $PDATE`
edate=$PDATE
bdate0=`echo $bdate|cut -c1-8`
edate0=`echo $edate|cut -c1-8`


#--------------------------------------------------------------------
# Set ctldir to point to control file directory

imgdef=`echo ${#IMGNDIR}`
if [[ $imgdef -gt 0 ]]; then
  ctldir=$IMGNDIR/bcor
else
  ctldir=$TANKDIR/bcor
fi

echo ctldir = $ctldir


#--------------------------------------------------------------------
# Create plots and place on RZDM

for type in ${SATYPE2}; do

   $NCP $ctldir/${type}.ctl* ./
   uncompress *.ctl.Z

   cdate=$bdate
   while [[ $cdate -le $edate ]]; do
     $NCP $TANKDIR/bcor/${type}.${cdate}.ieee_d* ./
     adate=`$NDATE +6 $cdate`
     cdate=$adate
   done
   uncompress *.ieee_d.Z

   for var in ${PTYPE}; do
      echo $var
      if [ "$var" =  'count' ]; then
cat << EOF > ${type}_${var}.gs
'open ${type}.ctl'
'run ${GSCRIPTS}/${plot_bcor_count} ${type} ${var} x1100 y850'
'quit'
EOF
      else
cat << EOF > ${type}_${var}.gs
'open ${type}.ctl'
'run ${GSCRIPTS}/${plot_bcor_sep} ${type} ${var} x1100 y850'
'quit'
EOF
      fi

      echo ${tmpdir}/${type}_${var}.gs
      timex $GRADS -bpc "run ${tmpdir}/${type}_${var}.gs"
   done 


#--------------------------------------------------------------------
# Copy images to server (rzdm)

   ssh -l ${WEB_USER} ${WEB_SVR} "mkdir -p ${WEBDIR}/bcor"
   for var in ${PTYPE}; do
      scp ${type}.${var}*.png  ${WEB_USER}@${WEB_SVR}:${WEBDIR}/bcor
   done


#--------------------------------------------------------------------
# Delete images and data files

   for var in ${PTYPE}; do
      rm -f ${type}.${var}*.png
   done
   rm -f ${type}.ieee_d
   rm -f ${type}.ctl

done


#--------------------------------------------------------------------
# Clean $tmpdir  

cd $tmpdir
cd ../
rm -rf $tmpdir


#--------------------------------------------------------------------
# If this is the last bcor plot job to finish then rm PLOT_WORK_DIR.
#

count=`ls ${LOADLQ}/*plot*_${SUFFIX}* | wc -l`
complete=`grep "COMPLETED" ${LOADLQ}/*plot*_${SUFFIX}* | wc -l`

running=`expr $count - $complete`

if [[ $running -eq 1 ]]; then
   cd ${PLOT_WORK_DIR}
   cd ../
   rm -rf ${PLOT_WORK_DIR}
fi

exit
