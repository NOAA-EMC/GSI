#! /bin/ksh

#------------------------------------------------------------------
#
#  plot_time.sh
#
#------------------------------------------------------------------

set -ax
export list=$listvars

SATYPE2=$1
PVAR=$2
PTYPE=$3

echo "Starting plot_time.sh"

#------------------------------------------------------------------
# Set environment variables.
tmpdir=${PLOT_WORK_DIR}/plot_time_${SUFFIX}_${SATYPE2}.$PDATE.${PVAR}
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

plot_time_count=plot_time_count.${RAD_AREA}.gs
echo plot_time_count = $plot_time_count

plot_time_sep=plot_time_sep.gs
echo plot_time_sep = $plot_time_sep


echo PLOT_WORK_DIR = $PLOT_WORK_DIR

#------------------------------------------------------------------
#   Set dates
bdate=`$NDATE -720 $PDATE`
rdate=`$NDATE -72 $PDATE`
edate=$PDATE
bdate0=`echo $bdate|cut -c1-8`
edate0=`echo $edate|cut -c1-8`

#--------------------------------------------------------------------
# Set ctldir to point to correct control file source

imgdef=`echo ${#IMGNDIR}`
if [[ $imgdef -gt 0 ]]; then
  ctldir=$IMGNDIR/time
else
  ctldir=$TANKDIR/time
fi

echo ctldir = $ctldir


#--------------------------------------------------------------------
# Create plots and place on RZDM

for type in ${SATYPE2}; do
   $NCP $ctldir/${type}*.ctl* ./
   if [[ -s ./${type}.ctl.Z ]]; then
      echo uncompressing ${type}.ctl.Z
      uncompress ./${type}.ctl.Z
   else
      echo nothing to uncompress
   fi

   cdate=$bdate
   while [[ $cdate -le $edate ]]; do
      $NCP $TANKDIR/time/${type}*${cdate}.ieee_d* ./
      if [[ -s ./${type}.${cdate}.ieee_d.Z ]]; then
        uncompress ./${type}.${cdate}.ieee_d.Z
      fi
      adate=`$NDATE +6 $cdate`
      cdate=$adate
   done

     for var in ${PTYPE}; do
     echo $var
      if [ "$var" =  'count' ]; then 
cat << EOF > ${type}_${var}.gs
'open ${type}.ctl'
'run ${GSCRIPTS}/${plot_time_count} ${type} ${var} x1100 y850'
'quit'
EOF
elif [ "$var" =  'penalty' ]; then
cat << EOF > ${type}_${var}.gs
'open ${type}.ctl'
'run ${GSCRIPTS}/${plot_time_count} ${type} ${var} x1100 y850'
'quit'
EOF
else
cat << EOF > ${type}_${var}.gs
'open ${type}.ctl'
'run ${GSCRIPTS}/${plot_time_sep} ${type} ${var} x1100 y850'
'quit'
EOF
fi
echo ${tmpdir}/${type}_${var}.gs
      timex $GRADS -bpc "run ${tmpdir}/${type}_${var}.gs"
   done


   ssh -l ${WEB_USER} ${WEB_SVR} "mkdir -p ${WEBDIR}/time"
   for var in ${PTYPE}; do
      scp ${type}.${var}*.png   ${WEB_USER}@${WEB_SVR}:${WEBDIR}/time/
   done


   for var in ${PTYPE}; do
      rm -f ${type}.${var}*.png
   done

   rm -f ${type}.ieee_d
   rm -f ${type}.${PDATE}.ieee_d
   rm -f ${type}.ctl

done



#--------------------------------------------------------------------
# Clean $tmpdir.
cd $tmpdir
cd ../
rm -rf $tmpdir

#--------------------------------------------------------------------
# If this is the last time/summary plot job to finish then rm PLOT_WORK_DIR.
#

#count=`ls ${LOADLQ}/*plot*_${SUFFIX}*time* | wc -l`
#complete=`grep "COMPLETED" ${LOADLQ}/*plot*_${SUFFIX}*time* | wc -l`

#running=`expr $count - $complete`

#if [[ $running -eq 1 ]]; then
#   cd ${PLOT_WORK_DIR}
#   cd ../
#   rm -rf ${PLOT_WORK_DIR}
#fi

echo "Exiting plot_time.sh"
exit

