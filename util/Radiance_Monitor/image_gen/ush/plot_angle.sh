#! /bin/ksh

#------------------------------------------------------------------
#
#  plot_angle.sh
#
#------------------------------------------------------------------

set -ax
export list=$listvars

SATYPE2=$1
PVAR=$2
PTYPE=$3

plot_angle_count=plot_angle_count.${RAD_AREA}.gs
plot_angle_sep=plot_angle_sep.${RAD_AREA}.gs


#------------------------------------------------------------------
# Set environment variables.

if [[ "$SATYPE2" = 'airs_aqua' || "$SATYPE2" = 'iasi_metop-a' ]]; then
   tmpdir=${PLOT_WORK_DIR}/plot_angle_${SUFFIX}_${SATYPE2}.$PDATE.${PVAR}.${PTYPE}
else
   tmpdir=${PLOT_WORK_DIR}/plot_angle_${SUFFIX}_${SATYPE2}.$PDATE.${PVAR}
fi
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir



#------------------------------------------------------------------
#   Set dates

bdate=`$NDATE -720 $PDATE`
rdate=`$NDATE -72 $PDATE`
edate=$PDATE
bdate0=`echo $bdate|cut -c1-8`
edate0=`echo $edate|cut -c1-8`



#--------------------------------------------------------------------
# Copy control files to $tmpdir

imgdef=`echo ${#IMGNDIR}`
if [[ $imgdef -gt 0 ]]; then
  ctldir=$IMGNDIR/angle
else
  ctldir=$TANKDIR/angle
fi

echo ctldir = $ctldir

for type in ${SATYPE2}; do
  $NCP $ctldir/${type}.ctl* ./
done
uncompress *.ctl.Z


#--------------------------------------------------------------------
# Loop over satellite types.  Copy data files, create plots and 
# place on the web server. 
#
# Data file location may either be in angle, bcoef, bcor, and time 
# subdirectories under $TANKDIR, or in the Operational organization
# of radmon.YYYYMMDD directories under $TANKDIR. 

for type in ${SATYPE2}; do

   cdate=$bdate
   while [[ $cdate -le $edate ]] ; do
      day=`echo $cdate | cut -c1-8 `

      if [[ -d ${TANKDIR}/radmon.${day} ]]; then
         test_file=${TANKDIR}/radmon.${day}/angle.${type}.${cdate}.ieee_d
         if [[ -s $test_file ]]; then
            $NCP ${test_file} ./${type}.${cdate}.ieee_d
         elif [[ -s ${test_file}.Z ]]; then
            $NCP ${test_file}.Z ./${type}.${cdate}.ieee_d.Z
         fi
      fi
      if [[ ! -s ${type}.${cdate}.ieee_d && ! -s ${type}.${cdate}.ieee_d.Z ]]; then
         $NCP $TANKDIR/angle/${type}.${cdate}.ieee_d* ./
      fi
     
      adate=`$NDATE +6 $cdate`
      cdate=$adate

   done
   uncompress $tmpdir/*.ieee_d.Z

   for var in ${PTYPE}; do
      echo $var
      if [ "$var" =  'count' ]; then

cat << EOF > ${type}_${var}.gs
'open ${type}.ctl'
'run ${GSCRIPTS}/${plot_angle_count} ${type} ${var} x1100 y850'
'quit'
EOF

      elif [ "$var" =  'penalty' ]; then

cat << EOF > ${type}_${var}.gs
'open ${type}.ctl'
'run ${GSCRIPTS}/${plot_angle_count} ${type} ${var} x1100 y850'
'quit'
EOF

      else

cat << EOF > ${type}_${var}.gs
'open ${type}.ctl'
'run ${GSCRIPTS}/${plot_angle_sep} ${type} ${var} x1100 y850'
'quit'
EOF
      fi

      timex $GRADS -bpc "run ${tmpdir}/${type}_${var}.gs"
   done 

   rm -f ${type}*.ieee_d
   rm -f ${type}.ctl

done

#--------------------------------------------------------------------
# Copy image files to server (rzdm).  Delete images and data files.

#ssh -l ${WEB_USER} ${WEB_SVR} "mkdir -p ${WEBDIR}/angle"

#for var in ${PTYPE}; do
#   scp ${type}.${var}*.png    ${WEB_USER}@${WEB_SVR}:${WEBDIR}/angle
#done
scp *.png    ${WEB_USER}@${WEB_SVR}:${WEBDIR}/angle

for var in ${PTYPE}; do
   rm -f ${type}.${var}*.png
done



#--------------------------------------------------------------------
# Clean $tmpdir. 

cd $tmpdir
cd ../
rm -rf $tmpdir


#--------------------------------------------------------------------
# If this is the last angle plot job to finish then rm PLOT_WORK_DIR.
# 
echo ${LOADLQ}

count=`ls ${LOADLQ}/*plot*_${SUFFIX}* | wc -l`
complete=`grep "COMPLETED" ${LOADLQ}/*plot*_${SUFFIX}* | wc -l`

running=`expr $count - $complete`

#if [[ $running -eq 1 ]]; then
#   cd ${PLOT_WORK_DIR}
#   cd ../
#   rm -rf ${PLOT_WORK_DIR}
#fi

exit
