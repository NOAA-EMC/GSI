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
# Loop over satellite types.  Create plots and place on RZDM

for type in ${SATYPE2}; do

   icnt=0
   cdate=$bdate
   while [[ $cdate -le $edate ]] ; do
      test_file=$TANKDIR/angle/${type}.${cdate}.ieee_d
      if [[ -s $test_file ]]; then
         icnt=` expr $icnt + 1 `

         $NCP ${test_file} $tmpdir/${type}.${cdate}.ieee_d
      elif [[ -s ${test_file}.Z ]]; then
         icnt=` expr $icnt + 1 `
         $NCP ${test_file}.Z $tmpdir/${type}.${cdate}.ieee_d.Z
         uncompress $tmpdir/${type}.${cdate}.ieee_d.Z
      fi
      adate=`$NDATE +6 $cdate`
      cdate=$adate
   done
   nfile=$icnt

   nstep=$(grep "xdef" ${type}.ctl | cut -c6-9)
   nchan=$(grep "ydef" ${type}.ctl | cut -c5-9)
   nregion=$(grep "zdef" ${type}.ctl | cut -c5-7)

cat << EOF > input
 &INPUT
 satname='${type}',
 nfile=${nfile},
 nregion=${nregion},
 n_chan=${nchan},
 nstep=${nstep},
 /
EOF


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

#--------------------------------------------------------------------
# Copy image files to server (rzdm).  Delete images and data files.

   ssh -l ${WEB_USER} ${WEB_SVR} "mkdir -p ${WEBDIR}/angle"
   for var in ${PTYPE}; do
      scp ${type}.${var}*.png    ${WEB_USER}@${WEB_SVR}:${WEBDIR}/angle
   done

   for var in ${PTYPE}; do
      rm -f ${type}.${var}*.png
   done

   rm -f ${type}*.ieee_d
   rm -f ${type}.ctl

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

if [[ $running -eq 1 ]]; then
   cd ${PLOT_WORK_DIR}
   cd ../
   rm -rf ${PLOT_WORK_DIR}
fi

exit
