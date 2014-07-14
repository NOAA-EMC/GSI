#! /bin/ksh

#------------------------------------------------------------------
#
#  plot_summary.sh
#
#------------------------------------------------------------------

set -ax
export list=$listvars

echo Start plot_summary.sh
echo USE_ANL = $USE_ANL

#SATYPE2=$1
SATYPE2=$SATYPE

#------------------------------------------------------------------
# Set environment variables.
#tmpdir=${STMP_USER}/plot_summary_${SUFFIX}.$PDATE
tmpdir=${PLOT_WORK_DIR}/../plot_summary_${SUFFIX}
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir



#------------------------------------------------------------------
#   Set dates

bdate=${START_DATE}
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
# Create plots and place on server (rzdm)

for type in ${SATYPE2}; do

   $NCP $ctldir/${type}.ctl* ./
   ${UNCOMPRESS} *.ctl.${Z}

   cdate=$bdate

   while [[ $cdate -le $edate ]]; do
      day=`echo $cdate | cut -c1-8 `

      if [[ -d ${TANKDIR}/radmon.${day} ]]; then
         test_file=${TANKDIR}/radmon.${day}/time.${type}.${cdate}.ieee_d
         if [[ $USE_ANL = 1 ]]; then
            test_file2=${TANKDIR}/radmon.${day}/time.${type}_anl.${cdate}.ieee_d
         else
            test_file2=
         fi

         if [[ -s $test_file ]]; then
            $NCP ${test_file} ./${type}.${cdate}.ieee_d
         elif [[ -s ${test_file}.${Z} ]]; then
            $NCP ${test_file}.${Z} ./${type}.${cdate}.ieee_d.${Z}
         fi

         if [[ -s $test_file2 ]]; then
            $NCP ${test_file2} ./${type}_anl.${cdate}.ieee_d
         elif [[ -s ${test_file2}.${Z} ]]; then
            $NCP ${test_file2}.${Z} ./${type}_anl.${cdate}.ieee_d.${Z}
         fi
      fi

#      if [[ ! -s ${type}.${cdate}.ieee_d && ! -s ${type}.${cdate}.ieee_d.${Z} ]]; then
#         $NCP $TANKDIR/time/${type}*${cdate}.ieee_d* ./
#      fi
      adate=`$NDATE +6 $cdate`
      cdate=$adate
   done
   ${UNCOMPRESS} *.ieee_d.${Z}

   outfile=${tmpdir}/${type}.gs
   rm -f ${outfile}

cat << EOF > ${outfile}
'open ${type}.ctl'
'run ${IG_GSCRIPTS}/plot_summary.gs ${type} ${PLOT_SUB_AVGS} ${USE_ANL} x1100 y850'
'quit'
EOF

   $GRADS -bpc "run ${outfile}"

#   rm -f ${type}.ctl 
#   rm -f ${type}*.ieee_d

done

#--------------------------------------------------------------------
# Copy image files to $IMGNDIR to set up for mirror to web server.
# Delete images and data files.

if [[ ! -d ${IMGNDIR}/summary ]]; then
   mkdir -p ${IMGNDIR}/summary
fi
$NCP -f *summary.png ${IMGNDIR}/summary/.

#rm -f *.summary.png


#--------------------------------------------------------------------
# Clean $tmpdir. 
#
#cd $tmpdir
#cd ../
#rm -rf $tmpdir

echo End plot_summary.sh

exit

