#! /bin/ksh

#------------------------------------------------------------------
#
#  plot_summary.sh
#
#------------------------------------------------------------------

set -ax
export list=$listvars


#------------------------------------------------------------------
# Set environment variables.
tmpdir=${PLOT_WORK_DIR}
cd $tmpdir

#------------------------------------------------------------------
#   Set dates

bdate=${SDATE}
edate=${EDATE}
bdate0=`echo $bdate|cut -c1-8`
edate0=`echo $edate|cut -c1-8`

#--------------------------------------------------------------------
# Copy and rename data (*.ieee_d) files for both SUFFIX1 and SUFFIX2

for type in ${SATYPE}; do

   cdate=$bdate

   while [[ $cdate -le $edate ]]; do
      day=`echo $cdate | cut -c1-8 `

      if [[ -d ${TANKDIR1}/radmon.${day} ]]; then
         test_file=${TANKDIR1}/radmon.${day}/time.${type}.${cdate}.ieee_d
         if [[ -s $test_file ]]; then
            $NCP ${test_file} ./${SUFFIX1}.${type}.${cdate}.ieee_d
         elif [[ -s ${test_file}.${Z} ]]; then
            $NCP ${test_file}.${Z} ./${SUFFIX1}.${type}.${cdate}.ieee_d.${Z}
         fi
      fi

      if [[ -d ${TANKDIR2}/radmon.${day} ]]; then
         test_file=${TANKDIR2}/radmon.${day}/time.${type}.${cdate}.ieee_d
         if [[ -s $test_file ]]; then
            $NCP ${test_file} ./${SUFFIX2}.${type}.${cdate}.ieee_d
         elif [[ -s ${test_file}.${Z} ]]; then
            $NCP ${test_file}.${Z} ./${SUFFIX2}.${type}.${cdate}.ieee_d.${Z}
         fi
      fi

      adate=`$NDATE +6 $cdate`
      cdate=$adate
   done
   ${UNCOMPRESS} *.ieee_d.${Z}

cat << EOF > ${type}.gs
'open ${SUFFIX1}.${type}.ctl'
'open ${SUFFIX2}.${type}.ctl'
'run ${IG_GSCRIPTS}/plot_comp_summary.gs ${type} ${SUFFIX1} ${SUFFIX2} x1100 y850'
'quit'
EOF

   $GRADS -bpc "run ${tmpdir}/${type}.gs"

#   rm -f ${type}.ctl 
#   rm -f ${type}*.ieee_d

done

#--------------------------------------------------------------------
# Copy image files to $IMGNDIR to set up for mirror to web server.
# Delete images and data files.

if [[ ! -d ${IMGNDIR1}/pngs/comp ]]; then
   mkdir -p ${IMGNDIR1}/pngs/comp
fi
$NCP -r *comp.png ${IMGNDIR1}/pngs/comp/.

#rm -f *.comp.png


#--------------------------------------------------------------------
# Clean $tmpdir. 
#
#cd $tmpdir
#cd ../
#rm -rf $tmpdir


exit

