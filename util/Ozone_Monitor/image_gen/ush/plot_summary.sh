#! /bin/ksh

#------------------------------------------------------------------
#  plot_summary.sh
#

set -ax

SATYPE2=$1


#------------------------------------------------------------------
# Set work space for this SATYPE2 source.
#
tmpdir=${WORKDIR}/${SATYPE2}.$PDATE
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir


#------------------------------------------------------------------
#   Set dates
bdate=`$NDATE -168 $PDATE`
edate=$PDATE
bdate0=`echo $bdate|cut -c1-8`
edate0=`echo $edate|cut -c1-8`


#--------------------------------------------------------------------
# Copy control and data files to $tmpdir
#
tankdir_bdate0=${TANKDIR}/${RUN}.${bdate0}/oznmon/time
tankdir_edate0=${TANKDIR}/${RUN}.${edate0}/oznmon/time

for type in ${SATYPE2}; do
   cdate=$bdate

   while [[ $cdate -le $edate ]]; do
      cdate0=`echo $cdate|cut -c1-8`
      tankdir_cdate0=${TANKDIR}/${RUN}.${cdate0}/oznmon/time

      if [[ ! -e ${type}.ctl ]]; then
         $NCP ${tankdir_cdate0}/${type}.ctl ./
      fi

      $NCP ${tankdir_cdate0}/${type}.${cdate}.ieee_d ./
      adate=`$NDATE +6 $cdate`
      cdate=$adate
   done

   #----------------------------------------------------------------
   #  Modify tdef line in .ctl file to start at bdate.
   #
   if [[ -e ${type}.ctl ]]; then
      ${OZN_IG_SCRIPTS}/update_ctl_tdef.sh ${type}.ctl ${bdate} 29
   fi

cat << EOF > ${type}.gs
'open ${type}.ctl'
'run ${OZN_IG_GSCRPTS}/plot_summary.gs ${OZNMON_SUFFIX} ${RUN} ${type} x750 y700'
'quit'
EOF

    $GRADS -bpc "run ${tmpdir}/${type}.gs"

#   rm -f ${type}.ctl 
#   rm -f ${type}*.ieee_d
#   rm -f ${type}.summary.png

done

#--------------------------------------------------------------------
#  copy image files to TANKDIR
#
${NCP} *.png ${OZN_IMGN_TANKDIR}/.

#--------------------------------------------------------------------
# Clean $tmpdir. 
#cd $tmpdir
#cd ../
#rm -rf $tmpdir

exit

