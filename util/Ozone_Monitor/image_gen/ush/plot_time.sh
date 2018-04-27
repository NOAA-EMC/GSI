#! /bin/ksh

#------------------------------------------------------------------
#  plot_time.sh
#

set -ax

export SATYPE2=$1
export PVAR=$2
export PTYPE=$3

echo "SATYPE2, PVAR, PTYPE = $SATYPE2, $PVAR, $PTYPE"
echo "RUN = $RUN"

#------------------------------------------------------------------
# Set work space for this SATYPE2 source.
#
tmpdir=${WORKDIR}/${SATYPE2}.$PDATE.${PVAR}
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

#------------------------------------------------------------------
#   Set dates
bdate=`$NDATE -720 $PDATE`
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
      ${OZN_IG_SCRIPTS}/update_ctl_tdef.sh ${type}.ctl ${bdate} 121
   fi


   for var in ${PTYPE}; do
      echo $var

cat << EOF > ${type}_${var}.gs
'reinit'
'clear'
'open  ${type}.ctl'
'run ${OZN_IG_GSCRPTS}/plot_time_${string}.gs ${OZNMON_SUFFIX} ${RUN} ${type} ${var} x750 y700'
'quit'
EOF

      echo ${tmpdir}/${type}_${var}.gs

         $GRADS -bpc "run ${tmpdir}/${type}_${var}.gs"

         #----------------------------------------------
         #  rename the analysis plots
         #
#         if [[ $string == 'anl' ]] ; then
#            reglist='region1 region2 region3 region4 region5 region6'
#            frlist='fr1 fr2 fr3 fr4 fr5 fr6'
#            if [[ ${var} == 'count' ]] ; then
#               if [[ $type == 'omi_aura' || $type == 'gome_metop-a' || $type == 'gome_metop-b' ]] ; then
#                  for region in $reglist ; do
#                     mv ${type}.count_${region}_fr1.png ${type}.countanl_${region}_fr1.png
#                  done
#               else
#                  for region in $reglist ; do
#                     for fr in $frlist ; do
#                        mv ${type}.count_${region}_${fr}.png ${type}.countanl_${region}_${fr}.png
#                     done
#                  done
#               fi
#            elif [[ ${var} == 'omg' ]] ; then
#               if [[ $type == 'omi_aura' || $type == 'gome_metop-a' || $type == 'gome_metop-b' ]] ; then
#                  for region in $reglist ; do
#                     mv ${type}.omg_${region}_fr1.png ${type}.oma_${region}_fr1.png
#                  done
#               else
#                  for region in $reglist ; do
#                     for fr in $frlist ; do
#                        mv ${type}.omg_${region}_${fr}.png ${type}.oma_${region}_${fr}.png
#                     done
#                  done
#               fi
#            else
#               if [[ $type == 'omi_aura' || $type == 'gome_metop-a' || $type == 'gome_metop-b' ]] ; then
#                  for region in $reglist ; do
#                     mv ${type}.cpen_${region}_fr1.png ${type}.cpenanl_${region}_fr1.png
#                  done
#               else
#                  for region in $reglist ; do
#                     for fr in $frlist ; do
#                        mv ${type}.cpen_${region}_${fr}.png ${type}.cpenanl_${region}_${fr}.png
#                     done
#                  done
#               fi
#            fi
#         fi
      done 
#   fi
done

#--------------------------------------------------------------------
#  copy image files to TANKDIR
#
${NCP} *.png ${OZN_IMGN_TANKDIR}/.

#--------------------------------------------------------------------
# Clean $tmpdir.  Submit done job.
cd $tmpdir
cd ../
#rm -rf $tmpdir

exit

