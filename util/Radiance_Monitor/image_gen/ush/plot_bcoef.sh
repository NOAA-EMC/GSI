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

tmpdir=${PLOT_WORK_DIR}/plot_bcoef_${RADMON_SUFFIX}.$PDATE
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

plot_bcoef=plot_bcoef.gs



#------------------------------------------------------------------
#   Set dates
bdate=${START_DATE}
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
# Loop over satellite types.  Copy data files, create plots and
# place on the web server.
#
# Data file location may either be in angle, bcoef, bcor, and time
# subdirectories under $TANKDIR, or in the Operational organization
# of radmon.YYYYMMDD directories under $TANKDIR.


$NCP ${IG_SCRIPTS}/nu_plot_bcoef.sh .

for type in ${SATYPE}; do

   $NCP $ctldir/${type}.ctl* ./
   ${UNCOMPRESS} ${type}.ctl.${Z}

   cdate=$bdate
   while [[ $cdate -le $edate ]]; do
      if [[ $REGIONAL_RR -eq 1 ]]; then
         tdate=`$NDATE +6 $cdate`
         day=`echo $tdate | cut -c1-8 `
         hh=`echo $cdate | cut -c9-10`
         . ${IG_SCRIPTS}/rr_set_tz.sh $hh
      else
         day=`echo $cdate | cut -c1-8 `
      fi

      if [[ -d ${TANKDIR}/radmon.${day} ]]; then
         
         if [[ $REGIONAL_RR -eq 1 ]]; then
#            test_file=`ls ${TANKDIR}/radmon.${day}/*bcoef.${type}.${cdate}.ieee_d*`
            test_file=${TANKDIR}/radmon.${day}/${rgnHH}.bcoef.${type}.${cdate}.ieee_d.${rgnTM}
#            test_file_anl=`ls ${TANKDIR}/radmon.${day}/*bcoef.${type}_anl.${cdate}.ieee_d*`
         else
            test_file=${TANKDIR}/radmon.${day}/bcoef.${type}.${cdate}.ieee_d
#            test_file_anl=${TANKDIR}/radmon.${day}/bcoef.${type}_anl.${cdate}.ieee_d
         fi

         if [[ $USE_ANL = 1 ]]; then
            if [[ $REGIONAL_RR -eq 1 ]]; then
               test_file=${TANKDIR}/radmon.${day}/${rgnHH}.bcoef.${type}_anl.${cdate}.ieee_d.${rgnTM}
            else
               test_file2=${TANKDIR}/radmon.${day}/bcoef.${type}_anl.${cdate}.ieee_d
            fi
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
#         $NCP $TANKDIR/bcoef/${type}.${cdate}.ieee_d* ./
#      fi

#      if [[ $REGIONAL_RR -eq 1 ]]; then
#         adate=`$NDATE +1 $cdate`
#      else
#         adate=`$NDATE +6 $cdate`
#      fi

      adate=`$NDATE +${CYCLE_INTERVAL} $cdate`
      cdate=$adate

   done
   ${UNCOMPRESS} *.ieee_d.${Z}

   if [[ $PLOT_STATIC_IMGS -eq 1 ]]; then
      list="mean atmpath clw lapse2 lapse cos_ssmis sin_ssmis emiss ordang4 ordang3 ordang2 ordang1"
      for var in $list; do

cat << EOF > ${type}_${var}.gs
'open ${type}.ctl'
'run ${IG_GSCRIPTS}/${plot_bcoef} ${type} ${var} x1100 y850'
'quit'
EOF
         $GRADS -bpc "run ${tmpdir}/${type}_${var}.gs"


      done 
      if [[ ! -d ${IMGNDIR}/bcoef ]]; then
         mkdir -p ${IMGNDIR}/bcoef
      fi
      cp -f *.png  ${IMGNDIR}/bcoef
   fi


   #--------------------------------------------------------------------------
   #  nu_plot_bcoef.sh produces the data files needed by the js/html files to
   #  generate the interactive charts
   ./nu_plot_bcoef.sh ${type}



#   rm -f ${type}.ieee_d
#   rm -f ${type}.ctl

done

rm -f nu_plot_time.sh


#for var in $list; do
#   rm -f ${type}.${var}*.png
#done

#--------------------------------------------------------------------
# Clean $tmpdir.  Submit done job.

#cd $tmpdir
#cd ../
#rm -rf $tmpdir


exit

