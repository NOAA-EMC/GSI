#! /bin/ksh

#------------------------------------------------------------------
#
#  plot_summary.sh
#
#------------------------------------------------------------------

set -ax
export list=$listvars
SATYPE2=$SATYPE

echo "Start plot_summary.sh"


#------------------------------------------------------------------
# Set environment variables.
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


#--------------------------------------------------------------------
# Assemble the required data files (*.ieee_d), make txt files, and 
# conditionally create static images (see below), and move all 
# resulting files to the $TANKDIR/imgn directory for transfer to 
# server.
#
for type in ${SATYPE2}; do

   $NCP $ctldir/${type}.ctl* ./
   ${UNCOMPRESS} *.ctl.${Z}

   cdate=$bdate

   #-------------------------------------
   #  Locate and copy data files.
   #
   while [[ $cdate -le $edate ]]; do
      day=`echo $cdate | cut -c1-8`

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

      adate=`$NDATE +6 $cdate`
      cdate=$adate
   done
   ${UNCOMPRESS} *.ieee_d.${Z}

   
#--------------------------------------------------------------------
#  Plotting is moving towards dynamic, interactive images drawn
#  on-the-fly in the client browser.  These images require small text
#  files instead of static images produced by GrADS.  The flag
#  PLOT_STATIC_IMGS controls the conditional plotting of the older
#  static images.  
#
#  At present this only affects the summary plots, but will eventually
#  include most radiance images.
#
   if [[ $PLOT_STATIC_IMGS -eq 1 ]]; then

      outfile=${tmpdir}/${type}.gs
      rm -f ${outfile}

cat << EOF > ${outfile}
'open ${type}.ctl'
'run ${IG_GSCRIPTS}/plot_summary.gs ${type} ${PLOT_SUB_AVGS} ${USE_ANL} x1100 y850'
'quit'
EOF

      $GRADS -bpc "run ${outfile}"
   fi


#--------------------------------------------------------------------
#  Build new data file for server.
#  This supports the javascript summary plot.
#
#  Algorithm:
#    1. Copy summary.x locally
#    2. For each entry in SATYTPE
#       2) build the times.txt file from the data files
#       3) build the use.txt file from the ctl file
#       1) build the input file (namelist for summary.x)
#       4) run the summary.x executable
#       5) copy the [satype].sum.txt file to $TANKDIR/imgn/{suffix}/pngs/summary/.
#       6) clean up

   echo "BEGIN javascript file generation:"
   
   if [[ ! -s summary.x ]]; then
      $NCP ${IG_EXEC}/summary.x .
   fi

   ls ${type}.*.ieee_d | cut -d'.' -f2 > tmp
   tac tmp > times.txt
   rm tmp

   cat ./${type}.ctl | grep iuse | gawk '{print $8}' > use.txt

   nchanl=`cat ./${type}.ctl | grep title |gawk '{print $4}'`
   ncycle=`cat times.txt | wc -l`
   st_time=`head -1 times.txt`
   input=${type}.input.txt

   nregion=5
   if [[ ${RAD_AREA} == "rgn" ]]; then
     nregion=1
   fi
 
   cat <<EOF > ${input}
     &INPUT
      satname=${type},
      nchanl=${nchanl},
      ncycle=${ncycle},
      nregion=${nregion},
      st_time=${st_time},
     /
EOF

   rm -f input
   cp ${input} input
   ./summary.x < input > out.${type}
   echo "END javascript file generation:"
   rm -f times.txt
   rm -f use.txt

   rm -f ${input}

#   rm -f ${type}.ctl 
#   rm -f ${type}*.ieee_d

done

#--------------------------------------------------------------------
# Copy image files to $IMGNDIR to set up for mirror to web server.
# Delete images and data files.

if [[ ! -d ${IMGNDIR}/summary ]]; then
   mkdir -p ${IMGNDIR}/summary
fi
$NCP *summary.png ${IMGNDIR}/summary/.

if [[ $SUFFIX = "4devb" || $SUFFIX = "pr4dev" || $SUFFIX = "wopr" ]]; then
   for type in ${SATYPE2}; do
      $NCP ${type}.sum.txt ${IMGNDIR}/summary/${type}.${PDATE}.sum.txt
   done
else
  $NCP *.sum.txt ${IMGNDIR}/summary/.
fi

#rm -f *.summary.png


#--------------------------------------------------------------------
# Clean $tmpdir. 
#
#cd $tmpdir
#cd ../
#rm -rf $tmpdir

echo "End plot_summary.sh"

exit

