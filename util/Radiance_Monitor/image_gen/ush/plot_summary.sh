#! /bin/bash

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
tmpdir=${PLOT_WORK_DIR}/plot_summary_${RADMON_SUFFIX}
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir


#------------------------------------------------------------------
#   Set dates
#
bdate=${START_DATE}
edate=$PDATE
bdate0=`echo $bdate|cut -c1-8`
edate0=`echo $edate|cut -c1-8`

#--------------------------------------------------------------------
# Set ctldir to point to correct control file source
#
imgdef=`echo ${#IMGNDIR}`
if [[ $imgdef -gt 0 ]]; then
  ctldir=$IMGNDIR/time
else
  ctldir=$TANKDIR/time
fi


usef="use.txt"
timesf="times.txt"
chanf="chan.txt"

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

      if [[ $REGIONAL_RR -eq 1 ]]; then
         tdate=`$NDATE +6 $cdate`
         day=`echo $tdate | cut -c1-8`
         cyc=`echo $cdate | cut -c9-10`
         . ${IG_SCRIPTS}/rr_set_tz.sh $cyc
      else
         day=`echo $cdate | cut -c1-8`
         cyc=`echo $cdate | cut -c9-10`
      fi

      #----------------------------------------------------
      #  Attempt to locate the extracted ieee data files.
      #
      ieee_src=${TANKverf}/${RUN}.${day}/${cyc}/${MONITOR}
      if [[ ! -d ${ieee_src} ]]; then
         ieee_src=${TANKverf}/${RUN}.${day}/${MONITOR}
      fi
      if [[ ! -d ${ieee_src} ]]; then
         ieee_src=${TANKverf}/${MONITOR}.${day}
      fi
      if [[ ! -d ${ieee_src} ]]; then
         ieee_src=${TANKverf}/${RUN}.${day}
      fi

      echo "rgnHH, rgnTM = $rgnHH, $rgnTM"

      test_file=${ieee_src}/time.${type}.${cdate}.ieee_d

      if [[ $USE_ANL = 1 ]]; then
         test_file2=${ieee_src}/time.${type}_anl.${cdate}.ieee_d
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

      adate=`$NDATE +${CYCLE_INTERVAL} ${cdate}`
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
   #  Build data files for server.
   #  This supports the javascript summary plot.
   #
   #  Algorithm:
   #    1. Copy summary.x locally
   #    2. For each entry in SATYTPE
   #       a) build the times.txt file from the data files
   #       b) build the use.txt file from the ctl file
   #       c) build the input file (namelist for summary.x)
   #       d) run the summary.x executable
   #       e) copy the [satype].sum.txt file to $TANKDIR/imgn/{suffix}/pngs/summary/.
   #       f) clean up

   echo "BEGIN data file generation:"
   rm -f $timesf
   
   if [[ ! -s summary.x ]]; then
      $NCP ${IG_EXEC}/radmon_ig_summary.x summary.x 
   fi

   ls ${type}.*.ieee_d | cut -d'.' -f2 > tmp
   tac tmp > $timesf
   rm tmp

   rm -f $usef
   cat ./${type}.ctl | grep iuse | gawk '{print $8}' > $usef

   nchanl=`cat ./${type}.ctl | grep title |gawk '{print $4}'`

   #------------------------------
   #  build chan.txt using ctl file
   #
   rm $chanf
   grep iuse ${type}.ctl | gawk '{print $5}' > $chanf

   ncycle=`cat $timesf | wc -l`
   st_time=`head -1 $times`
   cyc_per_day=$((24/$CYCLE_INTERVAL))           # number cycles per day

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
      cyc_per_day=${cyc_per_day},
     /
EOF

   rm -f input
   cp ${input} input
   ./summary.x < input > out.${type}
   echo "END data file generation:"

   rm -f ${input}

done


#--------------------------------------------------------------------
# Copy image files to $IMGNDIR to set up for mirror to web server.
# Delete images and data files.

if [[ ! -d ${IMGNDIR}/summary ]]; then
   mkdir -p ${IMGNDIR}/summary
fi
$NCP *summary.png ${IMGNDIR}/summary/.

for type in ${SATYPE2}; do
   $NCP ${type}.sum.txt ${IMGNDIR}/summary/${type}.${PDATE}.sum.txt
done
$NCP *.sum.txt ${IMGNDIR}/summary/.



#--------------------------------------------------------------------
# Clean $tmpdir. 
#
cd $tmpdir
cd ../
rm -rf $tmpdir

echo "End plot_summary.sh"

exit

