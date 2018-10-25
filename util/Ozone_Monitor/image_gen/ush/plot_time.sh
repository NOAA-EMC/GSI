#! /bin/ksh

#------------------------------------------------------------------
#  plot_time.sh
#

set -ax


export SATYPE=$1
export PVAR=$2
export PTYPE=$3

echo "SATYPE, PVAR, PTYPE = $SATYPE, $PVAR, $PTYPE"
echo "RUN = $RUN"

#------------------------------------------------------------------
# Set work space for this SATYPE source.
#
tmpdir=${WORKDIR}/${SATYPE}.$PDATE.${PVAR}
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

#------------------------------------------------------------------
#   Set dates and copy data files
#
#   120 cycles worth of data (30 days) are required for time plots.
#   Start with PDATE and back up 119 times to get what we need.
#

ctr=0
cdate=$PDATE

while [[ $ctr -le 119 ]]; do
   c_pdy=`echo $cdate|cut -c1-8`
   c_cyc=`echo $cdate|cut -c9-10`
   tankdir_cdate=${TANKDIR}/${RUN}.${c_pdy}/${c_cyc}/oznmon/time

   if [[ ! -e ./${SATYPE}.ctl ]]; then
      $NCP ${tankdir_cdate}/${SATYPE}.ctl ./
   fi

   data_file=${tankdir_cdate}/${SATYPE}.${cdate}.ieee_d
   if [[ -s ${data_file} ]]; then
      $NCP ${data_file} ./
   else
      data_file=${data_file}.${Z}
      if [[ -s ${data_file} ]]; then
         $NCP ${data_file} ./
         $UNCOMPRESS ${data_file}
      fi
   fi

   cdate=`$NDATE -6 $cdate`
   ctr=`expr $ctr + 1`
done



#----------------------------------------------------------------
#  Modify tdef line in .ctl file to start at bdate.
#
if [[ -e ${SATYPE}.ctl ]]; then
   edate=`$NDATE -720 $PDATE`
   ${OZN_IG_SCRIPTS}/update_ctl_tdef.sh ${SATYPE}.ctl ${edate} 121
fi


for var in ${PTYPE}; do
   echo $var

cat << EOF > ${SATYPE}_${var}.gs
'reinit'
'clear'
'open  ${SATYPE}.ctl'
'run ${OZN_IG_GSCRPTS}/plot_time_${string}.gs ${OZNMON_SUFFIX} ${RUN} ${SATYPE} ${var} x750 y700'
'quit'
EOF

   echo ${tmpdir}/${SATYPE}_${var}.gs

   $GRADS -bpc "run ${tmpdir}/${SATYPE}_${var}.gs"

done 


#--------------------------------------------------------------------
#  copy image files to TANKDIR
#
${NCP} *.png ${OZN_IMGN_TANKDIR}/.


#--------------------------------------------------------------------
# Clean $tmpdir.  Submit done job.
cd $tmpdir
cd ../
rm -rf $tmpdir

exit

