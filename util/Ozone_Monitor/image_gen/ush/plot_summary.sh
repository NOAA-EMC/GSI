#! /bin/ksh -l

#------------------------------------------------------------------
#  plot_summary.sh
#

set -ax

if [[ ${MY_MACHINE} = "hera" ]]; then
   module load grads
fi

SATYPE=$1
ptype=$2

#------------------------------------------------------------------
# Set work space for this SATYPE source.
#
tmpdir=${WORKDIR}/${SATYPE}.${ptype}.${PDATE}
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir


#------------------------------------------------------------------
#   Set dates and copy data files
#
#   120 cycles worth of data (30 days) are required for summary 
#   plots.  Start with PDATE and back up 119 times.
#

ctr=0
cdate=$PDATE

while [[ $ctr -le 120 ]]; do
   c_pdy=`echo $cdate|cut -c1-8`
   c_cyc=`echo $cdate|cut -c9-10`
   tankdir_cdate=${TANKDIR}/${RUN}.${c_pdy}/${c_cyc}/oznmon/time

   if [[ ! -e ./${SATYPE}.${ptype}.ctl ]]; then
      $NCP ${tankdir_cdate}/${SATYPE}.${ptype}.ctl ./
   fi
   
   data_file=${tankdir_cdate}/${SATYPE}.${ptype}.${cdate}.ieee_d
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
#  Modify tdef line in .ctl file to start at bdate.  tdef line 
#  should be 1 more than the total number of cycles so the last
#  cycle will be the cycle specified by $PDATE.
#
if [[ -e ${SATYPE}.${ptype}.ctl ]]; then
   bdate=`$NDATE -720 $PDATE`
   ${OZN_IG_SCRIPTS}/update_ctl_tdef.sh ${SATYPE}.${ptype}.ctl ${bdate} 121 
fi

cat << EOF > ${SATYPE}.gs
'open ${SATYPE}.${ptype}.ctl'
'run ${OZN_IG_GSCRPTS}/plot_summary.gs ${OZNMON_SUFFIX} ${RUN} ${SATYPE} ${ptype} x750 y700'
'quit'
EOF

$GRADS -bpc "run ${tmpdir}/${SATYPE}.gs"


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

