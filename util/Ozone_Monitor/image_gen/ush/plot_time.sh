#! /bin/ksh -l

#------------------------------------------------------------------
#  plot_time.sh
#

set -ax

if [[ ${MY_MACHINE} = "hera" ]]; then
   module load grads
fi


export SATYPE=$1
export PVAR=$2
export PTYPE=$3
dsrc=$4

echo "SATYPE, PVAR, PTYPE, dsrc = $SATYPE, $PVAR, $PTYPE $dsrc"
echo "RUN = $RUN"

echo COMP1, COMP2, DO_COMP = $COMP1, $COMP2, $DO_COMP

ADD_COMP=0
if [[ $SATYPE = $COMP1 ]]; then
   ADD_COMP=1
fi

#------------------------------------------------------------------
# Set work space for this SATYPE source.
#
tmpdir=${WORKDIR}/${SATYPE}.${dsrc}.$PDATE.${PVAR}
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

   if [[ ! -e ./${SATYPE}.${dsrc}.ctl ]]; then
      $NCP ${tankdir_cdate}/${SATYPE}.${dsrc}.ctl ./
   fi

   data_file=${tankdir_cdate}/${SATYPE}.${dsrc}.${cdate}.ieee_d
   if [[ -s ${data_file} ]]; then
      $NCP ${data_file} ./
   else
      data_file=${data_file}.${Z}
      if [[ -s ${data_file} ]]; then
         $NCP ${data_file} ./
         $UNCOMPRESS ${data_file}
      fi
   fi

   if [[ $ADD_COMP -eq 1 ]]; then
      if [[ ! -e ./${COMP2}.${dsrc}.ctl ]]; then
         $NCP ${tankdir_cdate}/${COMP2}.${dsrc}.ctl ./
      fi
      
      data_file=${tankdir_cdate}/${COMP2}.${dsrc}.${cdate}.ieee_d
      if [[ -s ${data_file} ]]; then
         $NCP ${data_file} ./
      else
         data_file=${data_file}.${Z}
         if [[ -s ${data_file} ]]; then
            $NCP ${data_file} ./
            $UNCOMPRESS ${data_file}
         fi
      fi

   fi

   cdate=`$NDATE -6 $cdate`
   ctr=`expr $ctr + 1`
done



#----------------------------------------------------------------
#  Modify tdef line in .ctl file to start at bdate.
#
if [[ -e ${SATYPE}.${dsrc}.ctl ]]; then
   edate=`$NDATE -720 $PDATE`
   ${OZN_IG_SCRIPTS}/update_ctl_tdef.sh ${SATYPE}.${dsrc}.ctl ${edate} 121

   if [[ $ADD_COMP -eq 1 ]]; then
      ${OZN_IG_SCRIPTS}/update_ctl_tdef.sh ${COMP2}.${dsrc}.ctl ${edate} 121
   fi
fi


for var in ${PTYPE}; do
   echo $var

   if [[ $ADD_COMP -eq 0 ]]; then

cat << EOF > ${SATYPE}_${var}.gs
'reinit'
'clear'
'open  ${SATYPE}.${dsrc}.ctl'
'run ${OZN_IG_GSCRPTS}/plot_time_${dsrc}.gs ${OZNMON_SUFFIX} ${RUN} ${SATYPE} ${var} x750 y700'
'quit'
EOF

   else

cat << EOF > ${SATYPE}_${var}.gs
'reinit'
'clear'
'open  ${SATYPE}.${dsrc}.ctl'
'open  ${COMP2}.${dsrc}.ctl'
'run ${OZN_IG_GSCRPTS}/plot_time_${dsrc}_2x.gs ${OZNMON_SUFFIX} ${RUN} ${SATYPE} ${COMP2} ${var} x750 y700'
'quit'
EOF

   fi

   echo ${tmpdir}/${SATYPE}_${var}.gs
   $GRADS -bpc "run ${tmpdir}/${SATYPE}_${var}.gs"

done 


#--------------------------------------------------------------------
#  copy image files to TANKDIR
#
${NCP} *.png ${OZN_IMGN_TANKDIR}/.


#--------------------------------------------------------------------
# Clean $tmpdir.  Submit done job.
#cd $tmpdir
#cd ../
#rm -rf $tmpdir

exit

