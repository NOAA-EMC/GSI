#! /bin/ksh -l

#------------------------------------------------------------------
#  plot_horiz.sh
#

echo "begin plot_horiz.sh"
set -ax


if [[ ${MY_MACHINE} = "theia" ]]; then
   module load grads
fi

export SATYPE=$1
export PVAR=$2
export PTYPE=$3
export string=ges

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
#   Set dates and copy data
#
#   4 cycles worth of data are required for horiz plots.
#   Start with PDATE and back up 3 times to get what we need.
#
ctr=0
cdate=$PDATE

while [[ $ctr -le 3 ]]; do
   c_pdy=`echo $cdate|cut -c1-8`
   c_cyc=`echo $cdate|cut -c9-10`
   tankdir_cdate=${TANKDIR}/${RUN}.${c_pdy}/${c_cyc}/oznmon/horiz
   $NCP ${tankdir_cdate}/${SATYPE}.ctl ./
   $NCP ${tankdir_cdate}/${SATYPE}*${c_pdy}* ./

   cdate=`$NDATE -6 $cdate`
   ctr=`expr $ctr + 1`
done

$UNCOMPRESS *${Z}

#----------------------------------------------------------------
#  Modify tdef line in .ctl file to start at bdate.
#
if [[ -e ${SATYPE}.ctl ]]; then
   edate=`$NDATE -18 $PDATE`
   ${OZN_IG_SCRIPTS}/update_ctl_tdef.sh ${SATYPE}.ctl ${edate} 4
fi

$NCP ${OZN_IG_GSCRPTS}/cbarnew.gs ./


$STNMAP -i ${SATYPE}.ctl

for var in ${PTYPE}; do

cat << EOF > ${SATYPE}_${var}.gs
'open ${SATYPE}.ctl'
'run ${OZN_IG_GSCRPTS}/plot_horiz_${string}.gs ${OZNMON_SUFFIX} ${RUN} ${SATYPE} ${var} x800 y700'
'quit'
EOF
   $GRADS -blc "run ${tmpdir}/${SATYPE}_${var}.gs"   


done 


#--------------------------------------------------------------------
#   Move image files to TANK
#

${NCP} *.png ${OZN_IMGN_TANKDIR}/.


#--------------------------------------------------------------------
# Clean $tmpdir.  Submit done job.
cd $tmpdir
cd ../
rm -rf $tmpdir

echo "end plot_horiz.sh"
exit

