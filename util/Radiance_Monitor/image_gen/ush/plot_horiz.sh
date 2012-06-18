#!/bin/ksh

#------------------------------------------------------------------
#
#  plot_horiz.sh
#
#  This version of horiz does not use ieee_d files as a data 
#  storage.  It can only make plots for dates for which data still
#  exists in $RADDIR directory. 
#
#------------------------------------------------------------------
set -ax
export list=$listvars

echo calling sequence $0 $1 $2 $3 $4

SAT=$1
echo $SAT

#------------------------------------------------------------------
# Set up tempdir and link data and ctl files.  

pid_test=`echo $PID | tail -c -3`
if [[ ${pid_test} = "_1" || ${pid_test} = "_2" ]]; then
   tmpdir=${DATADIR}/horiz_${SUFFIX}.${PID}.${PDATE}
else
   tmpdir=${DATADIR}/horiz_${SUFFIX}.${SAT}.${PDATE}
fi
rm -rf $tmpdir

mkdir -p $tmpdir
cd $tmpdir

ln ${DATADIR}/${SAT}.* ${tmpdir}/. 


#------------------------------------------------------------------
#------------------------------------------------------------------
#
#  PLOTTING BLOCK
#
#  Loop over satellite types.  Submit plot job for each type.

$NCP ${GSCRIPTS}/cbarnew.gs ./
STNMAP="/usrx/local/grads/bin/stnmap"
GRADS="/usrx/local/grads/bin/grads"


$STNMAP -i ${SAT}.ctl

for var in ${PTYPE}; do

cat << EOF > ${SAT}_${var}.gs
'open ${SAT}.ctl'
'run ${GSCRIPTS}/plot_horiz.gs ${SAT} ${var} x1100 y850'
'quit'
EOF

   cmdfile="cmdfile_${SAT}_${var}"
   rm -f $cmdfile

cat << EOF > ${cmdfile}
${GRADS} -blc run ${SAT}_${var}.gs
EOF

   timex $GRADS -blc "run ${SAT}_${var}.gs"
done



#------------------------------------------------------------------
#   Transfer image files to server

#ssh -l ${WEB_USER} ${WEB_SVR} "mkdir -p ${WEBDIR}/horiz"

#if [[ ${PID} = "iasi_1" || ${PID} = "iasi_2" ]]; then
#   nums="_1 _2 _3 _4 _5 _6 _7 _8 _9"

#   for var in ${PTYPE}; do
#      for num in ${nums}; do
#         scp ${SAT}.${var}${num}*.png    ${WEB_USER}@${WEB_SVR}:${WEBDIR}/horiz
#      done
#   done
#else
#   for var in ${PTYPE}; do
#      scp ${SAT}.${var}*.png    ${WEB_USER}@${WEB_SVR}:${WEBDIR}/horiz
#   done
#fi

scp *.png    ${WEB_USER}@${WEB_SVR}:${WEBDIR}/horiz

#--------------------------------------------------------------------
# Delete images and data files in $tmpdir

#for var in ${PTYPE}; do
#   rm -f ${SAT}.${var}*.png
#done
#rm -f ${SAT}*.ieee_d
#rm -f ${SAT}.ctl
#rm -f ${SAT}.map
#rm -f ${SAT}*.gs


#------------------------------------------------------------------
#------------------------------------------------------------------
#   Clean up horiz ploting directory if other processes are
#   finished (if running is 1, this is the last horiz process 
#   running).
#

cd $tmpdir
cd ..
rm -rf $tmpdir

cat ${LOADLQ}/plot_${SUFFIX}_horiz* 

#count=`ls ${LOADLQ}/plot_${SUFFIX}* | wc -l`
#complete=`grep "COMPLETED" ${LOADLQ}/plot_${SUFFIX}* | wc -l`

#running=`expr $count - $complete`

#if [[ $running -eq 1 ]] ; then
#  cd $DATADIR
#  cd ..
#  rm -rf $DATADIR
#fi

exit
