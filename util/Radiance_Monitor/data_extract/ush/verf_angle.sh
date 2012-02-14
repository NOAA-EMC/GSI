#!/bin/ksh

#####################################################################
#
#  verf_angle.sh
#
#  Extract the angle data from the radiance diagnostic files and
#  store in binary files.
#
#  Log:
#  07/2010  safford  adapted from radmon suite
#  08/2010  safford  remove ctl file save
#####################################################################

set -ax
date

gesanl=ges
#if [[ $USE_ANL -eq 1 ]]; then
#   gesanl='ges anl'
#else
#   gesanl=ges
#fi

#--------------------------------------------------------------------
# Set environment variables.

tmpdir=${WORKDIR}/angle_bias_${SUFFIX}.$PDATE
mkdir -p $tmpdir
cd $tmpdir


#--------------------------------------------------------------------
#   Rename TANKDIR and create if necessary
#

if [ ! -d $TANKDIR ]; then
   mkdir -p $TANKDIR
fi

TANKDIR=$TANKDIR/angle
if [ ! -d $TANKDIR ]; then
   mkdir -p $TANKDIR
fi


#--------------------------------------------------------------------
#   Copy extraction program and data to working directory

if [[ $SUFFIX = oex ]]; then
   $NCP $EXEDIR/angle_bias_oex.${RAD_AREA}.x  ./angle_bias.x
elif [[ $SUFFIX = xrn ]]; then
   $NCP $EXEDIR/angle_bias_xrn.${RAD_AREA}.x  ./angle_bias.x
else
   $NCP $EXEDIR/angle_bias.${RAD_AREA}.x  ./angle_bias.x
fi
$NCP $EXEDIR/scaninfo.txt  ./scaninfo.txt

$NCP $DATDIR/satang.${PDATE} ./satang.txt

RADDIR=$DATDIR
for type in ${SATYPE}; do
   $NCP $RADDIR/diag_${type}_ges.${PDATE}.Z  ./${type}.Z
   uncompress ./${type}.Z

   if [[ $USE_ANL -eq 1 ]]; then
      $NCP $RADDIR/diag_${type}_anl.${PDATE}.Z  ./${type}_anl.Z
      uncompress ./${type}_anl.Z
   fi
done



#--------------------------------------------------------------------
#   Run program for given time

iyy=`echo $PDATE | cut -c1-4`
imm=`echo $PDATE | cut -c5-6`
idd=`echo $PDATE | cut -c7-8`
ihh=`echo $PDATE | cut -c9-10`

for type in ${SATYPE}; do
   for dtype in ${gesanl}; do

      if [[ $dtype == "anl" ]]; then
         data_file=${type}.${PDATE}_anl.ieee_d
         ctl_file=${type}_anl.ctl
         stdout_file=stdout.${type}_anl
      else
         data_file=${type}.${PDATE}.ieee_d
         ctl_file=${type}.ctl
         stdout_file=stdout.${type}
      fi

      rm input

      nchanl=-999
cat << EOF > input
 &INPUT
  satname='${type}',
  iyy=${iyy},
  imm=${imm},
  idd=${idd},
  ihh=${ihh},
  idhh=-720,
  incr=6,
  nchanl=${nchanl},
  suffix='${SUFFIX}',
  imkctl=${MAKE_CTL},
  imkdata=${MAKE_DATA},
  gesanl='${dtype}',
 /
EOF
      timex $tmpdir/angle_bias.x < input >   ${stdout_file}


#-------------------------------------------------------------------
#  copy data and control files to $TANKDIR and compress
#  copy stdout to $TANKDIR (no compression)
# 

      if [[ -s ${data_file} ]]; then
         $NCP ${data_file} $TANKDIR
         compress -f $TANKDIR/${data_file}
      fi

      if [[ -s ${ctl_file} ]]; then
         $NCP ${ctl_file}  ${TANKDIR}
         compress -f ${TANKDIR}/${ctl_file}
      fi 

      $NCP ${stdout_file}  $TANKDIR
   done
done

#-------------------------------------------------------------------
#   Remove files older than 90 days in $TANKDIR/angle
#

find ${TANKDIR} -name "*.ieee_d*" -mtime +91 -exec rm {} \; 


#------------------------------------------------------------------
#   Clean up $tmpdir and $WORKDIR if this is the last verf job.
#

cd $tmpdir
cd ../
rm -rf $tmpdir

count=`ls ${LOADLQ}/verf*_$SUFFIX* | wc -l`
complete=`grep "COMPLETED" ${LOADLQ}/verf*_$SUFFIX* | wc -l`

total=`expr $count - $complete`

if [[ $total -le 1 ]]; then
   cd $WORKDIR
   cd ../
   rm -rf $WORKDIR
fi

exit
