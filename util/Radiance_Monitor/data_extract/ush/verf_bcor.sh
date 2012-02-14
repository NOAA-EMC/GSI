#!/bin/ksh

#------------------------------------------------------------------
#
#  verf_bcor_opr.sh
#
#  Extract bcor data from the radiance diagnostic files and store 
#  in binary files.
#
#  Log:
#   08/2010  safford  adapted from radmon suite
#------------------------------------------------------------------

set -ax
date
export list=$listvar



#------------------------------------------------------------------
# Set environment variables.
tmpdir=${WORKDIR}/bcor_${SUFFIX}.$PDATE
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir



#--------------------------------------------------------------------
#   Rename TANKDIR to TANKDIR/bcor and create if necessary

if [ ! -d $TANKDIR ]; then
   mkdir -p $TANKDIR
fi

TANKDIR=$TANKDIR/bcor
if [ ! -d $TANKDIR ]; then
   mkdir -p $TANKDIR
fi


#--------------------------------------------------------------------
#   Copy extraction program and data to working directory

$NCP $EXEDIR/bcor.${RAD_AREA}.x  ./bcor.x

RADDIR=$DATDIR
for type in ${SATYPE}; do
   $NCP $RADDIR/diag_${type}_ges.${PDATE}.Z  ./${type}.Z
   uncompress ./${type}.Z
done



#--------------------------------------------------------------------
#   Run program for given time

iyy=`echo $PDATE | cut -c1-4`
imm=`echo $PDATE | cut -c5-6`
idd=`echo $PDATE | cut -c7-8`
ihh=`echo $PDATE | cut -c9-10`

for type in ${SATYPE}; do
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
 /
EOF
   timex $tmpdir/bcor.x < input >   stdout.$type

#-------------------------------------------------------------------
#  copy data file back to $TANKDIR and compress
#  copy stdout file (no compression)
#

   if [[ -s ${type}.${PDATE}.ieee_d ]]; then
      $NCP ${type}.${PDATE}.ieee_d* $TANKDIR
      compress -f $TANKDIR/${type}.${PDATE}.ieee_d
   fi

   $NCP stdout.$type             $TANKDIR

   if [[ -s ${type}.ctl ]]; then
      $NCP ${type}.ctl ${TANKDIR}
      compress -f ${TANKDIR}/${type}.ctl
   fi

done

#-------------------------------------------------------------------
#   Remove old files in $TANKDIR

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
