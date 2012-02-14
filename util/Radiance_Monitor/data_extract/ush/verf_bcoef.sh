#!/bin/ksh

#------------------------------------------------------------------
#
# verf_bcoef.sh
#
# Extract the bcoef data from the radiance diagnostic files and 
# store in binary files.
#
# Log:
#  07/2010  safford  adapted from radmon suite
#------------------------------------------------------------------

set -ax
date
export list=$listvar



#------------------------------------------------------------------
# Set up tmpdir.
#
tmpdir=${WORKDIR}/bcoef_${SUFFIX}.$PDATE
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir


#--------------------------------------------------------------------
#   Rename TANKDIR to TANKDIR/bcoef and create if necessary

if [ ! -d $TANKDIR ]; then
   mkdir -p $TANKDIR
fi

TANKDIR=$TANKDIR/bcoef
if [ ! -d $TANKDIR ]; then
   mkdir -p $TANKDIR
fi



#--------------------------------------------------------------------
#   Copy extraction program and data to working directory

$NCP $EXEDIR/bcoef.${RAD_AREA}.x  ./bcoef.x

RADDIR=$DATDIR
for type in ${SATYPE}; do
   if [ -s $RADDIR/diag_${type}_ges.${PDATE}.Z ] ; then
      $NCP $RADDIR/diag_${type}_ges.${PDATE}.Z  ./${type}.Z
      uncompress ./${type}.Z
   fi
   $NCP $RADDIR/biascr.${PDATE}  ./biascr.txt
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
   npredr=5

cat << EOF > input
 &INPUT
  satname='${type}',
  npredr=${npredr},
  nchanl=${nchanl},
  iyy=${iyy},
  imm=${imm},
  idd=${idd},
  ihh=${ihh},
  idhh=-720,
  incr=6,
  suffix='${SUFFIX}',
  imkctl=${MAKE_CTL},
  imkdata=${MAKE_DATA},
 /
EOF
   timex $tmpdir/bcoef.x < input >   stdout.$type


#-------------------------------------------------------------------
#  copy data file back to $TANKDIR and compress
#  and copy stdout file (no compression)
#

   if [[ -s ${type}.${PDATE}.ieee_d ]]; then
      $NCP ${type}.${PDATE}.ieee_d  $TANKDIR
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
running=`expr $count - $complete`

if [[ $running -eq 1 ]]; then
   cd $WORKDIR
   cd ../
   rm -rf $WORKDIR
fi

exit
