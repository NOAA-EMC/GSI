#!/bin/ksh

set -ax
export list=$listvar


# extract data only if flag is set to 1
if [[ $data_extract -eq 1 ]]; then

#------------------------------------------------------------------
# Set environment variables.
tmpdir=${stmproot}/$LOGNAME/ozone_monitor/horiz_${SUFFIX}_${string}.$PDATE
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir


#--------------------------------------------------------------------
#   Make tank directoy

mkdir -p $TANKDIR/horiz


#--------------------------------------------------------------------
#   Copy extraction program and data to working directory

$NCP $EXEDIR/horiz.x  ./horiz.x

RADDIR=$DATDIR
for type in ${SATYPE}; do
   $NCP $DATDIR/diag_${type}.${PDATE}.gz  ./${type}.gz
   gunzip ./${type}.gz
done


#--------------------------------------------------------------------
#   Run program for given time

iyy=`echo $PDATE | cut -c1-4`
imm=`echo $PDATE | cut -c5-6`
idd=`echo $PDATE | cut -c7-8`
ihh=`echo $PDATE | cut -c9-10`

for type in ${SATYPE}; do
   rm -f input

      $NCP $TANKDIR/horiz/${type}.ctl ./${type}.ctl

cat << EOF > input
 &INPUT
  satname='${type}',
  iyy=${iyy},
  imm=${imm},
  idd=${idd},
  ihh=${ihh},
  idhh=-18,
  incr=6,
 /
EOF
   $tmpdir/horiz.x < input >   stdout.$type
   $NCP ${type}.ctl              $TANKDIR/horiz/
   $NCP ${type}.${PDATE}.ieee_d  $TANKDIR/horiz/
   $NCP stdout.$type             $TANKDIR/horiz/
done


#-------------------------------------------------------------------
#   Remove old files in $TANKDIR/horiz

 rdate=`$NDATE -720 $PDATE`
 for type in ${SATYPE}; do
    rm -rf $TANKDIR/horiz/${type}.${rdate}.ieee_d
 done



#------------------------------------------------------------------
#   Clean up $tmpdir  Submit plot job
#
cd $tmpdir
cd ../
rm -rf $tmpdir

# End of data_extract block
fi

# Plot data only if flag is set to 1
if [[ $PLOT -eq 1 ]]; then

# Loop over satellite types.  Submit plot job for each type.
export listvars=PDATE,NDATE,DATDIR,TANKDIR,webpsw,webmch,webid,WEBDIR,EXEDIR,FIXDIR,LOGDIR,SCRIPTS,GSCRIPTS,STNMAP,GRADS,USER,SUB,SUFFIX,SATYPE,NCP,string,ptmproot,stmproot,transfer_plot,machine,listvars
suffix=a

if [ ! -d ${stmproot}/$LOGNAME/ozone_monitor/plotjobs_${SUFFIX}_${string} ] ; then
   mkdir -p ${stmproot}/$LOGNAME/ozone_monitor/plotjobs_${SUFFIX}_${string}
fi

cmdfile=${stmproot}/$LOGNAME/ozone_monitor/plotjobs_${SUFFIX}_${string}/cmdfile_phoriz
rm -f $cmdfile

count=`$bjobs | grep ${LOGNAME} | grep "${SUFFIX}_phoriz" | grep ${string} | wc -l`
if [[ $count -eq 0 ]] ; then
   rm -f $LOGDIR/plot_horiz_${string}.log
>$cmdfile
   for type in ${SATYPE}; do
      if [[ $type == 'omi_aura' || $type == 'gome_metop-a' ]] ; then
         list="obs ges obsges sza fovn"
      else
         list="obs ges obsges"
      fi
      echo "$SCRIPTS/plot_horiz.sh $type $suffix '$list'" >> $cmdfile
   done
   chmod u+x $cmdfile

   if [ $machine = WCOSS ]; then
     $SUB -P ${ACCOUNT} -q dev -o $LOGDIR/plot_horiz_${string}.log -M 30 -W 1:45 -R affinity[core] -J ${SUFFIX}_phoriz_${suffix}_${string} $cmdfile
   else
     $SUB -A ${ACCOUNT} -l procs=1,walltime=0:20:00 -N ${SUFFIX}_phoriz_$string -V -j oe -o $LOGDIR/plot_horiz_${suffix}_${string}.log $cmdfile
   fi
 

fi


# End of plotting block
fi

exit
