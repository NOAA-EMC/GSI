#!/bin/ksh

set -ax
date
export list=$listvar

if [[ $data_extract -eq 1 ]]; then


#------------------------------------------------------------------
# Set environment variables.
tmpdir=${stmproot}/$LOGNAME/ozone_monitor/time_${SUFFIX}_${string}.$PDATE
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir


#--------------------------------------------------------------------
#   Make tank directoy

mkdir -p $TANKDIR/time


#--------------------------------------------------------------------
#   Copy extraction program and data to working directory

$NCP $EXEDIR/time.x  ./time.x

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

   $NCP $TANKDIR/time/${type}.ctl ./${type}.ctl

cat << EOF > input
 &INPUT
  satname='${type}',
  iyy=${iyy},
  imm=${imm},
  idd=${idd},
  ihh=${ihh},
  idhh=-720,
  incr=6,
  nregion=6,
  region(1)='global',              rlonmin(1)=-180.0,rlonmax(1)=180.0,rlatmin(1)=-90.0,rlatmax(1)= 90.0,
  region(2)='70N-90N',             rlonmin(2)=-180.0,rlonmax(2)=180.0,rlatmin(2)= 70.0,rlatmax(2)= 90.0,
  region(3)='20N-70N',             rlonmin(3)=-180.0,rlonmax(3)=180.0,rlatmin(3)= 20.0,rlatmax(3)= 70.0,
  region(4)='20S-20N',             rlonmin(4)=-180.0,rlonmax(4)=180.0,rlatmin(4)=-20.0,rlatmax(4)= 20.0,
  region(5)='20S-70S',             rlonmin(5)=-180.0,rlonmax(5)=180.0,rlatmin(5)=-70.0,rlatmax(5)=-20.0,
  region(6)='70S-90S',             rlonmin(6)=-180.0,rlonmax(6)=180.0,rlatmin(6)=-90.0,rlatmax(6)=-70.0,
 /
EOF
   $tmpdir/time.x < input >   stdout.$type
   $NCP ${type}.ctl              $TANKDIR/time/
   $NCP ${type}.${PDATE}.ieee_d  $TANKDIR/time/
   $NCP stdout.$type             $TANKDIR/time/

done


#-------------------------------------------------------------------
#   Remove old files in $TANKDIR/time

  rdate=`$NDATE -720 $PDATE`
  for type in ${SATYPE}; do
     rm -rf $TANKDIR/time/${type}.${rdate}.ieee_d
  done

#------------------------------------------------------------------
#   Clean up $tmpdir  Submit plot job
#
cd $tmpdir
cd ../
#rm -rf $tmpdir

# End of data extract block
fi

if [ ! -d ${stmproot}/$LOGNAME/ozone_monitor/plotjobs_${SUFFIX}_${string} ] ; then 
   mkdir -p ${stmproot}/$LOGNAME/ozone_monitor/plotjobs_${SUFFIX}_${string}
fi

# Submit plotting jobs only if PLOT=1
if [[ $PLOT -eq 1 ]]; then


# Loop over satellite types.  Submit poe job to make plots.  Each task handles
# a single satellite type

export listvars=PDATE,NDATE,DATDIR,TANKDIR,webpsw,webmch,webid,WEBDIR,EXEDIR,LOGDIR,SCRIPTS,GSCRIPTS,STNMAP,GRADS,USER,SUB,SUFFIX,SATYPE,NCP,string,ptmproot,stmproot,transfer_plot,machine,listvars
list="count omg cpen"
suffix=a
cmdfile=${stmproot}/$LOGNAME/ozone_monitor/plotjobs_${SUFFIX}_${string}/cmdfile_ptime_${suffix}

rm -f $cmdfile

count=`$bjobs | grep $LOGNAME | grep "${SUFFIX}_ptime_${suffix}_${string}" | wc -l`
if [[ $count -eq 0 ]] ; then
   rm -f $LOGDIR/plot_time_${suffix}_${string}.log
>$cmdfile
   for type in ${SATYPE}; do
      echo "$SCRIPTS/plot_time.sh $type $suffix '$list'" >> $cmdfile
   done
   chmod 755 $cmdfile
   if [ $machine = WCOSS ]; then
      $SUB -P ${ACCOUNT} -q dev -o $LOGDIR/plot_time_${suffix}_${string}.log -M 30 -W 1:45 -R affinity[core] -J ${SUFFIX}_ptime_${string} $cmdfile  
   else
#    $SUB -A $ACCOUNT -l procs=1,walltime=0:20:00 -N ${SUFFIX}_ptime_$string -v $listvars -j oe -o $LOGDIR/plot_time_${suffix}_${string}.log $cmdfile
     $SUB -A $ACCOUNT -l procs=1,walltime=0:20:00 -N ${SUFFIX}_ptime_$string -V -j oe -o $LOGDIR/plot_time_${suffix}_${string}.log $cmdfile
   fi
fi

if [[ $string == 'ges' ]] ; then
# Variable SATYPE contains list of satellites/instruments to plot.  The summary plots are made for all cycles

export listvars=PDATE,NDATE,DATDIR,TANKDIR,webpsw,webmch,webid,WEBDIR,EXEDIR,LOGDIR,SCRIPTS,GSCRIPTS,STNMAP,GRADS,USER,SUB,SUFFIX,SATYPE,NCP,string,ptmproot,stmproot,transfer_plot,machine,listvars
cmdfile=${stmproot}/$LOGNAME/ozone_monitor/plotjobs_${SUFFIX}_${string}/cmdfile_psummary
rm -f $cmdfile
count=`$bjobs | grep $LOGNAME | grep "${SUFFIX}_psummary_${string}" | wc -l`
if [[ $count -eq 0 ]] ; then
   rm -f $LOGDIR/plot_summary_${string}.log
>$cmdfile
   for type in ${SATYPE}; do
      if [[ $type != "omi_aura" && $type != "gome_metop-a" && $type != "gome_metop-b" ]] ; then
         echo "$SCRIPTS/plot_summary.sh $type" >> $cmdfile
      fi
   done
   chmod 755 $cmdfile
   if [ $machine = WCOSS ]; then
      $SUB -P ${ACCOUNT} -q dev -o $LOGDIR/plot_summary_${string}.log -M 30 -W 1:45 -R affinity[core] -J ${SUFFIX}_psummary_${string} $cmdfile
   else
#    $SUB -A $ACCOUNT -l procs=1,walltime=0:20:00 -N ${SUFFIX}_psummary_$string -v $listvars -j oe -o $LOGDIR/plot_summary_${suffix}_${string}.log $cmdfile
     $SUB -A $ACCOUNT -l procs=1,walltime=0:20:00 -N ${SUFFIX}_psummary_$string -V -j oe -o $LOGDIR/plot_summary_${suffix}_${string}.log $cmdfile
   fi
fi
fi

# End of plotting block
fi

exit


