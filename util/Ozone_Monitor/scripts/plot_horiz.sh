#! /bin/ksh

set -ax
export list=$listvars

export SATYPE2=$1
export PVAR=$2
export PTYPE=$3

#------------------------------------------------------------------
# Set environment variables.
export tmpdir=${stmproot}/$LOGNAME/ozone_monitor/plot_horiz_${SUFFIX}_${string}_${SATYPE2}.$PDATE.${PVAR}
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir



#------------------------------------------------------------------
#   Set dates
bdate=`$NDATE -18 $PDATE`
edate=$PDATE
bdate0=`echo $bdate|cut -c1-8`
edate0=`echo $edate|cut -c1-8`



#--------------------------------------------------------------------
# Copy control files to $tmpdir
$NCP $TANKDIR/horiz/*.ctl ./
$NCP $TANKDIR/horiz/*${bdate0}* ./
$NCP $TANKDIR/horiz/*${edate0}* ./
$NCP ${GSCRIPTS}/cbarnew.gs ./



for type in ${SATYPE2}; do
   date

   $STNMAP -i ${type}.ctl

   for var in ${PTYPE}; do
cat << EOF > ${type}_${var}.gs
'open ${type}.ctl'
'run ${GSCRIPTS}/plot_horiz_${string}.gs ${type} ${var} x800 y700'
'quit'
EOF
      $GRADS -blc "run ${tmpdir}/${type}_${var}.gs"   

#  rename the analysis plots
   if [[ $string == 'anl' ]] ; then
      levlist='1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22'
      if [[ ${var} == 'ges' ]] ; then
         if [[ $type == 'omi_aura' || $type == 'gome_metop-a' || $type == 'gome_metop-b' ]] ; then
            mv ${type}.ges_1.png ${type}.anl_1.png
         else
           for level in $levlist ; do
               mv ${type}.ges_${level}.png ${type}.anl_${level}.png
           done
         fi
      fi
      if [[ ${var} == 'obsges' ]] ; then
         if [[ $type == 'omi_aura' || $type == 'gome_metop-a' || $type == 'gome_metop-b' ]] ; then
            mv ${type}.obsges_1.png ${type}.obsanl_1.png
         else
           for level in $levlist ; do
               mv ${type}.obsges_${level}.png ${type}.obsanl_${level}.png
           done
         fi
      fi
   fi

   done 


   if [[ $transfer_plot -eq 1 ]] ; then
#     transfer plots from wcoss to rzdm
      rm -f $LOGDIR/transfer_horiz_${SATYPE2}.log
      export subdir=horiz
      export listvar1=PDATE,webpsw,webmch,webid,WEBDIR,LOGDIR,USER,SUB,SUFFIX,SATYPE2,string,PVAR,subdir,tmpdir,listvar1
      $SUB -P ${PROJECT} -q transfer -o $LOGDIR/transfer_horiz_${SATYPE2}.log -M 30 -W 0:45 -R affinity[core] -J transfer_horiz ${SCRIPTS}/transfer.sh
   fi
  
done


#--------------------------------------------------------------------
# Clean $tmpdir.  Submit done job.
cd $tmpdir
cd ../
#rm -rf $tmpdir

exit

