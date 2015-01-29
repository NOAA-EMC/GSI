#! /bin/ksh

set -ax
export list=$listvars

export SATYPE2=$1

#------------------------------------------------------------------
# Set environment variables.
export tmpdir=${stmproot}/$LOGNAME/ozone_monitor/plot_summary_${SUFFIX}_$string_${SATYPE2}.$PDATE
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir



#------------------------------------------------------------------
#   Set dates
bdate=`$NDATE -168 $PDATE`
edate=$PDATE
bdate0=`echo $bdate|cut -c1-8`
edate0=`echo $edate|cut -c1-8`


for type in ${SATYPE2}; do

    $NCP $TANKDIR/time/${type}.ctl ./

    cdate=$bdate
    while [[ $cdate -le $edate ]]; do
       $NCP $TANKDIR/time/${type}.${cdate}.ieee_d ./
       adate=`$NDATE +6 $cdate`
       cdate=$adate
    done

cat << EOF > ${type}.gs
'open ${type}.ctl'
'run ${GSCRIPTS}/plot_summary.gs ${type} x750 y700'
'quit'
EOF
    $GRADS -bpc "run ${tmpdir}/${type}.gs"

#   rm -f ${type}.ctl 
#   rm -f ${type}*.ieee_d
#   rm -f ${type}.summary.png

done

  if [[ $transfer_plot -eq 1 ]] ; then
#    transfer plots from wcoss to rzdm
     rm -f  $LOGDIR/transfer_summary_${SATYPE2}.log
     export subdir=summary
     export listvar1=PDATE,webpsw,webmch,webid,WEBDIR,LOGDIR,USER,SUB,SUFFIX,SATYPE2,string,PVAR,subdir,tmpdir,listvar1
     $SUB -P ${PROJECT} -q transfer -o $LOGDIR/transfer_summary_${SATYPE2}.log -M 30 -W 0:45 -R affinity[core] -J transfer_summary ${SCRIPTS}/transfer.sh
  fi


#--------------------------------------------------------------------
# Clean $tmpdir.  Submit done job.
cd $tmpdir
cd ../
#rm -rf $tmpdir

exit

