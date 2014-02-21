#! /bin/ksh

set -ax
export list=$listvars

export SATYPE2=$1
export PVAR=$2
export PTYPE=$3

if [[ "$SUFFIX" != "opr" ]]; then
  export plot_2files=0
else 
  export plot_2files=0
fi

#------------------------------------------------------------------
# Set environment variables.
export tmpdir=${stmproot}/$LOGNAME/ozone_monitor/plot_time_${SUFFIX}_${string}_${SATYPE2}.$PDATE.${PVAR}
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir


#------------------------------------------------------------------
#   Set dates
bdate=`$NDATE -720 $PDATE`
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


if [[ $plot_2files == 1 ]] ; then
   ln -s /da/noscrub/Haixia.Liu/ozone/monitor/stats/opr/ges/time time_opr

  for var in ${PTYPE}; do
    echo $var
cat << EOF > ${type}_${var}.gs
'reinit'
'clear'
'open  ${type}.ctl'
'open  time_opr/${type}.ctl'
'run ${GSCRIPTS}/plot_time_${string}.gs.2files ${type} ${var} x750 y700'
'quit'
EOF
    echo ${tmpdir}/${type}_${var}.gs

    $GRADS -bpc "run ${tmpdir}/${type}_${var}.gs"

  done 

else
  for var in ${PTYPE}; do
    echo $var
cat << EOF > ${type}_${var}.gs
'reinit'
'clear'
'open  ${type}.ctl'
'run ${GSCRIPTS}/plot_time_${string}.gs ${type} ${var} x750 y700'
'quit'
EOF

    echo ${tmpdir}/${type}_${var}.gs

    $GRADS -bpc "run ${tmpdir}/${type}_${var}.gs"

#   rename the analysis plots
    if [[ $string == 'anl' ]] ; then
       reglist='region1 region2 region3 region4 region5 region6'
       frlist='fr1 fr2 fr3 fr4 fr5 fr6'
             if [[ ${var} == 'count' ]] ; then
                if [[ $type == 'omi_aura' || $type == 'gome_metop-a' || $type == 'gome_metop-b' ]] ; then
                   for region in $reglist ; do
                     mv ${type}.count_${region}_fr1.png ${type}.countanl_${region}_fr1.png
                   done
                else
                   for region in $reglist ; do
                   for fr in $frlist ; do
                      mv ${type}.count_${region}_${fr}.png ${type}.countanl_${region}_${fr}.png
                   done
                   done
                fi
             elif [[ ${var} == 'omg' ]] ; then
                if [[ $type == 'omi_aura' || $type == 'gome_metop-a' || $type == 'gome_metop-b' ]] ; then
                   for region in $reglist ; do
                     mv ${type}.omg_${region}_fr1.png ${type}.oma_${region}_fr1.png
                   done
                else
                   for region in $reglist ; do
                   for fr in $frlist ; do
                      mv ${type}.omg_${region}_${fr}.png ${type}.oma_${region}_${fr}.png
                   done
                   done
                fi
             else
                if [[ $type == 'omi_aura' || $type == 'gome_metop-a' || $type == 'gome_metop-b' ]] ; then
                   for region in $reglist ; do
                     mv ${type}.cpen_${region}_fr1.png ${type}.cpenanl_${region}_fr1.png
                   done
                else
                   for region in $reglist ; do
                   for fr in $frlist ; do
                      mv ${type}.cpen_${region}_${fr}.png ${type}.cpenanl_${region}_${fr}.png
                   done
                   done
                fi
             fi
    fi
  done 
fi


  if [[ $transfer_plot -eq 1 ]] ; then
#    transfer plots from wcoss to rzdm
     rm -f  $LOGDIR/transfer_time_${SATYPE2}.log
     export subdir=time
     export listvar1=PDATE,webpsw,webmch,webid,WEBDIR,LOGDIR,USER,SUB,SUFFIX,SATYPE2,string,PVAR,subdir,tmpdir,listvar1
     $SUB -P ${PROJECT} -q transfer -o $LOGDIR/transfer_time_${SATYPE2}.log -M 30 -W 0:45 -R affinity[core] -J transfer_time ${SCRIPTS}/transfer.sh
  fi

done


#--------------------------------------------------------------------
# Clean $tmpdir.  Submit done job.
cd $tmpdir
cd ../
#rm -rf $tmpdir

exit

