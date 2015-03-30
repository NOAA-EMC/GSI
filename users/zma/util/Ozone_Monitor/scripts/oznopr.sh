#!/bin/ksh

set -ax
export list=$list0


#------------------------------------------------------------------
# Set environment variables.
 tmpdir=${stmproot}/$LOGNAME/ozone_monitor/ozn${SUFFIX}.$PDATE.$string
 rm -rf $tmpdir
 mkdir -p $tmpdir
 cd $tmpdir


#--------------------------------------------------------------------
#   Set environment variables to export to subsequent scripts

if [[ "$SUFFIX" = "opr" ]]; then
   export WEBDIR=${WEBDIR_OZN:-/home/www/emc/htdocs/gmb/wx20hl/ozone/monitor/pngs}
else
   export WEBDIR=${WEBDIR_OZN:-/home/www/emc/htdocs/gmb/wx20hl/ozone/monitor/pngs.${SUFFIX}}
fi

export webpsw=${webpsw:-/u/$LOGNAME/.open02}
export webmch=${webmch:-emcrzdm.ncep.noaa.gov}
export webid=${webid:-wx20hl}

export GROUP=${GROUP:-g01}
if [ $machine = WCOSS ]; then
   export STNMAP=${GrADS_ROOT}/bin/stnmap
   export GRADS=${GrADS_ROOT}/bin/grads
   export CUE2RUN=dev
elif [ $machine = ZEUS ];then
   export STNMAP=${GRADS_ROOT}/bin/stnmap
   export GRADS=${GRADS_ROOT}/bin/grads
   export CUE2RUN=batch
fi

# Copy data files to local data directory.  Untar oznstat file.  Change DATDIR definition
export DATDIRL=${stmproot}/$LOGNAME/ozone_monitor/datozn_${SUFFIX}_${string}
rm -rf $DATDIRL
mkdir -p $DATDIRL
if [[ "$SUFFIX" = "opr" ]]; then
   $NCP $DATDIR/gdas1.t${CYA}z.oznstat  $DATDIRL/oznstat.$PDATE
else
   $NCP $DATDIR/oznstat.gdas.${PDATE}  $DATDIRL/oznstat.$PDATE
fi

cd $DATDIRL
tar -xvf oznstat.$PDATE
rm -f oznstat.$PDATE

#SATYPE=`ls -l d*ges* | sed -e 's/_/ /g;s/\./ /' | awk '{ print $11 "_" $12 }'`
SATYPE=`ls -l d*ges* | sed -e 's/_/ /g;s/\./ /' | gawk '{ print $11 "_" $12 }'`
#if [ $machine = IBMP6 ]; then
#   SATYPE=`ls -l d*ges* | sed -e 's/_/ /g;s/\./ /' | gawk '{ print $10 "_" $11 }'`
#else
#   SATYPE=`ls -l d*ges* | sed -e 's/_/ /g;s/\./ /' | gawk '{ print $11 "_" $12 }'`
#fi

echo $SATYPE

for type in ${SATYPE}; do
  if [ -s ./diag_${type}_${string}.${PDATE}.gz ] ; then
    mv ./diag_${type}_${string}.${PDATE}.gz  ./diag_${type}.${PDATE}.gz
  elif [ -s ./diag_${type}_${string}.${PDATE}.Z ] ; then    # oznstat from HPSS having format *.Z rather than *.gz
    mv ./diag_${type}_${string}.${PDATE}.Z  ./diag_${type}.${PDATE}.gz
  fi
done

export DATDIR=$DATDIRL


# Export variables
export listvar=PDATE,NDATE,DATDIR,TANKDIR,webpsw,webmch,webid,WEBDIR,EXEDIR,LOGDIR,SCRIPTS,GSCRIPTS,STNMAP,GRADS,USER,SUB,SUFFIX,SATYPE,NCP,PLOT,ACCOUNT,string,bjobs,data_extract,ptmproot,stmproot,transfer_plot,machine,CUE2RUN,GROUP,listvar


#------------------------------------------------------------------
#   Clean up $tmpdir  
#
 cd $tmpdir
 cd ../
 rm -rf $tmpdir



 rm -f $LOGDIR/time.log
if [ $machine = WCOSS ]; then
   /bin/sh $SCRIPTS/time.sh
elif [ $machine = ZEUS ];then
#  $SUB -A $PROJECT -l procs=1,walltime=0:20:00 -N time_${SUFFIX}_$string -v $listvar -o $LOGDIR/time.log $SCRIPTS/time.sh
#  $SUB -A $ACCOUNT -l procs=1,walltime=0:20:00 -N time_${SUFFIX}_$string -V -j oe -o $LOGDIR/time.log $SCRIPTS/time.sh
   /bin/sh $SCRIPTS/time.sh 
fi

 rm -f $LOGDIR/horiz.log
if [ $machine = WCOSS ]; then
   /bin/sh $SCRIPTS/horiz.sh
elif [ $machine = ZEUS ];then
#  $SUB -A $ACCOUNT -l procs=1,walltime=0:20:00 -N horiz_${SUFFIX}_$string -V -j  oe -o $LOGDIR/horiz.log $SCRIPTS/horiz.sh
   /bin/sh $SCRIPTS/horiz.sh
fi


# update date only if flag is set to 1
if [[ $data_extract -eq 1  && "$string" = "ges"  ]]; then
   rm -f $LOGDIR/update.log
   if [ $machine = WCOSS ]; then
      /bin/sh $SCRIPTS/update.sh
   elif [ $machine = ZEUS ];then
#     $SUB -A $ACCOUNT -l procs=1,walltime=0:20:00 -N update_${SUFFIX}_$string  -V -j  oe -o $LOGDIR/update.log $SCRIPTS/update.sh
      /bin/sh $SCRIPTS/update.sh
   fi
fi

exit
