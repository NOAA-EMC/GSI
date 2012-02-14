#!/bin/ksh

#-------------------------------------------------------------------
#
#  script:  mk_drift.sh
#
#  purpose:  Generate the drift report for obs and pen values
#            by sat/instrument, channel, and region.
#-------------------------------------------------------------------

set -ax
date
export list=$listvar

#. /u/$USER/home/radweb_global_dev/scripts/radopr_conf.sh
#. ../parm/radopr_conf.sh

#------------------------------------------------------------------
# Set environment variables.
#-------------------------------------------------------------------

#export TANKDIR=/u/wx20es/verif/test/


#-------------------------------------------------------------------
#  Create $workdir
#-------------------------------------------------------------------
#export workdir=${WORKDIR}/drift_test_${SUFFIX}
export workdir=/stmp/wx20es/drift_test_${SUFFIX}
rm -rf $workdir
mkdir -p $workdir
cd $workdir


#-------------------------------------------------------------------
#  Set dates
#      BDATE is beginning date for the 7 day range
#      EDATE is ending date for 7 day range (always use 00 cycle) 
#-------------------------------------------------------------------
EDATE=`cat $TANKDIR/cycle/prodate`
sdate=`echo $EDATE|cut -c1-8`
EDATE=${sdate}00

#BDATE=`$NDATE -360 $EDATE`  # 15 days
BDATE=`$NDATE -168 $EDATE`   #  7 days

echo EDATE = $EDATE
echo BDATE = $BDATE

drift_obs="${workdir}/drift_obs.${EDATE}.txt"
drift_pen="${workdir}/drift_pen.${EDATE}.txt"
drift_report="${workdir}/drift_report.${EDATE}.txt"
drift_hdr="${workdir}/drift_hdr.txt"
obs_hdr="${workdir}/obs_hdr.txt"
pen_hdr="${workdir}/pen_hdr.txt"

cat << EOF > ${drift_hdr}
Drift Report
   $SUFFIX
   $EDATE

(bound = 30 day avg +/- 1sdv)

EOF
cat ${REGION} >> ${drift_hdr}

cat << EOF > ${obs_hdr}

OBSERVATIONAL COUNTS                                     7 day avg       bound
============= ======                                     =========       =====
EOF

cat << EOF > ${pen_hdr}

PENALTY VALUES                                           7 day avg       bound  
======= ======                                           =========       =====
EOF


cat ${obs_hdr} >> ${drift_obs}
cat ${pen_hdr} >> ${drift_pen}

rm ${obs_hdr} ${pen_hdr}


#-------------------------------------------------------------------
#  Loop over $SATYPE and build test files for each
#-------------------------------------------------------------------
for type in ${SATYPE}; do

   #-------------------------------------------------------------------
   #  Create $tmpdir
   #-------------------------------------------------------------------
   export tmpdir=${workdir}/test_${SUFFIX}.${type}.$EDATE
   rm -rf $tmpdir
   mkdir -p $tmpdir
   cd $tmpdir

   #-------------------------------------------------------------------
   #  Create the cycle_hrs.txt file
   #-------------------------------------------------------------------
   cdate=$BDATE
   while [[ $cdate -le $EDATE ]]; do
      echo $cdate >> cycle_hrs.txt
      adate=`$NDATE +6 $cdate`
      cdate=$adate
   done


   #-------------------------------------------------------------------
   #  Copy the data files, ctl file, and base file to tmp dir
   #-------------------------------------------------------------------
   cdate=$BDATE
   while [[ $cdate -le $EDATE ]]; do
      $NCP $TANKDIR/time/${type}.${cdate}.ieee_d* ./
      if [[ -s ./${type}.${cdate}.ieee_d.Z ]]; then
        uncompress ./${type}.${cdate}.ieee_d.Z
      fi
      adate=`$NDATE +6 $cdate`
      cdate=$adate
   done

   $NCP $TANKDIR/time/${type}.ctl* ./
   if [[ -s ./${type}.ctl.Z ]]; then
      uncompress ./${type}.ctl.Z
   fi

   $NCP $TANKDIR/time/${type}.base* ./
   if [[ -s ./${type}.base.Z ]]; then
      uncompress ./${type}.base.Z
   fi
   
   #-------------------------------------------------------------------
   #  Get the number of channels for this $type
   #-------------------------------------------------------------------
   line=`cat ${type}.base | grep ${type}`
   nchan=`echo $line|nawk '{print $3}'`
   echo channels = $nchan

   #-------------------------------------------------------------------
   #  Cut out the iuse flags from the ctl file and dump them
   #  into the channel.txt file for make_base.x to access
   #-------------------------------------------------------------------
   nawk '/iuse/{print $8}' ${type}.ctl >> channel.txt


   #-------------------------------------------------------------------
   #  Copy the executable and run it 
   #------------------------------------------------------------------
   out_file=${type}.test
   $NCP $EXEDIR/make_base.${RAD_AREA}.x ./

cat << EOF > input
 &INPUT
  satname='${type}',
  n_chan=${nchan},
  nregion=5,
  nfile=29,
  date='${EDATE}',
  out_file='${out_file}',
 /
EOF

   timex make_base.${RAD_AREA}.x < input > stdout.${type}.test

   #-------------------------------------------------------------------
   #  Copy the drift.pl script and run it 
   #------------------------------------------------------------------
   $NCP $SCRIPTS/drift.pl .
   ./drift.pl ${type}

  
   #-------------------------------------------------------------------
   #  cat the drift_obs and drift_pen files to the main report
   #-------------------------------------------------------------------
   if [[ -s drift_obs.txt ]]; then
      lines=`wc -l <drift_obs.txt`
      if [[ $lines -gt 1 ]]; then
         cat drift_obs.txt >> ${drift_obs}
      fi
   fi

   if [[ -s drift_pen.txt ]]; then
      lines=`wc -l <drift_pen.txt`
      if [[ $lines -gt 1 ]]; then
         cat drift_pen.txt >> ${drift_pen}
      fi
   fi

done   # loop over SATYPE


#-------------------------------------------------------------------
#  Assemble the drift report and mail it.
#-------------------------------------------------------------------

hdr="0"
if [[ -s ${drift_obs} ]]; then
   lines=`wc -l <${drift_obs}`
   if [[ $lines -gt 3 ]]; then
      hdr="1"
      cat ${drift_hdr} >> ${drift_report}
      cat ${drift_obs} >> ${drift_report}
   fi
fi

if [[ -s ${drift_pen} ]]; then
   lines=`wc -l <${drift_pen}`
   if [[ $lines -gt 3 ]]; then
      if [[ $hdr == "0" ]]; then
         cat ${drift_hdr} >> ${drift_report}
      fi
      cat ${drift_pen} >> ${drift_report}
   fi
fi


echo MAIL_TO = ${MAIL_TO}
if [[ -s ${drift_report} ]]; then
   lines=`wc -l <${drift_report}`
   if [[ $lines -gt 6 ]]; then
      cat ${DISCLAIMER} >> ${drift_report}
      if [[ $MAIL_CC == "" ]]; then
         /bin/mail -v -s drift_report ${MAIL_TO}< ${drift_report}     
      else
         /bin/mail -v -s drift_report -c "${MAIL_CC}" ${MAIL_TO}< ${drift_report}     
      fi
   fi
fi


#-------------------------------------------------------------------
#  Clean up
#-------------------------------------------------------------------
cd $workdir
cd ..
rm -rf $workdir

