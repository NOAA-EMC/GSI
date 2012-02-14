#!/bin/ksh

####################################################################
#
#  verf_time.sh
#
#  Extract time related data from the radiance diagnostic files and
#  store in binary files.
#
####################################################################

set -ax
date

#-------------------------------------------------------------------
# Set environment variables.
#-------------------------------------------------------------------
export list=$listvar
disclaimer=${DISCLAIMER}
report=report.txt

diag_report=diag_report.txt
diag_hdr=diag_hdr.txt
diag=diag.txt

obs_err=obs_err.txt
obs_hdr=obs_hdr.txt
pen_err=pen_err.txt
pen_hdr=pen_hdr.txt
chan_err=chan_err.txt
chan_hdr=chan_hdr.txt

bad_chan=bad_chan.${PDATE}

export DO_DIAG_RPT=${DO_DIAG_RPT:-0}
export DO_DATA_RPT=${DO_DATA_RPT:-0}
export DO_DRIFT_RPT=${DO_DRIFT_RPT:-0}

echo DO_DIAG_RPT = $DO_DIAG_RPT
echo DO_DATA_RPT = $DO_DATA_RPT
echo DO_DRIFT_RPT = $DO_DRIFT_RPT

if [[ $USE_ANL -eq 1 ]]; then
   gesanl='ges anl'
else
   gesanl=ges
fi


#if [[ $SUFFIX = "oex" ]]; then
#    SATYPE="sndrd1_g11"
#fi

#--------------------------------------------------------------------
#   Make temp dir, rename tank directory and create if necessary
#-------------------------------------------------------------------
tmpdir=${WORKDIR}/time_${SUFFIX}.$PDATE
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

if [ ! -d $TANKDIR ]; then
   mkdir -p $TANKDIR
fi

orig_TANKDIR=$TANKDIR
TANKDIR=$TANKDIR/time
if [ ! -d $TANKDIR ]; then
   mkdir -p $TANKDIR
fi


#--------------------------------------------------------------------
#   Copy extraction program and data to working directory
#-------------------------------------------------------------------

if [[ $SUFFIX = "oex" ]]; then
   $NCP $EXEDIR/time.oex.x ./time.x
elif [[ $SUFFIX = "xrn" ]]; then
   $NCP $EXEDIR/time.xrn.x ./time.x
else
   $NCP $EXEDIR/time.${RAD_AREA}.x   ./time.x
fi

RADDIR=$DATDIR
for type in ${SATYPE}; do

   if [ -s $RADDIR/diag_${type}_ges.${PDATE}.Z ] ; then
      $NCP $RADDIR/diag_${type}_ges.${PDATE}.Z  ./${type}.Z
      uncompress ./${type}.Z
   fi

   if [[ $USE_ANL -eq 1 ]]; then
      $NCP $RADDIR/diag_${type}_anl.${PDATE}.Z  ./${type}_anl.Z
      uncompress ./${type}_anl.Z
   fi

done

iyy=`echo $PDATE | cut -c1-4`
imm=`echo $PDATE | cut -c5-6`
idd=`echo $PDATE | cut -c7-8`
ihh=`echo $PDATE | cut -c9-10`
cyc=$ihh
export CYCLE=$cyc


#--------------------------------------------------------------------
#   Loop over each entry in SATYPE
#--------------------------------------------------------------------
for type in ${SATYPE}; do

   if [[ ${DO_DATA_RPT} -eq 1 || ${DO_DRIFT_RPT} -eq 1 ]]; then 
      $NCP $TANKDIR/${type}.base ./
   fi

   for dtype in ${gesanl}; do
      rm input

      if [[ $dtype == "anl" ]]; then
         data_file=${type}.${PDATE}_anl.ieee_d
         ctl_file=${type}_anl.ctl
         stdout_file=stdout.${type}_anl
      else
         data_file=${type}.${PDATE}.ieee_d
         ctl_file=${type}.ctl
         stdout_file=stdout.${type}
      fi

#--------------------------------------------------------------------
#   Run program for given satellite/instrument
#--------------------------------------------------------------------
      nchanl=-999
cat << EOF > input
 &INPUT
  satname='${type}',
  iyy=${iyy},
  imm=${imm},
  idd=${idd},
  ihh=${ihh},
  idhh=-720
  incr=6
  nchanl=${nchanl},
  suffix='${SUFFIX}',
  imkctl=${MAKE_CTL},
  imkdata=${MAKE_DATA},
  gesanl='${dtype}',
 /
EOF
     timex $tmpdir/time.x < input >   ${stdout_file}

#-------------------------------------------------------------------
#  copy data file back to $TANKDIR and compress
#  copy stdout file (no compression)
#-------------------------------------------------------------------

      if [[ -s ${data_file} ]]; then
         $NCP ${data_file} $TANKDIR
         compress -f $TANKDIR/${data_file}
      fi

      $NCP ${stdout_file}             $TANKDIR

      if [[ -s ${ctl_file} ]]; then
         $NCP ${ctl_file} ${TANKDIR}
         compress -f ${TANKDIR}/${ctl_file}
      fi

      if [[ -s ${bad_chan} ]]; then
         cat ${bad_chan}
      fi
   done
done




#-------------------------------------------------------------------
#-------------------------------------------------------------------
#  Begin error analysis and reporting
#-------------------------------------------------------------------
#-------------------------------------------------------------------

echo DO_DIAG_RPT = $DO_DIAG_RPT
if [[ $DO_DIAG_RPT -eq 1 ]]; then

#-------------------------------------------------------------------
#  Check stdout file for any reported problem(s) reading the 
#  diagnostic file by calling ck_stdout.sh
#
   ${SCRIPTS}/ck_stdout.sh  ${diag}

   if [[ -s ${diag} ]]; then
      cat << EOF > ${diag_hdr}
Problem Reading Diagnostic File
     $SUFFIX 
     $PDATE

  Problems were encountered reading the diagnostic file for
  the following sources:

EOF

      cat ${diag_hdr} >> ${diag_report}
      cat ${diag} >> ${diag_report}
      cat ${disclaimer} >> ${diag_report}

      rm ${diag} ${diag_hdr}
   fi 

#-------------------------------------------------------------------
#  mail error notifications

   if [[ -s ${diag_report} ]]; then
      lines=`wc -l <${diag_report}`
      if [[ $lines -gt 1 ]]; then
         if [[ $MAIL_CC == "" ]]; then
            /bin/mail -v -s diagnostic_error_report ${MAIL_TO}< ${diag_report}
         else
            /bin/mail -v -s diagnostic_error_report -c "${MAIL_CC}" ${MAIL_TO}< ${diag_report}
         fi
      fi
   fi

fi


#-------------------------------------------------------------------
#  define bad_pen and bad_obs files


if [[ $DO_DATA_RPT -eq 1 ]]; then
   bad_pen=bad_pen.${PDATE}
#  bad_obs=bad_obs.${PDATE}
   bad_chan=bad_chan.${PDATE}

   qdate=`ndate -06 $PDATE`
   pcyc=`echo $qdate | cut -c9-10`
   export PREV_CYCLE=$pcyc

   #prev_bad_obs=bad_obs.${qdate}
   prev_bad_pen=bad_pen.${qdate}
   prev_bad_chan=bad_chan.${qdate}

#-------------------------------------------------------------------
#  Use these assignments ONLY for testing purposes with parallel:
#
#  prev_bad_obs=$bad_obs
#  prev_bad_pen=$bad_pen

#-------------------------------------------------------------------
#  copy previous cycle's bad_obs and bad_pen files

#  $NCP ${TANKDIR}/${prev_bad_obs} ./
   $NCP ${TANKDIR}/${prev_bad_pen} ./
   $NCP ${TANKDIR}/${prev_bad_chan} ./

#-------------------------------------------------------------------
#  run mk_err_rpt for obs and pen to create the error files

#  ${SCRIPTS}/mk_err_rpt.sh ${prev_bad_obs} ${bad_obs} obs ${qdate} ${PDATE} ${obs_err}
   ${SCRIPTS}/mk_err_rpt.sh ${prev_bad_pen} ${bad_pen} pen ${qdate} ${PDATE} ${tmpdir}/${diag_report} ${pen_err}
   ${SCRIPTS}/mk_err_rpt.sh ${prev_bad_chan} ${bad_chan} chan ${qdate} ${PDATE} ${tmpdir}/${diag_report} ${chan_err}


#-------------------------------------------------------------------
#  put together the unified error report with both obs and
#  penalty problems and mail it

echo BEFORE ERROR REPORTING

   if [[ -s ${obs_err} || -s ${pen_err} || -s ${chan_err} ]]; then

      echo DOING ERROR REPORTING

      cat << EOF > $report

Cycle Data Integrity Report 
  $SUFFIX 
  $PDATE

EOF

      cat ${REGION} >> $report

      if [[ -s ${chan_err} ]]; then

         echo OUTPUTING CHAN_ERR

         cat << EOF > ${chan_hdr}
         
  The following channels report 0 observational counts over the past two cycles:
   
  Satellite/Instrument    Channel
  ====================    =======

EOF

         cat ${chan_hdr} >> $report
         cat ${chan_err} >> $report
 
      fi


      if [[ -s ${obs_err} ]]; then

         cat << EOF > ${obs_hdr}

  Observational counts outside of the established normal range were found
  for these sensor/channel/regions in the past two cycles:

  Questionable Observational Counts 
  ============ ============= ======  

EOF

         cat ${obs_hdr} >> $report
         cat ${obs_err} >> $report
         rm ${obs_hdr} ${obs_err}
      fi 



      if [[ -s ${pen_err} ]]; then

         cat << EOF > ${pen_hdr}


  Penalty values outside of the established normal range were found
  for these sensor/channel/regions in the past two cycles: 

  Questionable Penalty Values 
  ============ ======= ======      Cycle                 Penalty          Bound
                                   -----                 -------          -----
EOF
         cat ${pen_hdr} >> $report
         cat ${pen_err} >> $report

         rm ${pen_hdr} ${pen_err}
      fi 

      cat ${disclaimer} >> $report
   fi

#-------------------------------------------------------------------
#  mail error notifications
#
   if [[ -s ${report} ]]; then
      lines=`wc -l <${report}`
      if [[ $lines -gt 2 ]]; then
         if [[ $MAIL_CC == "" ]]; then
            /bin/mail -v -s cycle_report ${MAIL_TO}< ${report}
         else
            /bin/mail -v -s cycle_report -c "${MAIL_CC}" ${MAIL_TO}< ${report}
         fi 
      fi
   fi



#-------------------------------------------------------------------
#  copy new bad_pen and bad_obs files to $TANKDIR

   if [[ -s ${bad_chan} ]]; then
      echo bad_chan located, $bad_chan
   else
      echo no $bad_chan 
   fi

   $NCP ${bad_pen} ${TANKDIR}/.
#  $NCP ${bad_obs} ${TANKDIR}/.
   $NCP ${bad_chan} ${TANKDIR}/.


   find ${TANKDIR} -name "bad_pen*" -mtime +31 -exec rm {} \;
   find ${TANKDIR} -name "bad_obs*" -mtime +31 -exec rm {} \;
   find ${TANKDIR} -name "bad_chan*" -mtime +31 -exec rm {} \;

fi


#-------------------------------------------------------------------
#  Drift check
#

if [[ ${DO_DIAG_RPT} -eq 1 && ${CYCLE} = "00" ]]; then
   export TANKDIR=${orig_TANKDIR}
   export listvar=PDATE,NDATE,DATDIR,TANKDIR,EXEDIR,LOGDIR,SCRIPTS,USER_CLASS,SUB,SUFFIX,SATYPE,NCP,ACOUNT,MAIL_TO,MAIL_CC,DISCLAIMER,REGION,WORKDIR,RAD_AREA,listvar
   ${SCRIPTS}/mk_drift.sh 1>${LOGDIR}/mk_drift.log 2>${LOGDIR}/mk_drift.err
fi


#-------------------------------------------------------------------
#-------------------------------------------------------------------
#  end error reporting section
#-------------------------------------------------------------------
#-------------------------------------------------------------------


#-------------------------------------------------------------------
#   Remove data old files in $TANKDIR
#-------------------------------------------------------------------

find ${TANKDIR} -name "*.ieee_d*" -mtime +91 -exec rm {} \;


#-------------------------------------------------------------------
#   Clean up $tmpdir and $WORKDIR if this is the last verf job.
#

echo cleaning up $tmpdir
cd $tmpdir
cd ../
rm -rf $tmpdir

echo cleaning up $WORKDIR
count=`ls ${LOADLQ}/verf*_${SUFFIX}* | wc -l`
complete=`grep "COMPLETED" ${LOADLQ}/verf*_$SUFFIX* | wc -l`

total=`expr $count - $complete`

if [[ $total -le 1 ]]; then
   cd $WORKDIR
   cd ../
   rm -rf $WORKDIR
fi

exit
