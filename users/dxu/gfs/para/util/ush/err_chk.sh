#!/bin/sh
#
#  Script:  err_check  Author:  Bill Facey  2 Apr 96
#
# ABSTRACT:  This script checks a return code  in $err.  If $err=0
# a message that the program completed is put in the job outputfile and
# in a log file.  If $err != 0 then fail messages are put in job 
# outputfile and in the jlogfile, a failjob is sent to the front end,
# and processing is halted and the screen is turned purple.
# 
# USAGE:  To use this script one must export the following variables
# to the script:  err, pgm, pgmout, jobid, logfile, SENDMSG, failjob, 
# and ENVIR.
# 
#
set +x
export NWPROD=${NWPROD:-/nwprod}
[ -z "$utilscript" ] && utilscript=$NWPROD/util/ush
if test "$err" -ne '0'
then
  cat < errfile
  cat < errfile >> $pgmout
  echo "*******************************************************"
  echo "********  ERROR PROGRAM $pgm RETURN CODE $err  ********"
  echo "*******************************************************"
  # --------------------------------------------------------
  msg1="ERROR PROGRAM $pgm RETURN CODE $err"
  sh $utilscript/postmsg.sh "$jlogfile" "$msg1"
  # --------------------------------------------------------
  # UTILITY - error-exit script
  # --------------------------------------------------------
  echo "*******************************************************"
  echo " ***** FAILED ${jobid} - ABNORMAL EXIT *****"
  echo " ***** FAILED ${jobid} - ABNORMAL EXIT *****"
  echo " ***** FAILED ${jobid} - ABNORMAL EXIT *****"
  echo "*******************************************************"
  msg2=" FAILED ${jobid} - ABNORMAL EXIT"
  sh $utilscript/postmsg.sh "$jlogfile" "$msg2"

  #####################################
  # list  production libraries
  #####################################
  for lib in $libdirs
  do
    cd $lib; pwd ; ls -ltr       
    cd $lib/exec* ; pwd ; ls -ltr
    cd $lib/ucl* ;  pwd ; ls -ltr
    cd $lib/fix* ;  pwd ; ls -ltr
    cd $DATA
  done

  # list temporary files
  cd $DATA;pwd;ls -ltr

  # save standard output
  cat break $pgmout break > allout
  cat allout

  cd

  if test "$SENDSMS" = "YES"
  then
   ###########################################################
   $SMSBIN/smsmsg "***JOB ${jobid} ERROR in $pgm  RC=$err ***"
   $SMSBIN/smsmsg "***JOB ${jobid} ERROR in $pgm  RC=$err ***"
   $SMSBIN/smsmsg "***SEE OUTPUT IN ${COM} ***"
   $SMSBIN/smsabort
   ###########################################################
  fi

  #
  #  If error return code is 139, then program segfaulted.
  #  Look for any core files, and save them to a directory
  #  in /ptmp
  #
  if [ $err -eq 139 ]
  then
     cd $DATA
     if [ -s core ] 
     then
        mkdir -p /ptmpp1/production.core/$pgm.$$
        cp core /ptmpp1/production.core/$pgm.$$
     fi 
     for coredir in  `ls -d coredir.*`
     do
        mkdir -p /ptmpp1/production.core/$pgm.$$/$coredir
        cp ${coredir}/* /ptmpp1/production.core/$pgm.$$/$coredir
     done
  fi

  set -x
#  SAG:  Changed JOB_NAME for frost and snow.  It is now the same as
#  the LOADL_STEP_ID variable.
#  JOB_NAME=`echo $LOADL_STEP_ID | awk -F. '{print $1"."$5}'`
  JOB_NAME=$LOADL_STEP_ID
  date
  llcancel $JOB_NAME
  sleep 60
  exit
  set +x

else
  echo " --------------------------------------------- "
  echo " ********** COMPLETED PROGRAM $pgm  **********"
  echo " --------------------------------------------- "
  msg="$pgm completed normally"
  sh $utilscript/postmsg.sh "$jlogfile" "$msg"
fi
exit
