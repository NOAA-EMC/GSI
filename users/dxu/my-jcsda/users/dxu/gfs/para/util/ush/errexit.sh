#!/bin/sh
#
# --------------------------------------------------------
#  Script:  errexit  Author:  Bill Facey  2 Apr 96
#
# ABSTRACT:  This script is to be used when a fatal error or condition 
# has been reached and you want to terminate the job.  The script puts
# failmessages in job outputfile and in the jlogfile, a failjob is sent
# set to job output file, and to the jlogfile, a fail message is sent to
# the HDS, and processing is terminateod and the screen is turned purple.
#
# USAGE:  To use this script one must export the following variables 
# to the script: jobid, SENDMSG, ENVIR, HOLDATMP, HOLDNTMP,
# and failjob,and jlogfile.  One can provide a message for the logfile
# by passing it to the script as an argument.
#
set -x
export NWPROD=${NWPROD:-/nwprod}
[ -z "$utilscript" ] && utilscript="/nwprod/util/ush"
[ -z "$DISPMSG" ] || $DISPMSG "$failjob"
msg1=" FAILED ${jobid} - ABNORMAL EXIT"
num=$#
echo num=$num
if test "$num" -eq 1
then
msg1="$1"
fi
sh $utilscript/postmsg.sh "$jlogfile" "$msg1"
# --------------------------------------------------------
# UTILITY - error-exit script
# --------------------------------------------------------
set +x
echo "*******************************************************"
echo " ***** FAILED ${jobid} - ABNORMAL EXIT *****"
echo " ***** FAILED ${jobid} - ABNORMAL EXIT *****"
echo " ***** FAILED ${jobid} - ABNORMAL EXIT *****"
echo "*******************************************************"
set -x
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
    ########################################################
    $SMSBIN/smsmsg "***JOB ${jobid} ERROR in $pgm  RC=$err ***"
    $SMSBIN/smsmsg "***JOB ${jobid} ERROR in $pgm  RC=$err ***"
    $SMSBIN/smsmsg "***SEE OUTPUT IN ${COM} ***"
    $SMSBIN/smsabort
    ########################################################
fi

#  SAG: Changed JOB_NAME for frost and snow.  It is now the same as
#  the LOADL_STEP_ID variable.
#JOB_NAME=`echo $LOADL_STEP_ID | awk -F. '{print $1"."$5}'`
JOB_NAME=$LOADL_STEP_ID
date
llcancel $JOB_NAME
sleep 60
exit
