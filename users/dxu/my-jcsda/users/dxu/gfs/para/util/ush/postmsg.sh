#!/bin/sh
#
#  script:  postmsg   author:  Bill Facey  3/14/96
#
#  Abstract:  This script posts messages to a log file.  Two arguments
#  is returned if script cannot find only 2 arguments.  Script assumes 
#  that the variable JOBID has been exported by the parent shell.  The
#  variable JOBID contains the name of the job and its pid in the form
#  JOBNAME.pid.
#
set +x
jlogfile="$1"
msg="$2"
num=$#
if test num -eq 2
then
   #
   # Test for the existence of jlogfile variable
   #
   [ -z "$jlogfile" ] && jlogfile=/com/logs/jlogfile
   #
   # Set the date string
   #
   getdate=`date -u '+%m/%d %H:%M:%S'`
   unset msgline
   msgline="Z ${jobid}-$msg"
   echo "${getdate}${msgline}" >> $jlogfile
else
   echo "This script requires 2 arguments; you passed $num."
   exit 16
fi
exit
