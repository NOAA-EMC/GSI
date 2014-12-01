#!/bin/ksh
#
set +x
#  script:  setup  Author:  Bill Facey  4/3/96
#
# Abstract:  This script copies various script utiities to the
# working directory so that they may be used in our operational
# scripts.  The following utility scripts are setup: tracer,
# prep_step.sh, err_chk.sh, errexit.sh, null, and postmsg.sh.
# Before using these scripts, review them.  Err_chk_sh and
# errexit.sh are a bit different from what we are used to.
# Postmsg.sh is new.
# NOTE - The following can be  used interchangeably;
# err_chk and errchk (They are the same.)
# errexit and err_exit (They are the same.).
#
# USAGE:  Script assumes you are in the working directory when
# you use it.
# ###############################################
# NOW SETUP -HERE FILES- ROUTINES FOR PROCESSING
# ###############################################
[ -z "$utilscript" ] && utilscript="/nwprod/util/ush"
####################################################
# UTILITY - text to copy into errfile between steps
####################################################
tr="prep_step TRACEBACK:  this step should always be over_written."
echo "$tr" > tracer
 
##################################################################
# UTILITY - script to initialize files before each step/execution
##################################################################
diff $utilscript/prep_step.sh  prep_step 2>/dev/null
ret=$?
if [ $ret -ne 0 ] ; then
  cp  $utilscript/prep_step.sh  prep_step
fi
chmod u+x prep_step
 
#####################################
# UTILITY - setup error-check script
#####################################
diff $utilscript/err_chk.sh  err_chk 2>/dev/null
ret=$?
if [ $ret -ne 0 ] ; then
  cp  $utilscript/err_chk.sh  err_chk
fi
chmod u+x err_chk

diff $utilscript/err_chk.sh  errchk  2>/dev/null
ret=$?
if [ $ret -ne 0 ] ; then
  cp  $utilscript/err_chk.sh  errchk
fi
chmod u+x errchk
 
 
####################################
# UTILITY - setup error-exit script
####################################
diff $utilscript/errexit.sh  errexit 2>/dev/null
ret=$?
if [ $ret -ne 0 ] ; then
  cp  $utilscript/errexit.sh  errexit
fi
chmod u+x errexit
 
diff $utilscript/errexit.sh  err_exit 2>/dev/null
ret=$?
if [ $ret -ne 0 ] ; then
  cp  $utilscript/errexit.sh  err_exit
fi
chmod u+x err_exit
 
################################
# UTILITY - text for page break
################################
rm break 2> /dev/null
tr="step ############# break ##############################"
echo "$tr">  break
chmod u+x break
 
############################################
# UTILITY - text to establish a null command
#############################################
rm null 2> /dev/null
echo " " > null
chmod u+x null
 
#############################################
# UTILITY - text to establish postmsg command
#############################################
diff $utilscript/postmsg.sh postmsg  2>/dev/null
ret=$?
if [ $ret -ne 0 ] ; then
  cp $utilscript/postmsg.sh postmsg
fi
chmod u+x postmsg
 
#############################################
# UTILITY - text to establish startmsg command
#############################################
diff $utilscript/startmsg.sh startmsg  2>/dev/null
ret=$?
if [ $ret -ne 0 ] ; then
  cp $utilscript/startmsg.sh startmsg
fi
chmod u+x startmsg
 
#  --------------------end setup script-----------------------------
