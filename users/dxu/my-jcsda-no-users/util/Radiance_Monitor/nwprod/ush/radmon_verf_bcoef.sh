#!/bin/ksh

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         radmon_verf_bcoef.sh
# Script description:  Extract bias correction coefficients data from radiance 
#                      diagnostic files.
#
# Author:        Ed  Safford       Org: NP23         Date: 2012-02-02
#
# Abstract:  This script extracts bias correction coefficient related data from 
#            radiance diagnostic files (which are an output from GSI runs), 
#            storing the extracted data in small binary files.
#
#            This script is a child script of exgdas_vrfyrad.sh.sms.  The parent
#            script opens and uncompresses the radiance diagnostic file and copies
#            other supporting files into a temporary working directory.
#
#
# Script history log:
# 2012-02-02  Safford  initial script
#
# Usage:  radmon_verf_bcoef.sh PDATE
#
#   Input script positional parameters:
#     PDATE             processing date
#                       yyyymmddcc format; required
#
#   Imported Shell Variables:
#     SUFFIX            data source suffix
#                       defauls to opr
#     MAKE_CTL          switch to construct the meta-data control file
#                       defaults to 1 (on)
#     MAKE_DATA         switch to construct the binary data file
#                       defaults to 1 (on)
#     EXECgfs           executable directory
#                       defaults to current directory
#     FIXgfs            fixed data directory
#                       defaults to current directory
#     RAD_AREA          global or regional flag
#                       defaults to global
#     TANKverf_rad      data repository
#                       defaults to current directory
#     SATYPE            list of satellite/instrument sources
#                       defaults to none
#     INISCRIPT         preprocessing script
#                       defaults to none
#     LOGSCRIPT         log script
#                       defaults to none
#     ERRSCRIPT         error processing script
#                       defaults to 'eval [[ $err = 0 ]]'
#     ENDSCRIPT         postprocessing script
#                       defaults to none
#     VERBOSE           Verbose flag (YES or NO)
#                       defaults to NO
#     LITTLE_ENDIAN     flag for LE machine
#                       defaults to 0 (big endian)
#
#   Exported Shell Variables:
#     err           Last return code
#
#   Modules and files referenced:
#     scripts    : $INISCRIPT
#                  $LOGSCRIPT
#                  $ERRSCRIPT
#                  $ENDSCRIPT
#
#     programs   : $NCP
#                  $bcoef_exec
#
#     fixed data : $biascr
#
#     input data : $data_file
#
#     output data: $bcoef_file
#                  $bcoef_ctl
#                  $bcoef_stdout
#
# Remarks:
#
#   Condition codes
#      0 - no problem encountered
#     >0 - some problem encountered
#
#  Control variable resolution priority
#    1 Command line argument.
#    2 Environment variable.
#    3 Inline default.
#
# Attributes:
#   Language: POSIX shell
#   Machine: IBM SP
####################################################################
#  Command line arguments.
export PDATE=${1:-${PDATE:?}}

# Directories
FIXgfs=${FIXgfs:-$(pwd)}
EXECgfs=${EXECgfs:-$(pwd)}
TANKverf_rad=${TANKverf_rad:-$(pwd)}

# File names
INISCRIPT=${INISCRIPT:-}
LOGSCRIPT=${LOGSCRIPT:-}
ERRSCRIPT=${ERRSCRIPT:-}
ENDSCRIPT=${ENDSCRIPT:-}

# Other variables
MAKE_CTL=${MAKE_CTL:-1}
MAKE_DATA=${MAKE_DATA:-1}
RAD_AREA=${RAD_AREA:-glb}
SATYPE=${SATYPE:-}
VERBOSE=${VERBOSE:-NO}
LITTLE_ENDIAN=${LITTLE_ENDIAN:-0}
USE_ANL=${USE_ANL:-0}
err=0
#bcoef_exec=radmon_bcoef.${RAD_AREA}
bcoef_exec=radmon_bcoef

if [[ "$VERBOSE" = "YES" ]]; then
   set -ax
   echo "$(date) executing $0 $* >&2"
fi
################################################################################
#  Preprocessing
$INISCRIPT
$LOGSCRIPT

if [[ $USE_ANL -eq 1 ]]; then
   gesanl="ges anl"
else
   gesanl="ges"
fi

#--------------------------------------------------------------------
#   Copy extraction program and supporting files to working directory

$NCP $EXECgfs/${bcoef_exec}              ./${bcoef_exec}
$NCP ${biascr}                              ./biascr.txt

if [[ ! -s ./${bcoef_exec} || ! -s ./biascr.txt ]]; then
   err=4
else


#--------------------------------------------------------------------
#   Run program for given time

   iyy=`echo $PDATE | cut -c1-4`
   imm=`echo $PDATE | cut -c5-6`
   idd=`echo $PDATE | cut -c7-8`
   ihh=`echo $PDATE | cut -c9-10`

   ctr=0
   fail=0

   for type in ${SATYPE}; do
      for dtype in ${gesanl}; do
      ctr=`expr $ctr + 1`

         if [[ $dtype == "anl" ]]; then
            data_file=${type}_anl.${PDATE}.ieee_d
            bcoef_file=bcoef.${data_file}
            ctl_file=${type}_anl.ctl
            bcoef_ctl=bcoef.${ctl_file}
            stdout_file=stdout.${type}_anl
            bcoef_stdout=bcoef.${stdout_file}
         else
            data_file=${type}.${PDATE}.ieee_d
            bcoef_file=bcoef.${data_file}
            ctl_file=${type}.ctl
            bcoef_ctl=bcoef.${ctl_file}
            stdout_file=stdout.${type}
            bcoef_stdout=bcoef.${stdout_file}
         fi 

      rm input

      nchanl=-999
      npredr=5

cat << EOF > input
 &INPUT
  satname='${type}',
  npredr=${npredr},
  nchanl=${nchanl},
  iyy=${iyy},
  imm=${imm},
  idd=${idd},
  ihh=${ihh},
  idhh=-720,
  incr=6,
  suffix='${SUFFIX}',
  imkctl=${MAKE_CTL},
  imkdata=${MAKE_DATA},
  gesanl='${dtype}',
  little_endian=${LITTLE_ENDIAN},
 /
EOF
      $TIMEX ./${bcoef_exec} < input >   ${stdout_file}
      if [[ $? -ne 0 ]]; then
          fail=`expr $fail + 1`
      fi


#-------------------------------------------------------------------
#  move data, control, and stdout files to $TANKverf_rad and compress
#

      if [[ -s ${data_file} ]]; then
         mv ${data_file} ${bcoef_file}
         mv ${bcoef_file} $TANKverf_rad/.
         ${COMPRESS} -f $TANKverf_rad/${bcoef_file}
      fi

      if [[ -s ${ctl_file} ]]; then
         mv ${ctl_file} ${bcoef_ctl}
         mv ${bcoef_ctl}  ${TANKverf_rad}/.
         ${COMPRESS} -f ${TANKverf_rad}/${bcoef_ctl}
      fi

      if [[ -s ${stdout_file} ]]; then
         mv ${stdout_file} ${bcoef_stdout}
         mv ${bcoef_stdout}  ${TANKverf_rad}/.
         ${COMPRESS} -f ${TANKverf_rad}/${bcoef_stdout}
      fi

      done  # dtype in $gesanl loop
   done     # type in $SATYPE loop

   if [[ $fail -eq $ctr || $fail -gt $ctr ]]; then
      err=5
   fi
fi


################################################################################
#  Post processing
$ENDSCRIPT
set +x

if [[ "$VERBOSE" = "YES" ]]; then
   echo $(date) EXITING $0 with error code ${err} >&2
fi

exit ${err}
